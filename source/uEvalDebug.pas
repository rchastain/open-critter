
unit uEvalDebug;

interface

uses
  uBoard, uEval, uMaterial, uPiece, uScore;

{$include platform.inc}

type
  TEvalFunc = procedure of object;
  TEvalDump = object(TEvaluator)
    function base_mtrl(side: TSide): TScorePair;
    procedure print_eval(); overload;
    procedure print_eval(pt: TPieceType; wf, bf: TEvalFunc;
      var wt, bt: TScorePair); overload;
  end;
  PEvalDump = ^TEvalDump;

implementation

uses
  uBitboard, uEvalData, uPsq, uSquare, uSystem;

procedure TEvalDump.print_eval(pt: TPieceType; wf, bf: TEvalFunc; var wt,
  bt: TScorePair);
var
  ws, bs: TScorePair;
  p: TPiece;
begin
  total := 0; wf(); ws := total;
  p := piece(pt, WHITE);
  inc(ws, get_psq_score(p, board^.bb.piece[p]));

  total := 0; bf(); bs := total;
  p := piece(pt, BLACK);
  inc(bs, get_psq_score(p, board^.bb.piece[p]));

  print_score(mi^.phase, PieceNames[pt], ws, bs);

  inc(wt, ws);
  inc(bt, bs);
end;

function TEvalDump.base_mtrl(side: TSide): TScorePair;
var
  mg, eg: TScore;
begin
  with mtrl_values[STAGE_OPENING], board^.count do
    mg := value_q * queen[side]
        + value_r * rook[side]
        + value_b * bishop[side]
        + value_n * knight[side]
        + value_p * pawn[side];

  with mtrl_values[STAGE_ENDGAME], board^.count do
    eg := value_q * queen[side]
        + value_r * rook[side]
        + value_b * bishop[side]
        + value_n * knight[side]
        + value_p * pawn[side];

  result := pair(mg, eg) * sign[side];
end;

procedure TEvalDump.print_eval();
var
  wtotal, btotal, ws, bs: TScorePair;
  mbonus: TScore;
  phmisc, drawishness: TScore;
  phase: TMtrlStage;
  dec_phase: integer;
  sc: TScales;
  s: TScale;
  h: TBitboard;
  c: TScoreString;
begin

  mi := get_mtrl_info(board^, mtrl_table^);
  phase := mi^.phase;
  sc[WHITE] := mi^.scale[WHITE];
  sc[BLACK] := mi^.scale[BLACK];

  wtotal := base_mtrl(WHITE);
  btotal := base_mtrl(BLACK);

  printf('             | Score | White     Mg     Eg | Black     Mg     Eg' + LineEnding);
  printf('-------------+-------+---------------------+--------------------' + LineEnding);
  print_score(phase, 'material', wtotal, btotal);

  mbonus := mi^.mtrl_score - scale(phase, wtotal + btotal);

  new_eval();
  get_pawn_info();

  ph^.pawn_score := 0;
  pawn_struct_w();
  ws := ph^.pawn_score;

  ph^.pawn_score := 0;
  pawn_struct_b();
  bs := ph^.pawn_score;

  fillchar(ph^, sizeof(TPawnHashEntry), 0);
  get_pawn_info();

  phmisc := ph^.pawn_score - (ws + bs);
  inc(ws, get_psq_score(W_PAWN, board^.bb.w_pawns));
  inc(bs, get_psq_score(B_PAWN, board^.bb.b_pawns));

  total := 0; eval_pawns_w(); inc(ws, total);
  total := 0; eval_pawns_b(); inc(bs, total);

  print_score(phase, 'pawn', ws, bs);

  inc(wtotal, ws);
  inc(btotal, bs);

  with board^ do begin
    mob_mask[WHITE] := not (bb.white or atk.bp) or (bb.black xor bb.b_pawns);
    mob_mask[BLACK] := not (bb.black or atk.wp) or (bb.white xor bb.w_pawns);
  end;

  print_eval(KNIGHT, @eval_knights_w, @eval_knights_b, wtotal, btotal);
  print_eval(BISHOP, @eval_bishops_w, @eval_bishops_b, wtotal, btotal);

  atk.white := atk.white or atk.wn or atk.wb;
  atk.black := atk.black or atk.bn or atk.bb;

  print_eval(ROOK, @eval_rooks_w, @eval_rooks_b, wtotal, btotal);

  atk.white := atk.white or atk.wr;
  atk.black := atk.black or atk.br;

  print_eval(QUEEN, @eval_queens_w, @eval_queens_b, wtotal, btotal);

  atk.white := atk.white or atk.wq or atk.wk;
  atk.black := atk.black or atk.bq or atk.bk;

  print_eval(KING, @eval_king_w, @eval_king_b, wtotal, btotal);

  total := 0; eval_passed_pawns_w(); ws := total;
  total := 0; eval_passed_pawns_b(); bs := total;

  print_score(phase, 'passed pawns', ws, bs);

  inc(wtotal, ws);
  inc(btotal, bs);

  total := 0; eval_development_w(mi^.space_weight); ws := total;
  total := 0; eval_development_b(mi^.space_weight); bs := total;

  print_score(phase, 'development', ws, bs);

  inc(wtotal, ws);
  inc(btotal, bs);

  ws := 0; bs := 0;

  h := hang_bb[WHITE];
  if h <> 0 then
    if h and (h - 1) <> 0 then
      dec(bs, MULTI_ATTACK);

  h := hang_bb[BLACK];
  if h <> 0 then
    if h and (h - 1) <> 0 then
      inc(ws, MULTI_ATTACK);

  if (ws <> 0) or (bs <> 0) then
    print_score(phase, 'hanging pcs', ws, bs);

  inc(wtotal, ws);
  inc(btotal, bs);

  if phmisc <> 0 then
    print_score(phase, 'kings+pawns', phmisc, 0);

  drawishness := 0;
  total := scale(phase, wtotal + btotal) + mbonus;

  if total > 0 then
    dec(drawishness, (ph^.draw_weight[WHITE] * min(total, 256)) div 64)
  else
    inc(drawishness, (ph^.draw_weight[BLACK] * min(-total, 256)) div 64);

  printf('-------------+-------+---------------------+--------------------' + LineEnding);
  print_score(phase, 'total', wtotal, btotal);

  if mbonus <> 0 then
    printf('  mtrl bonus |%6s |' + LineEnding, score_to_string(mbonus, c));

  if drawishness <> 0 then
    printf(' drawishness |%6s |' + LineEnding, score_to_string(drawishness, c));

  inc(total, drawishness);
  dec_phase := phase * 100 div STAGE_OPENING;

  printf(' final score |%6s | (Mg: %3d%% , Eg: %3d%%)' + LineEnding,
    score_to_string(total, c), dec_phase, 100 - dec_phase);

  if total > 0 then s := sc[WHITE] else s := sc[BLACK];
  total := total * s div SCALE_NORMAL;

  if (sc[WHITE] <> SCALE_NORMAL) or (sc[BLACK] <> SCALE_NORMAL) then begin
    sc[WHITE] := sc[WHITE] * 100 div SCALE_NORMAL;
    sc[BLACK] := sc[BLACK] * 100 div SCALE_NORMAL;
    printf('scaled score |%6s | (WS: %3d%% , BS: %3d%%)' + LineEnding,
      score_to_string(total, c), sc[WHITE], sc[BLACK]);
  end;
  printf(LineEnding);
end;

end.
