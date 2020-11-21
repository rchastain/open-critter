
unit uEval;

interface

uses
  uBitboard, uBoard, uMaterial, uMove, uPiece, uScore, uSquare, uSystem,
  uZobrist;

{$include platform.inc}

const
  PHASH_SIZE    = 65536;
  ECACHE_SIZE   = 32768;
  EC_SCORE_MASK = $ffff;
  EC_KEY_MASK   = uint64(not EC_SCORE_MASK);
  LAZY_PSNL     = $180;
  LAZY_MULT     = 45;
  TEMPO         = 13;

type
  PPawnHashEntry = ^TPawnHashEntry;
  TPawnHashEntry = packed record
    key: TKey;
    pawn_score: TScorePair;
    king_exposure : array [TSide] of smallint;
    pawn_files    : array [TSide] of byte;
    light_block   : array [TSide] of byte;
    dark_block    : array [TSide] of byte;
    passer_mask   : array [TSide] of byte;
    draw_weight   : array [TSide] of byte;
    alignment     : array [0..5] of byte;
  end;
  TPawnHash = array [0..PHASH_SIZE-1] of TPawnHashEntry;
  PPawnHash = ^TPawnHash;

  TEvalCacheEntry = uint64;
  TEvalCache = array [0..ECACHE_SIZE-1] of TEvalCacheEntry;
  PEvalCache = ^TEvalCache;

  TEvalStats = record
    phash_probes, phash_hits: uint64;
    ecache_probes, ecache_hits: uint64;
    lazy_evals, eg_recog: uint64;
  end;

  TAttackMap = record
    case integer of
      0: (white, black, wp, bp, wk, bk, wn, bn, wb, bb, wr, br, wq, bq: TBitboard);
      1: (all, pawn, king, knight, bishop, rook, queen: array [TSide] of TBitboard);
      2: (side: array [TSide] of TBitboard);
  end;

  PEvaluator = ^TEvaluator;
  TEvaluator = object
    board: PBoard;
    st: PStateInfo;
    mi: PMtrlInfo;

    total: TScorePair;
    atk: TAttackMap;
    ph: PPawnHashEntry;

    q_attack     : array [TSide] of boolean;
    k_att_cnt    : array [TSide] of integer;
    k_att_weight : array [TSide] of integer;
    undeveloped  : array [TSide] of integer;
    min_kp_dist  : array [TSide] of integer;
    king_ring    : array [TSide] of TBitboard;
    hang_bb      : array [TSide] of TBitboard;
    mob_mask     : array [TSide] of TBitboard;
    pawn_safe    : array [TSide] of TBitboard;
    nbr_attacks  : array [TSide] of TBitboard;

    mtrl_table: PMtrlTable;
    eval_cache: PEvalCache;
    pawn_hash: PPawnHash;

    stats: TEvalStats;

    procedure evaluate(var b: TBoard; alpha: TScore = 0; beta: TScore = 0;
     lazy_margin: TScore = 0);
    procedure clear_cache();
    procedure clear_stats();
    procedure cache_init();
    procedure cache_destroy();

{$ifdef HasPrefetch}
    procedure prefetch(state: PStateInfo); inline;
{$endif}

    procedure uncache(key: TKey);
    procedure new_eval();
    procedure get_pawn_info();
    procedure pawn_struct_w();
    procedure pawn_struct_b();
    procedure eval_pawns_w();
    procedure eval_pawns_b();
    procedure eval_knights_w();
    procedure eval_knights_b();
    procedure eval_bishops_w();
    procedure eval_bishops_b();
    procedure eval_rooks_w();
    procedure eval_rooks_b();
    procedure eval_queens_w();
    procedure eval_queens_b();
    procedure eval_king_w();
    procedure eval_king_b();
    procedure eval_development_w(space_weight: integer);
    procedure eval_development_b(space_weight: integer);
    procedure eval_passed_pawns_w();
    procedure eval_passed_pawns_b();
    function has_legal_moves(): boolean;

{$ifdef LOG_EVAL}
    procedure log(score: TScore; src: PAnsiChar);
{$endif}

  private
    raw_ecache, raw_phash, raw_mcache: pointer;
    function king_exposure_w(sq: TSquare): TScore;
    function king_exposure_b(sq: TSquare): TScore;
  end;

  TEgFunction = function (side: TSide; out score: TScore): boolean of object;

  TEgFnEntry = record
    fn: pointer;
    side: TSide;
  end;

var
  evaluator: PEvaluator;
  SafetyMult: array [0..15] of integer;
  root_stm: TSide;

implementation

uses
  uEndgame, uEvalData, uHistory, uPsq, uUtil, uFen;

const
  files_west: array [TFile] of byte = ( $00, $01, $03, $07, $0f, $1f, $3f, $7f );
  files_east: array [TFile] of byte = ( $fe, $fc, $f8, $f0, $e0, $c0, $80, $00 );

var
  eg_fn_tab: array [TEgFnID] of TEgFnEntry;

{$ifdef LOG_EVAL}
procedure TEvaluator.log(score: TScore; src: PAnsiChar);
var
  f: TextFile;
  fen: TFenString;
  c: TScoreString;
  line: array [0..255] of AnsiChar;
begin
  AssignFile(f, 'eval.log'); {$I-} Append(f); {$I+}
  if IOResult <> 0 then Rewrite(f);
  sprintf(line, '%s ; score = %d (%s) [%s] flags=%4x', encode_fen(board^, fen),
    score, score_to_string(score, c), src, board^.st^.flags);
  writeln(f, line);
  CloseFile(f);
end;
{$endif}

procedure TEvaluator.evaluate(var b: TBoard; alpha: TScore = 0; beta: TScore = 0;
  lazy_margin: TScore = 0);
var
  prev_state: PStateInfo;
  ec: ^TEvalCacheEntry;
  move: TMove;
  result, psnl: TScore;
  approx_psnl, lm, gain: TScore;
  psq_delta: TScorePair;
  egfn: TMethod;
  h: TBitboard;
  sc: TScales;
  s: TScale;
  label finalize, lazy_exit;

begin
  if b.st^.flags and FLAG_REPETITION <> 0 then begin
    {$ifdef LOG_EVAL}
      board := @b;
      log(b.st^.eval, 'DRAW');
    {$endif}
    exit;
  end;

  mi := get_mtrl_info(b, mtrl_table^);
  board := @b;
  st := b.st;
  st^.flags := st^.flags or mi^.flags;
  st^.phase.total := mi^.phase;
  st^.tempo := TEMPO;
  st^.lazy_count := 0;
  st^.threat[WHITE] := 0;
  st^.threat[BLACK] := 0;
  prev_state := st^.prev;

  sc[WHITE] := mi^.scale[WHITE];
  sc[BLACK] := mi^.scale[BLACK];

  ec := @eval_cache^[cardinal(st^.hashkey) and (ECACHE_SIZE - 1)];

  if st^.flags and FLAG_SKIP_EVAL <> 0 then begin
    with eg_fn_tab[TEgFnID(mi^.eg_fn_id)] do begin
      egfn.code := fn;
      egfn.data := @self;
      TEgFunction(egfn)(side, result);
    end;
    inc(stats.eg_recog);
    if st^.flags and FLAG_NO_UPDATE = 0 then b.update_state();
    {$ifdef LOG_EVAL}
      log(result, 'EG');
    {$endif}
    goto finalize;
  end;

  inc(stats.ecache_probes);
  if (ec^ xor st^.hashkey) and EC_KEY_MASK = 0 then begin
    inc(stats.ecache_hits);
    result := smallint(ec^ and EC_SCORE_MASK);
    {$ifdef LOG_EVAL}
      log(result, 'CACHE');
    {$endif}
    b.update_state();
    goto finalize;
  end;

  if (lazy_margin <> 0) and (mi^.flags and FLAG_AVOID_LAZY = 0) then
    if (mi^.eg_fn_id = 0) and (abs(prev_state^.psnl) < LAZY_PSNL) then begin
      approx_psnl := prev_state^.psnl;
      move := st^.lastmove;
      psq_delta := PSQ[piece_moved(move), target_sq(move)]
                 - PSQ[piece_moved(move), source_sq(move)];

      if is_capture(move) then
        dec(psq_delta, PSQ[capture_victim(move), target_sq(move)]);

      inc(approx_psnl, scale(mi^.phase, psq_delta));

      lm := lazy_margin + LAZY_MULT * prev_state^.lazy_count;
      result := (mi^.mtrl_score + approx_psnl) * sign[b.us];

      if (lm - alpha < result) or (lm + beta < -result) then
      begin
        st^.lazy_count := prev_state^.lazy_count + 1;
        st^.psnl := approx_psnl;
        inc(stats.lazy_evals);
        {$ifdef LOG_EVAL}
          log(result * sign[board^.us], 'LAZY');
        {$endif}
        b.update_state();
        goto lazy_exit;
      end;
    end;

  new_eval();
  total := st^.psqval;

  get_pawn_info();
  inc(total, ph^.pawn_score);

  eval_pawns_w();
  eval_pawns_b();

  mob_mask[WHITE] := not (b.bb.white or atk.bp) or (b.bb.black xor b.bb.b_pawns);
  mob_mask[BLACK] := not (b.bb.black or atk.wp) or (b.bb.white xor b.bb.w_pawns);

  eval_knights_w();
  eval_knights_b();

  eval_bishops_w();
  eval_bishops_b();

  atk.white := atk.white or atk.wn or atk.wb;
  atk.black := atk.black or atk.bn or atk.bb;

  eval_rooks_w();
  eval_rooks_b();

  atk.white := atk.white or atk.wr;
  atk.black := atk.black or atk.br;

  eval_queens_w();
  eval_queens_b();

  atk.white := atk.white or atk.wq or atk.wk;
  st^.w_attacks := atk.white;
  atk.black := atk.black or atk.bq or atk.bk;
  st^.b_attacks := atk.black;

  eval_king_w();
  eval_king_b();

  if (st^.threat[b.they] = 2) and (st^.checkersBB = 0) then begin
    result := mate_in(board^.ply + 1) * sign[board^.us];
    st^.flags := st^.flags or FLAG_MATE_IN_1;
    goto finalize;
  end;

  if (st^.phase.by_side[b.us] = 0) and (st^.epsq = NO_EP) then
    if not has_legal_moves() then
      if st^.checkersBB = 0 then begin
        st^.flags := st^.flags or FLAG_DRAW;
        result := SCORE_ZERO;
        goto finalize;
      end
      else begin
        st^.flags := st^.flags or FLAG_MATE;
        result := mated_in(board^.ply) * sign[board^.us];
        goto finalize;
      end;

  eval_passed_pawns_w();
  eval_passed_pawns_b();

  eval_development_w(mi^.space_weight);
  eval_development_b(mi^.space_weight);

  h := hang_bb[WHITE];
  if h <> 0 then
    if h and (h - 1) <> 0 then
      dec(total, MULTI_ATTACK);

  h := hang_bb[BLACK];
  if h <> 0 then
    if h and (h - 1) <> 0 then
      inc(total, MULTI_ATTACK);

  result := scale(mi^.phase, total) + mi^.mtrl_score;

  if result > 0 then
    s := sc[WHITE]
  else
    s := sc[BLACK];

  result := result * s div SCALE_NORMAL;

  if result > 0 then
    dec(result, (ph^.draw_weight[WHITE] * min(result, 256)) div 64)
  else
    inc(result, (ph^.draw_weight[BLACK] * min(-result, 256)) div 64);

  if (mi^.eg_fn_id <> 0) then
    with eg_fn_tab[TEgFnID(mi^.eg_fn_id)] do begin
      egfn.code := fn;
      egfn.data := @self;
      if TEgFunction(egfn)(side, result) then begin
        inc(stats.eg_recog);
        {$ifdef LOG_EVAL}
          log(result, 'EG');
        {$endif}
        goto finalize;
      end;
    end;

  if (st^.flags and FLAG_W_KM <> 0) and (result > 0) then
    if TEgEval(self).eg_minor(WHITE, result) then begin
      inc(stats.eg_recog);
      {$ifdef LOG_EVAL}
        log(result, 'EG');
      {$endif}
      goto finalize;
    end;

  if (st^.flags and FLAG_B_KM <> 0) and (result < 0) then
    if TEgEval(self).eg_minor(BLACK, result) then begin
      inc(stats.eg_recog);
      {$ifdef LOG_EVAL}
        log(result, 'EG');
      {$endif}
      goto finalize;
    end;

  {$ifdef LOG_EVAL}
    log(result, 'FULL');
  {$endif}

finalize:

  if st^.flags and FLAG_MATE_IN_1 = 0 then
    ec^ := (st^.hashkey and EC_KEY_MASK) or uint64(result and EC_SCORE_MASK);

  assert(valid_score(result));
  psnl := result - mi^.mtrl_score;
  if psnl > SCORE_WIN then psnl := SCORE_WIN;
  if psnl < -SCORE_WIN then psnl := -SCORE_WIN;

  if (st^.lastmove <> NOMOVE) and is_quiet(st^.lastmove) then
    if (prev_state^.lazy_count = 0) then begin
      assert(valid_score(prev_state^.psnl));
      gain := psnl - prev_state^.psnl;
      if gain > SCORE_WIN then gain := SCORE_WIN;
      if gain < -SCORE_WIN then gain := -SCORE_WIN;
      hist.set_psnl_gain(st^.lastmove, gain * sign[b.they]);
    end;

  st^.psnl := psnl;
  result := result * sign[b.us];

lazy_exit:
  if (st^.rule50 > 50) then
    result := (result * (114 - st^.rule50)) div 64;

  st^.eval := result;
  assert(valid_score(st^.eval));

{$ifdef DEBUG_BOARD}
  if not b.debug() then begin
    printf('error after eval' + LineEnding);
    print_board(b);
    b.print_move_history();
    readln;
  end;
{$endif}
end;

procedure TEvaluator.new_eval();
var
  b: TBitboard;
begin
  nbr_attacks [WHITE] := 0;  nbr_attacks [BLACK] := 0;
  k_att_cnt   [WHITE] := 0;  k_att_cnt   [BLACK] := 0;
  k_att_weight[WHITE] := 0;  k_att_weight[BLACK] := 0;
  undeveloped [WHITE] := 0;  undeveloped [BLACK] := 0;

  q_attack[WHITE] := false;
  q_attack[BLACK] := false;

  st^.pins[WHITE] := 0;
  st^.pins[BLACK] := 0;

  st^.checkersBB := 0;

  b := KingStep[board^.kingsq[WHITE]];
  atk.wk := b;
  king_ring[WHITE] := b or board^.bb.w_king;

  b := KingStep[board^.kingsq[BLACK]];
  atk.bk := b;
  king_ring[BLACK] := b or board^.bb.b_king;
end;

procedure TEvaluator.eval_pawns_w();
var
  b, w, e, chk: TBitboard;
begin
  w := pawn_west_w(board^.bb.w_pawns);
  chk := w and board^.bb.b_king;
  if chk <> 0 then
    st^.checkersBB := st^.checkersBB or pawn_east_b(chk);

  e := pawn_east_w(board^.bb.w_pawns);
  chk := e and board^.bb.b_king;
  if chk <> 0 then
    st^.checkersBB := st^.checkersBB or pawn_west_b(chk);

  b := e or w;

  atk.wp := b;
  atk.white := b;
  pawn_safe[BLACK] := not b;
  hang_bb[BLACK] := b and (board^.bb.black xor board^.bb.b_pawns xor board^.bb.b_king);

  if b and king_ring[BLACK] <> 0 then begin
    inc(k_att_cnt[BLACK]);
    inc(k_att_weight[BLACK], ATK_WEIGHT_PAWN);
  end;

  b := TBB(board^.bb.w_pawns shl 8) and board^.bb.occupied;
  dec(total, PAWN_BLOCK * popcnt_15(b));
end;

procedure TEvaluator.eval_pawns_b();
var
  b, w, e, chk: TBitboard;
begin
  w := pawn_west_b(board^.bb.b_pawns);
  chk := w and board^.bb.w_king;
  if chk <> 0 then
    st^.checkersBB := st^.checkersBB or pawn_east_w(chk);

  e := pawn_east_b(board^.bb.b_pawns);
  chk := e and board^.bb.w_king;
  if chk <> 0 then
    st^.checkersBB := st^.checkersBB or pawn_west_w(chk);

  b := e or w;

  atk.bp := b;
  atk.black := b;
  pawn_safe[WHITE] := not b;
  hang_bb[WHITE] := b and (board^.bb.white xor board^.bb.w_pawns xor board^.bb.w_king);

  if b and king_ring[WHITE] <> 0 then begin
    inc(k_att_cnt[WHITE]);
    inc(k_att_weight[WHITE], ATK_WEIGHT_PAWN);
  end;

  b := TBB(board^.bb.b_pawns shr 8) and board^.bb.occupied;
  inc(total, PAWN_BLOCK * popcnt_15(b));
end;

procedure TEvaluator.eval_knights_w();
var
  score: TScorePair;
  sq: TSquare;
  sqbb, b, a, h: TBitboard;
  lp: PShortInt;
begin
  score := 0;
  atk.wn := 0;

  lp := @board^.piece_list.wn;

  while lp^ <> -1 do begin
    sq := TSquare(lp^);
    sqbb := SquareBB[sq];
    b := KnightStep[sq];
    atk.wn := atk.wn or b;
    nbr_attacks[WHITE] := nbr_attacks[WHITE] or b;

    if b and board^.bb.b_king <> 0 then
      st^.checkersBB := st^.checkersBB or sqbb;

    if b and king_ring[BLACK] <> 0 then begin
      inc(k_att_cnt[BLACK]);
      inc(k_att_weight[BLACK], ATK_WEIGHT_KNIGHT);
    end;

    if b and king_ring[WHITE] <> 0 then
      inc(score, KNIGHT_DEFENSE);

    if b and pawn_safe[WHITE] and board^.bb.b_pawns <> 0 then
      inc(score, KNIGHT_VS_PAWN);

    if b and pawn_safe[WHITE] and board^.bb.b_bishops <> 0 then
      inc(score, KNIGHT_VS_MINOR);

    with board^ do begin
      h := b and (bb.b_queens or bb.b_rooks);
      if h <> 0 then begin
        hang_bb[BLACK] := hang_bb[BLACK] or h;
        inc(score, KNIGHT_VS_MAJOR);
      end;
    end;

    if sqbb and atk.bp <> 0 then
      dec(score, PAWN_VS_MINOR);

    inc(score, MOB_KNIGHT * popcnt_15(b and mob_mask[WHITE] and InFrontBB[WHITE, rank_of(sq)]));

    if sqbb and BB_OutpostW <> 0 then begin
      a := board^.bb.b_pawns and AdjacentFilesBB[file_of(sq)] and InFrontBB[WHITE, rank_of(sq)];
      if a = 0 then begin
        inc(score, OUTPOST_N);
        if PawnAttacksBB[BLACK, sq] and board^.bb.w_pawns <> 0 then begin
          inc(score, OUTPOST_N_PROT);
          if b and (king_ring[BLACK] or (board^.bb.black and pawn_safe[WHITE])) <> 0 then
            inc(score, KNIGHT_OUTPOST_SQ[sq] * $10001);
        end;
      end;
    end;

    b := b and not board^.bb.w_pawns;
    if b and (int64(b) - 1) = 0 then
      if b <> 0 then
        dec(score, KNIGHT_TRAPPED1)
      else
        dec(score, KNIGHT_TRAPPED2);

    if rank_of(sq) = RANK_1 then
      inc(undeveloped[WHITE]);

    inc(lp);
  end;

  inc(total, score);
end;

procedure TEvaluator.eval_knights_b();
var
  score: TScorePair;
  sq: TSquare;
  SqBB, b, a, h: TBitboard;
  lp: PShortInt;
begin
  score := 0;
  atk.bn := 0;

  lp := @board^.piece_list.bn;

  while lp^ <> -1 do begin
    sq := TSquare(lp^);
    SqBB := SquareBB[sq];
    b := KnightStep[sq];
    atk.bn := atk.bn or b;
    nbr_attacks[BLACK] := nbr_attacks[BLACK] or b;

    if b and board^.bb.w_king <> 0 then
      st^.checkersBB := st^.checkersBB or SqBB;

    if b and king_ring[WHITE] <> 0 then begin
      inc(k_att_cnt[WHITE]);
      inc(k_att_weight[WHITE], ATK_WEIGHT_KNIGHT);
    end;

    if b and king_ring[BLACK] <> 0 then
      inc(score, KNIGHT_DEFENSE);

    if b and pawn_safe[BLACK] and board^.bb.w_pawns <> 0 then
      inc(score, KNIGHT_VS_PAWN);

    if b and pawn_safe[BLACK] and board^.bb.w_bishops <> 0 then
      inc(score, KNIGHT_VS_MINOR);

    with board^ do
      h := b and (bb.w_queens or bb.w_rooks);

    if h <> 0 then begin
      inc(score, KNIGHT_VS_MAJOR);
      hang_bb[WHITE] := hang_bb[WHITE] or h;
    end;

    if SqBB and atk.wp <> 0 then
      dec(score, PAWN_VS_MINOR);

    inc(score, MOB_KNIGHT * popcnt_15(b and mob_mask[BLACK] and InFrontBB[BLACK, rank_of(sq)]));

    if SqBB and BB_OutpostB <> 0 then begin
      a := board^.bb.w_pawns and AdjacentFilesBB[file_of(sq)] and InFrontBB[BLACK, rank_of(sq)];
      if a = 0 then begin
        inc(score, OUTPOST_N);
        if PawnAttacksBB[WHITE, sq] and board^.bb.b_pawns <> 0 then begin
          inc(score, OUTPOST_N_PROT);
          if b and (king_ring[WHITE] or (board^.bb.white and pawn_safe[BLACK])) <> 0 then
            inc(score, KNIGHT_OUTPOST_SQ[flip(sq)] * $10001);
        end;
      end;
    end;

    b := b and not board^.bb.b_pawns;
    if b = 0 then
      dec(score, KNIGHT_TRAPPED2)
    else
      if b and (int64(b) - 1) = 0 then
        dec(score, KNIGHT_TRAPPED1);

    if rank_of(sq) = RANK_8 then
      inc(undeveloped[BLACK]);

    inc(lp);
  end;

  dec(total, score);
end;

procedure TEvaluator.eval_bishops_w();
var
  score: TScorePair;
  sq, psq, pin_sq, _b2, _b3, _c3: TSquare;
  SqBB, pin, bb_patk: TBitboard;
  b, a, h: TBitboard;
  lp: PShortInt;
begin
  score := 0;
  atk.wb := 0;

  lp := @board^.piece_list.wb;

  while lp^ <> -1 do begin
    sq := TSquare(lp^);
    SqBB := SquareBB[sq];
    b := bishop_attacks(sq, board^.bb.occupied xor board^.bb.w_queens);
    nbr_attacks[WHITE] := nbr_attacks[WHITE] or b;

    if b and king_ring[BLACK] <> 0 then begin
      inc(k_att_cnt[BLACK]);
      inc(k_att_weight[BLACK], ATK_WEIGHT_BISHOP);
    end;

    if Direction[board^.kingsq[BLACK], sq] and DIR_DIAG <> 0 then begin
      pin := BetweenBB[board^.kingsq[BLACK], sq] and board^.bb.occupied;
      if pin <> 0 then begin
        if pin and (pin-1) = 0 then begin
          st^.pins[WHITE] := st^.pins[WHITE] or pin;
          pin_sq := find_lsb(pin);
          st^.pinners[WHITE, pin_sq] := ord(sq);
          inc(score, BISHOP_PIN[board^.squares[pin_sq], WHITE]);
        end
      end
      else
        st^.checkersBB := st^.checkersBB or SqBB;
    end;

    b := board^.bishop_attacks(sq);
    atk.wb := atk.wb or b;

    if b and atk.wk <> 0 then
      inc(score, BISHOP_DEFENSE);

    if b and pawn_safe[WHITE] and board^.bb.b_pawns <> 0 then
      inc(score, BISHOP_VS_PAWN);

    if b and pawn_safe[WHITE] and board^.bb.b_knights <> 0 then
      inc(score, BISHOP_VS_MINOR);

    with board^ do begin
      h := (bb.b_queens or bb.b_rooks) and b;
      if h <> 0 then begin
        inc(score, BISHOP_VS_MAJOR);
        hang_bb[BLACK] := hang_bb[BLACK] or h;
      end;
    end;

    if SqBB and atk.bp <> 0 then
      dec(score, PAWN_VS_MINOR);

    if SqBB and BB_OutpostW <> 0 then begin
      a := board^.bb.b_pawns and AdjacentFilesBB[file_of(sq)] and InFrontBB[WHITE, rank_of(sq)];
      if (a = 0) and (PawnAttacksBB[BLACK, sq] and board^.bb.w_pawns <> 0) then begin
        inc(score, OUTPOST_B);
        if b and (king_ring[BLACK] or (board^.bb.black and pawn_safe[WHITE])) <> 0 then
          inc(score, OUTPOST_B_ATK);
      end;
    end;

    inc(score, MOB_BISHOP * popcnt_15(b and mob_mask[WHITE] and InFrontBB[WHITE, rank_of(sq)]));

    if rank_of(sq) = RANK_1 then
      inc(undeveloped[WHITE]);

    bb_patk := InFrontBB[BLACK, rank_of(sq)] and board^.bb.b_pawns and not atk.bp;
    if SqBB and BB_Light <> 0 then begin
      inc(score, popcnt_15(bb_patk and BB_Light) * pair(0, 5));
      dec(score, (ph^.light_block[WHITE] + ph^.light_block[BLACK] div 2) * pair(2, 2));
    end
    else begin
      inc(score, popcnt_15(bb_patk and BB_Dark) * pair(0, 5));
      dec(score, (ph^.dark_block[WHITE] + ph^.dark_block[BLACK] div 2) * pair(2, 2));
    end;

    if SqBB and BB_AH67 <> 0 then begin
      psq := sq;
      if file_of(sq) = FILE_A then
        inc(psq, DELTA_SE)
      else
        inc(psq, DELTA_SW);
      if board^.squares[psq] = B_PAWN then begin
        dec(score, BISHOP_TRAPPED);
        if atk.bp and SquareBB[psq] <> 0 then
          dec(score, BISHOP_TRAPPED);
      end;
    end;

    if chess_960 and (SqBB and BB_A1H1 <> 0) then begin
      if file_of(sq) = FILE_A then begin
        _b2 := B2; _b3 := B3; _c3 := C3;
      end
      else begin
        _b2 := G2; _b3 := G3; _c3 := F3;
      end;
      if board^.squares[_b2] = W_PAWN then
        if board^.squares[_b3] <> NOPIECE then
          dec(score, 2 * BISHOP_TRAPPED)
        else if board^.squares[_c3] = W_PAWN then
          dec(score, BISHOP_TRAPPED)
        else
          dec(score, BISHOP_TRAPPED div 2);
    end;
    inc(lp);
  end;
  inc(total, score);
end;

procedure TEvaluator.eval_bishops_b();
var
  score: TScorePair;
  sq, psq, pin_sq, _b7, _b6, _c6: TSquare;
  SqBB, pin, bb_patk: TBitboard;
  b, a, h: TBitboard;
  lp: PShortInt;
begin
  score := 0;
  atk.bb := 0;

  lp := @board^.piece_list.bb;

  while lp^ <> -1 do begin
    sq := TSquare(lp^);
    SqBB := SquareBB[sq];
    b := bishop_attacks(sq, board^.bb.occupied xor board^.bb.b_queens);
    nbr_attacks[BLACK] := nbr_attacks[BLACK] or b;

    if b and king_ring[WHITE] <> 0 then begin
      inc(k_att_cnt[WHITE]);
      inc(k_att_weight[WHITE], ATK_WEIGHT_BISHOP);
    end;

    if Direction[board^.kingsq[WHITE], sq] and DIR_DIAG <> 0 then begin
      pin := BetweenBB[board^.kingsq[WHITE], sq] and board^.bb.occupied;
      if pin <> 0 then begin
        if pin and (pin-1) = 0 then begin
          st^.pins[BLACK] := st^.pins[BLACK] or pin;
          pin_sq := find_lsb(pin);
          st^.pinners[BLACK, pin_sq] := ord(sq);
          inc(score, BISHOP_PIN[board^.squares[pin_sq], BLACK]);
        end
      end
      else
        st^.checkersBB := st^.checkersBB or SqBB;
    end;

    b := board^.bishop_attacks(sq);
    atk.bb := atk.bb or b;

    if b and atk.bk <> 0 then
      inc(score, BISHOP_DEFENSE);

    if b and pawn_safe[BLACK] and board^.bb.w_pawns <> 0 then
      inc(score, BISHOP_VS_PAWN);

    if b and pawn_safe[BLACK] and board^.bb.w_knights <> 0 then
      inc(score, BISHOP_VS_MINOR);

    with board^ do begin
      h := (bb.w_queens or bb.w_rooks) and b;
      if h <> 0 then begin
        inc(score, BISHOP_VS_MAJOR);
        hang_bb[WHITE] := hang_bb[WHITE] or h;
      end;
    end;

    if SqBB and atk.wp <> 0 then
      dec(score, PAWN_VS_MINOR);

    if SqBB and BB_OutpostB <> 0 then begin
      a := board^.bb.w_pawns and AdjacentFilesBB[file_of(sq)] and InFrontBB[BLACK, rank_of(sq)];
      if (a = 0) and (PawnAttacksBB[WHITE, sq] and board^.bb.b_pawns <> 0) then begin
        inc(score, OUTPOST_B);
        if b and (king_ring[WHITE] or (board^.bb.white and pawn_safe[BLACK])) <> 0 then
          inc(score, OUTPOST_B_ATK);
      end;
    end;

    inc(score, MOB_BISHOP * popcnt_15(b and mob_mask[BLACK] and InFrontBB[BLACK, rank_of(sq)]));

    if rank_of(sq) = RANK_8 then
      inc(undeveloped[BLACK]);

    bb_patk := InFrontBB[WHITE, rank_of(sq)] and board^.bb.w_pawns and not atk.wp;
    if SqBB and BB_Light <> 0 then begin
      inc(score, popcnt_15(bb_patk and BB_Light) * pair(0, 5));
      dec(score, (ph^.light_block[BLACK] + ph^.light_block[WHITE] div 2) * pair(2, 2));
    end
    else begin
      inc(score, popcnt_15(bb_patk and BB_Dark) * pair(0, 5));
      dec(score, (ph^.dark_block[BLACK] + ph^.dark_block[WHITE] div 2) * pair(2, 2));
    end;

    if SqBB and BB_AH23 <> 0 then begin
      psq := sq;
      if file_of(sq) = FILE_A then
        inc(psq, DELTA_NE)
      else
        inc(psq, DELTA_NW);
      if board^.squares[psq] = W_PAWN then begin
        dec(score, BISHOP_TRAPPED);
        if is_set(atk.wp, psq) then
          dec(score, BISHOP_TRAPPED);
      end;
    end;

    if chess_960 and (SqBB and BB_A8H8 <> 0) then begin
      if file_of(sq) = FILE_A then begin
        _b7 := B7; _b6 := B6; _c6 := C6;
      end
      else begin
        _b7 := G7; _b6 := G6; _c6 := F6;
      end;
      if board^.squares[_b7] = B_PAWN then
        if board^.squares[_b6] <> NOPIECE then
          dec(score, 2 * BISHOP_TRAPPED)
        else if board^.squares[_c6] = B_PAWN then
          dec(score, BISHOP_TRAPPED)
        else
          dec(score, BISHOP_TRAPPED div 2);
    end;

    inc(lp);
  end;
  dec(total, score);
end;

procedure TEvaluator.eval_rooks_w();
var
  score: TScorePair;
  sq, pin_sq, blocksq, bksq, wksq: TSquare;
  SqBB, pin: TBitboard;
  b, a, h: TBitboard;
  lp: PShortInt;
  mob: integer;
begin
  score := 0;
  atk.wr := 0;
  lp := @board^.piece_list.wr;

  bksq := board^.kingsq[BLACK];

  while lp^ <> -1 do begin
    sq := TSquare(lp^);
    SqBB := SquareBB[sq];

    with board^.bb do
      b := rook_attacks(sq, occupied xor w_queens xor w_rooks);

    nbr_attacks[WHITE] := nbr_attacks[WHITE] or b;

    if b and king_ring[BLACK] <> 0 then begin
      inc(k_att_cnt[BLACK]);
      inc(k_att_weight[BLACK], ATK_WEIGHT_ROOK);
    end;

    b := board^.rook_attacks(sq);
    atk.wr := atk.wr or b;

    if rank_of(sq) = RANK_7 then begin
      if (board^.bb.b_king or board^.bb.b_pawns) and BB_Ranks78 <> 0 then begin
        inc(score, ROOK_ON_7TH);
        if b and (board^.bb.w_queens or board^.bb.w_rooks) and BB_Rank7 <> 0 then
          if rank_of(bksq) = RANK_8 then
            inc(score, ROOK_ON_7TH_DBL);
      end;
    end
    else
    if rank_of(sq) = RANK_8 then begin
      if (rank_of(bksq) = RANK_8) then
        inc(score, ROOK_ON_8TH);
    end
    else
    if rank_of(sq) = RANK_6 then
      if (board^.bb.b_king or board^.bb.b_pawns) and BB_Ranks678 <> 0 then
        inc(score, ROOK_ON_6TH);

    if Direction[bksq, sq] and DIR_ORTH <> 0 then begin
      pin := BetweenBB[bksq, sq] and board^.bb.occupied;
      if pin <> 0 then begin
        if pin and (pin-1) = 0 then begin
          st^.pins[WHITE] := st^.pins[WHITE] or pin;
          pin_sq := find_lsb(pin);
          st^.pinners[WHITE, pin_sq] := byte(sq);
          inc(score, ROOK_PIN[board^.squares[pin_sq], WHITE]);
        end
      end
      else
        st^.checkersBB := st^.checkersBB or SqBB;
    end;

    if b and atk.wk <> 0 then
      inc(score, ROOK_DEFENSE);

    if b and pawn_safe[WHITE] and board^.bb.b_pawns <> 0 then
      inc(score, ROOK_VS_PAWN);

    if b and pawn_safe[WHITE] and (board^.bb.b_knights or board^.bb.b_bishops) <> 0 then
      inc(score, ROOK_VS_MINOR);

    h := b and board^.bb.b_queens;
    if h <> 0 then begin
      inc(score, ROOK_VS_QUEEN);
      hang_bb[BLACK] := hang_bb[BLACK] or h;
    end;

    if SqBB and atk.bp <> 0 then
      dec(score, PAWN_VS_ROOK);

    if SqBB and BB_OutpostW <> 0 then begin
      a := board^.bb.b_pawns and AdjacentFilesBB[file_of(sq)] and InFrontBB[WHITE, rank_of(sq)];
      if (a = 0) and (PawnAttacksBB[BLACK, sq] and board^.bb.w_pawns <> 0) then begin
        inc(score, OUTPOST_R);
        a := king_ring[BLACK] or (board^.bb.black and pawn_safe[WHITE]);
        if (b and a and RankBB[rank_of(sq)]) <> 0 then
          inc(score, OUTPOST_R_ATK);
      end;
    end;

    mob := popcnt_15(b and not (atk.bp or atk.bn or atk.bb or board^.bb.w_pawns));
    inc(score, mob * MOB_ROOK);

    if board^.bb.w_pawns and ForwardBB[WHITE, sq] = 0 then begin
      inc(score, ROOK_HALF_OPEN);
      a := board^.bb.b_pawns and ForwardBB[WHITE, sq];
      if a = 0 then begin
        b := (board^.bb.b_knights or board^.bb.b_bishops) and atk.bp and ForwardBB[WHITE, sq];
        if b <> 0 then begin
          blocksq := BSF(b);
          b := AdjacentFilesBB[file_of(blocksq)] and InFrontBB[BLACK, rank_of(blocksq)];
          if b and board^.bb.w_pawns <> 0 then
            inc(score, ROOK_OPEN_MINOR)
          else
            inc(score, ROOK_OPEN_BLOCK);
        end
        else
          inc(score, ROOK_OPEN);
      end
      else begin
        blocksq := BSF(a);
        b := AdjacentFilesBB[file_of(blocksq)] and InFrontBB[WHITE, rank_of(blocksq)];
        if b and board^.bb.b_pawns = 0 then
          inc(score, ROOK_ALMOST_OPEN);
      end;
      if (board^.bb.b_king and ForwardBB[WHITE, sq] <> 0) then
        inc(score, ROOK_KING_FILE);
    end
    else
      if mob <= 4 then begin
        wksq := board^.kingsq[WHITE];
        if (file_of(wksq) >= FILE_E) and (file_of(sq) > file_of(wksq)) and
           ((rank_of(wksq) = RANK_1) or (rank_of(wksq) = rank_of(sq))) then begin
          if ph^.pawn_files[WHITE] and files_east[file_of(wksq)] <> 0 then
            if st^.castle_rights and CR_W_BOTH <> 0 then
              dec(score, pair((ROOK_TRAPPED - (mob * 16)) div 2, 0))
            else
              dec(score, pair((ROOK_TRAPPED - (mob * 16)), 0))
        end else
        if (file_of(wksq) <= FILE_D) and (file_of(sq) < file_of(wksq)) and
           ((rank_of(wksq) = RANK_1) or (rank_of(wksq) = rank_of(sq))) then begin
          if ph^.pawn_files[WHITE] and files_west[file_of(wksq)] <> 0 then
            if st^.castle_rights and CR_W_BOTH <> 0 then
              dec(score, pair((ROOK_TRAPPED - (mob * 16)) div 2, 0))
            else
              dec(score, pair((ROOK_TRAPPED - (mob * 16)), 0))
        end;
      end;

    inc(lp);
  end;
  inc(total, score);
end;

procedure TEvaluator.eval_rooks_b();
var
  score: TScorePair;
  sq, pin_sq, blocksq, bksq, wksq: TSquare;
  SqBB, pin: TBitboard;
  b, a, h: TBitboard;
  lp: PShortInt;
  mob: integer;
begin
  score := 0;
  atk.br := 0;
  lp := @board^.piece_list.br;

  wksq := board^.kingsq[WHITE];

  while lp^ <> -1 do begin
    sq := TSquare(lp^);
    SqBB := SquareBB[sq];

    with board^.bb do
      b := rook_attacks(sq, occupied xor b_queens xor b_rooks);

    nbr_attacks[BLACK] := nbr_attacks[BLACK] or b;

    if b and king_ring[WHITE] <> 0 then begin
      inc(k_att_cnt[WHITE]);
      inc(k_att_weight[WHITE], ATK_WEIGHT_ROOK);
    end;

    b := board^.rook_attacks(sq);
    atk.br := atk.br or b;

    if rank_of(sq) = RANK_2 then begin
      if (board^.bb.w_king or board^.bb.w_pawns) and BB_Ranks12 <> 0 then begin
        inc(score, ROOK_ON_7TH);
        if b and (board^.bb.b_queens or board^.bb.b_rooks) and BB_Rank2 <> 0 then
          if rank_of(wksq) = RANK_1 then
            inc(score, ROOK_ON_7TH_DBL);
      end;
    end
    else
    if rank_of(sq) = RANK_1 then begin
      if (rank_of(wksq) = RANK_1) then
        inc(score, ROOK_ON_8TH);
    end
    else
    if rank_of(sq) = RANK_3 then
      if (board^.bb.w_king or board^.bb.w_pawns) and BB_Ranks123 <> 0 then
        inc(score, ROOK_ON_6TH);

    if Direction[wksq, sq] and DIR_ORTH <> 0 then begin
      pin := BetweenBB[wksq, sq] and board^.bb.occupied;
      if pin <> 0 then begin
        if pin and (pin-1) = 0 then begin
          st^.pins[BLACK] := st^.pins[BLACK] or pin;
          pin_sq := find_lsb(pin);
          st^.pinners[BLACK, pin_sq] := byte(sq);
          inc(score, ROOK_PIN[board^.squares[pin_sq], BLACK]);
        end
      end
      else
        st^.checkersBB := st^.checkersBB or SqBB;
    end;

    if b and atk.bk <> 0 then
      inc(score, ROOK_DEFENSE);

    if b and pawn_safe[BLACK] and board^.bb.w_pawns <> 0 then
      inc(score, ROOK_VS_PAWN);

    if b and pawn_safe[BLACK] and (board^.bb.w_knights or board^.bb.w_bishops) <> 0 then
      inc(score, ROOK_VS_MINOR);

    h := b and board^.bb.w_queens;
    if h <> 0 then begin
      inc(score, ROOK_VS_QUEEN);
      hang_bb[WHITE] := hang_bb[WHITE] or h;
    end;

    if SqBB and atk.wp <> 0 then
      dec(score, PAWN_VS_ROOK);

    if SqBB and BB_OutpostB <> 0 then begin
      a := board^.bb.w_pawns and AdjacentFilesBB[file_of(sq)] and InFrontBB[BLACK, rank_of(sq)];
      if (a = 0) and (PawnAttacksBB[WHITE, sq] and board^.bb.b_pawns <> 0) then begin
        inc(score, OUTPOST_R);
        a := king_ring[WHITE] or (board^.bb.white and pawn_safe[BLACK]);
        if (b and a and RankBB[rank_of(sq)]) <> 0 then
          inc(score, OUTPOST_R_ATK);
      end;
    end;

    mob := popcnt_15(b and not (atk.wp or atk.wn or atk.wb or board^.bb.b_pawns));
    inc(score, mob * MOB_ROOK);

    if board^.bb.b_pawns and ForwardBB[BLACK, sq] = 0 then begin
      inc(score, ROOK_HALF_OPEN);
      a := board^.bb.w_pawns and ForwardBB[BLACK, sq];
      if a = 0 then begin
        b := (board^.bb.w_knights or board^.bb.w_bishops) and atk.wp and ForwardBB[BLACK, sq];
        if b <> 0 then begin
          blocksq := BSR(b);
          b := AdjacentFilesBB[file_of(blocksq)] and InFrontBB[WHITE, rank_of(blocksq)];
          if b and board^.bb.b_pawns <> 0 then
            inc(score, ROOK_OPEN_MINOR)
          else
            inc(score, ROOK_OPEN_BLOCK);
        end
        else
          inc(score, ROOK_OPEN);
      end
      else begin
        blocksq := BSR(a);
        b := AdjacentFilesBB[file_of(blocksq)] and InFrontBB[BLACK, rank_of(blocksq)];
        if b and board^.bb.w_pawns = 0 then
          inc(score, ROOK_ALMOST_OPEN);
      end;
      if (board^.bb.w_king and ForwardBB[BLACK, sq] <> 0) then
        inc(score, ROOK_KING_FILE);
    end
    else
      if mob <= 4 then begin
        bksq := board^.kingsq[BLACK];
        if (file_of(bksq) >= FILE_E) and (file_of(sq) > file_of(bksq)) and
           ((rank_of(bksq) = RANK_8) or (rank_of(bksq) = rank_of(sq))) then begin
          if ph^.pawn_files[BLACK] and files_east[file_of(bksq)] <> 0 then
            if st^.castle_rights and CR_B_BOTH <> 0 then
              dec(score, pair((ROOK_TRAPPED - (mob * 16)) div 2, 0))
            else
              dec(score, pair((ROOK_TRAPPED - (mob * 16)), 0))
        end else
        if (file_of(bksq) <= FILE_D) and (file_of(sq) < file_of(bksq)) and
           ((rank_of(bksq) = RANK_8) or (rank_of(bksq) = rank_of(sq))) then begin
          if ph^.pawn_files[BLACK] and files_west[file_of(bksq)] <> 0 then
            if st^.castle_rights and CR_B_BOTH <> 0 then
              dec(score, pair((ROOK_TRAPPED - (mob * 16)) div 2, 0))
            else
              dec(score, pair((ROOK_TRAPPED - (mob * 16)), 0))
        end;
      end;

    inc(lp);
  end;
  dec(total, score);
end;

procedure TEvaluator.eval_queens_w();
var
  score: TScorePair;
  sq, pin_sq, ksq: TSquare;
  pin, b: TBitboard;
  dir: TDirection;
  lp: PShortInt;
begin
  score := 0;
  atk.wq := 0;
  ksq := board^.kingsq[BLACK];
  lp := @board^.piece_list.wq;

  while lp^ <> -1 do begin
    sq := TSquare(lp^);
    dir := Direction[ksq, sq];
    if dir <> 0 then begin
      pin := BetweenBB[ksq, sq] and board^.bb.occupied;
      if pin <> 0 then begin
        if pin and (pin-1) = 0 then begin
          st^.pins[WHITE] := st^.pins[WHITE] or pin;
          pin_sq := find_lsb(pin);
          st^.pinners[WHITE, pin_sq] := byte(sq);
          if dir and DIR_ORTH <> 0 then
            inc(score, QUEEN_PIN_ORTH[board^.squares[pin_sq], WHITE])
          else
            inc(score, QUEEN_PIN_DIAG[board^.squares[pin_sq], WHITE]);
        end
      end
      else
        st^.checkersBB := st^.checkersBB or SquareBB[sq];
    end;

    b := board^.queen_attacks(sq);
    atk.wq := atk.wq or b;

    if b and king_ring[BLACK] <> 0 then begin
      inc(k_att_cnt[BLACK]);
      inc(k_att_weight[BLACK], ATK_WEIGHT_QUEEN);
      q_attack[BLACK] := true;
    end;

    if b and atk.wk <> 0 then
      inc(score, QUEEN_DEFENSE);

    if b and pawn_safe[WHITE] and board^.bb.black <> 0 then
      inc(score, QUEEN_VS_PIECE);

    if atk.bp and SquareBB[sq] <> 0 then
      dec(score, PAWN_VS_QUEEN);

    inc(score, MOB_QUEEN * popcnt(b and not (atk.bp or nbr_attacks[BLACK] or board^.bb.white)));

    if rank_of(sq) = RANK_7 then
      if (board^.bb.b_king or board^.bb.b_pawns) and BB_Ranks78 <> 0 then begin
        inc(score, QUEEN_ON_7TH);
        if b and board^.bb.w_rooks and BB_Rank7 <> 0 then
          if rank_of(ksq) = RANK_8 then
            inc(score, QUEEN_ON_7TH_DBL);
      end;

    inc(lp);
  end;
  inc(total, score);
end;

procedure TEvaluator.eval_queens_b();
var
  score: TScorePair;
  sq, pin_sq, ksq: TSquare;
  pin, b: TBitboard;
  dir: TDirection;
  lp: PShortInt;
begin
  score := 0;
  atk.bq := 0;
  ksq := board^.kingsq[WHITE];
  lp := @board^.piece_list.bq;

  while lp^ <> -1 do begin
    sq := TSquare(lp^);
    dir := Direction[ksq, sq];
    if dir <> 0 then begin
      pin := BetweenBB[ksq, sq] and board^.bb.occupied;
      if pin <> 0 then begin
        if pin and (pin-1) = 0 then begin
          st^.pins[BLACK] := st^.pins[BLACK] or pin;
          pin_sq := find_lsb(pin);
          st^.pinners[BLACK, pin_sq] := byte(sq);
          if dir and DIR_ORTH <> 0 then
            inc(score, QUEEN_PIN_ORTH[board^.squares[pin_sq], BLACK])
          else
            inc(score, QUEEN_PIN_DIAG[board^.squares[pin_sq], BLACK]);
        end
      end
      else
        st^.checkersBB := st^.checkersBB or SquareBB[sq];
    end;

    b := board^.queen_attacks(sq);
    atk.bq := atk.bq or b;

    if b and king_ring[WHITE] <> 0 then begin
      inc(k_att_cnt[WHITE]);
      inc(k_att_weight[WHITE], ATK_WEIGHT_QUEEN);
      q_attack[WHITE] := true;
    end;

    if b and atk.bk <> 0 then
      inc(score, QUEEN_DEFENSE);

    if b and pawn_safe[BLACK] and board^.bb.white <> 0 then
      inc(score, QUEEN_VS_PIECE);

    if SquareBB[sq] and atk.wp <> 0 then
      dec(score, PAWN_VS_QUEEN);

    inc(score, MOB_QUEEN * popcnt(b and not(atk.wp or nbr_attacks[WHITE] or board^.bb.black)));

    if rank_of(sq) = RANK_2 then
      if (board^.bb.w_king or board^.bb.w_pawns) and BB_Ranks12 <> 0 then begin
        inc(score, QUEEN_ON_7TH);
        if b and board^.bb.b_rooks and BB_Rank2 <> 0 then
          if rank_of(ksq) = RANK_1 then
            inc(score, QUEEN_ON_7TH_DBL);
      end;

    inc(lp);
  end;
  dec(total, score);
end;

procedure TEvaluator.eval_king_w();
var
  score: TScorePair;
  danger: TScore;
  b, undefended, assistance: TBitboard;
  contact_checks, escapes: TBitboard;
  chksq, ksq: TSquare;
  lp: PShortInt;

  function queen_mate_theat(qsq: TSquare): boolean;
  var occ: TBitboard;
  begin
    if board^.queen_attacks(qsq) and SquareBB[chksq] <> 0 then
      if st^.pins[WHITE] and SquareBB[qsq] = 0 then begin
        occ := board^.bb.occupied xor SquareBB[qsq];
        if (rook_attacks(chksq, occ) and board^.queens_rooks(WHITE) = 0) and
           (bishop_attacks(chksq, occ) and board^.queens_bishops(WHITE) = 0) then
        begin
          result := true;
          exit;
        end;
      end;
    result := false;
  end;

begin
  score := 0;
  ksq := board^.kingsq[WHITE];
  danger := (SafetyMult[k_att_cnt[WHITE]] * k_att_weight[WHITE]) shr 3
    + ph^.king_exposure[WHITE];

  if (board^.count.bq <> 0) then begin
    if q_attack[WHITE] and (k_att_cnt[WHITE] >= 2) then begin

      undefended := atk.black and atk.wk
        and not (atk.wp or nbr_attacks[WHITE] or atk.wq);

      if undefended <> 0 then begin
        b := undefended and atk.bq and not board^.bb.black;
        if b <> 0 then begin
          assistance := nbr_attacks[BLACK] or atk.bp or atk.bk;
          contact_checks := b and assistance;
          if contact_checks <> 0 then begin
            dec(score, QUEEN_CONTACT_CHK);
            hang_bb[WHITE] := hang_bb[WHITE] or board^.bb.w_king;
            st^.threat[WHITE] := 1;
            escapes := king_ring[WHITE] and not (board^.bb.white or assistance);
            repeat
              chksq := find_lsb(contact_checks);
              b := ShadowBB[chksq, ksq] and not board^.queen_attacks(chksq);
              if escapes and b = 0 then begin
                lp := @board^.piece_list.bq;
                repeat
                  if queen_mate_theat(TSquare(lp^)) then begin
                    dec(score, QUEEN_MATE_THREAT);
                    st^.threat[WHITE] := 2;
                  end;
                  inc(lp);
                until lp^ = -1;
              end;
              clear_lsb(contact_checks);
            until contact_checks = 0;
          end;
        end;
      end;
    end;
  end
  else
    with board^.count do
      danger := (danger * (br + bb + bn)) shr 3;

  dec(score, pair(danger, 0));

  b := atk.wk and board^.bb.b_pawns;
  if b and pawn_safe[WHITE] <> 0 then begin
    inc(score, KING_VS_PAWN);
    if b and not atk.black <> 0 then
      inc(score, KING_VS_PAWN);
  end;

  b := BB_CrampFile[file_of(ksq)];
  if b and (board^.bb.b_queens or board^.bb.b_rooks) <> 0 then begin
    dec(score, pair(0, 12));
    if b and (board^.bb.w_pawns or board^.bb.b_pawns)= 0 then
      dec(score, pair(12, 36));
  end;

  inc(total, score);
end;

procedure TEvaluator.eval_king_b();
var
  score: TScorePair;
  danger: TScore;
  b, undefended, assistance: TBitboard;
  contact_checks, escapes: TBitboard;
  chksq, ksq: TSquare;
  lp: PShortInt;

  function queen_mate_theat(qsq: TSquare): boolean;
  var occ: TBitboard;
  begin
    if board^.queen_attacks(qsq) and SquareBB[chksq] <> 0 then
      if st^.pins[BLACK] and SquareBB[qsq] = 0 then begin
        occ := board^.bb.occupied xor SquareBB[qsq];
        if (rook_attacks(chksq, occ) and board^.queens_rooks(BLACK) = 0) and
           (bishop_attacks(chksq, occ) and board^.queens_bishops(BLACK) = 0) then
        begin
          result := true;
          exit;
        end;
      end;
    result := false;
  end;

begin
  score := 0;
  ksq := board^.kingsq[BLACK];
  danger := (SafetyMult[k_att_cnt[BLACK]] * k_att_weight[BLACK]) shr 3
    + ph^.king_exposure[BLACK];

  if (board^.count.wq <> 0) then begin
    if q_attack[BLACK] and (k_att_cnt[BLACK] >= 2) then begin

      undefended := atk.white and atk.bk
        and not (atk.bp or nbr_attacks[BLACK] or atk.bq);

      if undefended <> 0 then begin
        b := undefended and atk.wq and not board^.bb.white;
        if b <> 0 then begin
          assistance := nbr_attacks[WHITE] or atk.wp or atk.wk;
          contact_checks := b and assistance;
          if contact_checks <> 0 then begin
            dec(score, QUEEN_CONTACT_CHK);
            hang_bb[BLACK] := hang_bb[BLACK] or board^.bb.b_king;
            st^.threat[BLACK] := 1;
            escapes := king_ring[BLACK] and not (board^.bb.black or assistance);
            repeat
              chksq := find_lsb(contact_checks);
              b := ShadowBB[chksq, ksq] and not board^.queen_attacks(chksq);
              if escapes and b = 0 then begin
                lp := @board^.piece_list.wq;
                repeat
                  if queen_mate_theat(TSquare(lp^)) then begin
                    dec(score, QUEEN_MATE_THREAT);
                    st^.threat[BLACK] := 2;
                  end;
                  inc(lp);
                until lp^ = -1;
              end;
              clear_lsb(contact_checks);
            until contact_checks = 0;
          end;
        end;
      end;
    end;
  end
  else
    with board^.count do
      danger := (danger * (wr + wb + wn)) shr 3;

  dec(score, pair(danger, 0));

  b := atk.bk and board^.bb.w_pawns;
  if b and pawn_safe[BLACK] <> 0 then begin
    inc(score, KING_VS_PAWN);
    if b and not atk.white <> 0 then
      inc(score, KING_VS_PAWN);
  end;

  b := BB_CrampFile[file_of(ksq)];
  if b and (board^.bb.w_queens or board^.bb.w_rooks) <> 0 then begin
    dec(score, pair(0, 12));
    if b and (board^.bb.w_pawns or board^.bb.b_pawns) = 0 then
      dec(score, pair(12, 36));
  end;

  dec(total, score);
end;

procedure TEvaluator.pawn_struct_w();
var
  score: TScorePair;
  sq, block_sq: TSquare;
  blocker: TPiece;
  f: TFile; r, rr: TRank;
  isolated, doubled, weak, open: boolean;
  chain, candidate, passed: boolean;
  dist: integer;
  pawns, own, opp, b, a: TBitboard;
  own_kdist, opp_kdist: PDistance;
  connections: TBitboard;
begin
  score := 0;
  connections := 0;
  own := board^.bb.w_pawns;
  opp := board^.bb.b_pawns;
  own_kdist := @DistanceKP[WHITE, board^.kingsq[WHITE]];
  opp_kdist := @DistanceKP[WHITE, board^.kingsq[BLACK]];
  pawns := own;
  while pawns <> 0 do begin
    sq := find_lsb(pawns);

    dist := own_kdist^[sq];
    if dist < min_kp_dist[WHITE] then
      min_kp_dist[WHITE] := dist;

    dist := opp_kdist^[sq];
    if dist < min_kp_dist[BLACK] then
      min_kp_dist[BLACK] := dist;

    f := file_of(sq);
    r := rank_of(sq);

    ph^.pawn_files[WHITE] := ph^.pawn_files[WHITE] or (1 shl ord(f));
    isolated := AdjacentFilesBB[f] and own = 0;
    doubled := (own and (FileBB[f] xor SquareBB[sq])) <> 0;
    passed := PassedMaskBB[WHITE, sq] and opp = 0;
    chain := own and AdjacentFilesBB[f] and (RankBB[r] or RankBB[pred(r)]) <> 0;
    weak := false;

    if not (isolated or passed or chain) then
      if (own and AdjacentFilesBB[f] and not InFrontBB[WHITE, r] = 0) then
        if PawnAttacksBB[WHITE, sq] and opp = 0 then begin
          a := PawnAttacksBB[WHITE, sq];
          while a and (own or opp) = 0 do
            a := TBB(a shl 8);
          weak := (a or TBB(a shl 8)) and opp <> 0;
        end;

    candidate := not passed and (opp and ForwardBB[WHITE, sq] = 0) and
      (popcnt_15(AdjacentFilesBB[f] and not InFrontBB[WHITE, r] and own)
       >= popcnt_15(AdjacentFilesBB[f] and InFrontBB[WHITE, r] and opp));

    if candidate then
      inc(score, PAWN_CANDIDATE[r]);  // relative rank!

    open := (own or opp) and ForwardBB[WHITE, sq] = 0;

    if doubled then begin
      dec(score, PAWN_DOUBLED[open, f]);
      if isolated then
        dec(score, PAWN_DBL_ISO[open, f]);
    end;

    if isolated then
      dec(score, PAWN_ISOLATED[open, f]);

    if weak then
      dec(score, PAWN_WEAK[open, f]);

    block_sq := delta_add(sq, DELTA_N);
    if passed and (ForwardBB[WHITE, sq] and own = 0) then begin
      ph^.passer_mask[WHITE] := ph^.passer_mask[WHITE] or (1 shl ord(f));
      rr := relative_rank(r, WHITE);
      inc(score, PP_BASE[rr]);
      if PawnAttacksBB[BLACK, sq] and own <> 0 then
        inc(score, PP_PROTECTED[rr]);

      b := Connection[sq] and connections;
      connections := connections or SquareBB[sq];

      if b <> 0 then begin
        inc(score, PP_CONNECTED[rr] + PP_CONNECTED[rank_of(find_lsb(b))]); // rr
        clear_lsb(b);
        if b <> 0 then
          inc(score, PP_CONNECTED[rr] + PP_CONNECTED[rank_of(find_lsb(b))]); // rr
      end;

      if rr > RANK_3 then begin
        dec(score, pair(0, own_kdist^[block_sq] * PP_KING_DIST[rr]));
        inc(score, pair(0, opp_kdist^[block_sq] * PP_KING_DIST[rr] * 2));
        if (WestOf[f] and opp = 0) or (EastOf[f] and opp = 0) then
          inc(score, PP_OUTSIDE[rr]);
      end;
    end;

    blocker := board^.squares[block_sq];
    if (blocker = B_PAWN) and (r = RANK_6) then begin
      b := own and BB_Rank5;
      if ((b and AdjacentLeftBB[f] <> 0) and (HiddenWest[WHITE, f] and opp = 0)) or
         ((b and AdjacentRightBB[f] <> 0) and (HiddenEast[WHITE, f] and opp = 0)) then
        inc(score, PAWN_HIDDEN);
    end;

    if SquareBB[sq] and BB_Light <> 0 then begin
      inc(ph^.light_block[WHITE], CENTER_BLOCK[sq]);
      if blocker = B_PAWN then
        inc(ph^.light_block[WHITE], CENTER_BLOCK[sq]);
    end
    else begin
      inc(ph^.dark_block[WHITE], CENTER_BLOCK[sq]);
      if blocker = B_PAWN then
        inc(ph^.dark_block[WHITE], CENTER_BLOCK[sq]);
    end;

    clear_lsb(pawns);
  end;

  dec(score, Islands[ph^.pawn_files[WHITE]] * pair(0, 7));

  if ph^.pawn_files[WHITE] = $ff then
    dec(score, NO_OPEN_FILES);

  inc(ph^.pawn_score, score);
end;

procedure TEvaluator.pawn_struct_b();
var
  score: TScorePair;
  sq, block_sq: TSquare;
  blocker: TPiece;
  f: TFile; r, rr: TRank;
  isolated, doubled, weak, open: boolean;
  chain, candidate, passed: boolean;
  dist: integer;
  pawns, own, opp, b, a: TBitboard;
  own_kdist, opp_kdist: PDistance;
  connections: TBitboard;
begin
  score := 0;
  connections := 0;
  own := board^.bb.b_pawns;
  opp := board^.bb.w_pawns;
  own_kdist := @DistanceKP[BLACK, board^.kingsq[BLACK]];
  opp_kdist := @DistanceKP[BLACK, board^.kingsq[WHITE]];
  pawns := own;
  while pawns <> 0 do begin
    sq := find_lsb(pawns);

    dist := own_kdist^[sq];
    if dist < min_kp_dist[BLACK] then
      min_kp_dist[BLACK] := dist;

    dist := opp_kdist^[sq];
    if dist < min_kp_dist[WHITE] then
      min_kp_dist[WHITE] := dist;

    f := file_of(sq);
    r := rank_of(sq);

    ph^.pawn_files[BLACK] := ph^.pawn_files[BLACK] or (1 shl ord(f));
    isolated := AdjacentFilesBB[f] and own = 0;
    doubled := (own and (FileBB[f] xor SquareBB[sq])) <> 0;
    passed := PassedMaskBB[BLACK, sq] and opp = 0;
    chain := own and AdjacentFilesBB[f] and (RankBB[r] or RankBB[succ(r)]) <> 0;
    weak := false;

    if not (isolated or passed or chain) then
      if (own and AdjacentFilesBB[f] and not InFrontBB[BLACK, r] = 0) then
        if PawnAttacksBB[BLACK, sq] and opp = 0 then begin
          a := PawnAttacksBB[BLACK, sq];
          while a and (own or opp) = 0 do
            a := TBB(a shr 8);
          weak := (a or TBB(a shr 8)) and opp <> 0;
        end;

    candidate := not passed and (opp and ForwardBB[BLACK, sq] = 0) and
      (popcnt_15(AdjacentFilesBB[f] and not InFrontBB[BLACK, r] and own)
       >= popcnt_15(AdjacentFilesBB[f] and InFrontBB[BLACK, r] and opp));

    if candidate then
      inc(score, PAWN_CANDIDATE[flip(r)]);  // relative rank!

    open := (own or opp) and ForwardBB[BLACK, sq] = 0;

    if doubled then begin
      dec(score, PAWN_DOUBLED[open, f]);
      if isolated then
        dec(score, PAWN_DBL_ISO[open, f]);
    end;

    if isolated then
      dec(score, PAWN_ISOLATED[open, f]);

    if weak then
      dec(score, PAWN_WEAK[open, f]);

    block_sq := delta_add(sq, DELTA_S);
    if passed and (ForwardBB[BLACK, sq] and own = 0) then begin
      ph^.passer_mask[BLACK] := ph^.passer_mask[BLACK] or (1 shl ord(f));
      rr := relative_rank(r, BLACK);
      inc(score, PP_BASE[rr]);
      if PawnAttacksBB[WHITE, sq] and own <> 0 then
        inc(score, PP_PROTECTED[rr]);

      b := Connection[sq] and connections;
      connections := connections or SquareBB[sq];

      if b <> 0 then begin
        inc(score, PP_CONNECTED[rr] + PP_CONNECTED[rank_of(flip(find_lsb(b)))]); // rr
        clear_lsb(b);
        if b <> 0 then
          inc(score, PP_CONNECTED[rr] + PP_CONNECTED[rank_of(flip(find_lsb(b)))]); // rr
      end;

      if rr > RANK_3 then begin
        dec(score, pair(0, own_kdist^[block_sq] * PP_KING_DIST[rr]));
        inc(score, pair(0, opp_kdist^[block_sq] * PP_KING_DIST[rr] * 2));
        if (WestOf[f] and opp = 0) or (EastOf[f] and opp = 0) then
          inc(score, PP_OUTSIDE[rr]);
      end;
    end;

    blocker := board^.squares[block_sq];
    if (blocker = W_PAWN) and (r = RANK_3) then begin
      b := own and BB_Rank4;
      if (b and AdjacentLeftBB[f] <> 0) and (HiddenWest[BLACK, f] and opp = 0) or
         (b and AdjacentRightBB[f] <> 0) and (HiddenEast[BLACK, f] and opp = 0) then
        inc(score, PAWN_HIDDEN);
    end;

    if SquareBB[sq] and BB_Light <> 0 then begin
      inc(ph^.light_block[BLACK], CENTER_BLOCK[sq]);
      if blocker = W_PAWN then
        inc(ph^.light_block[BLACK], CENTER_BLOCK[sq]);
    end
    else begin
      inc(ph^.dark_block[BLACK], CENTER_BLOCK[sq]);
      if blocker = W_PAWN then
        inc(ph^.dark_block[BLACK], CENTER_BLOCK[sq]);
    end;

    clear_lsb(pawns);
  end;

  dec(score, Islands[ph^.pawn_files[BLACK]] * pair(0, 7));

  if ph^.pawn_files[BLACK] = $ff then
    dec(score, NO_OPEN_FILES);

  dec(ph^.pawn_score, score);
end;

procedure TEvaluator.get_pawn_info();
var
  exposure, castled_exposure: TScore;
  wksq, bksq: TSquare;
  mask, b: TBitboard;
  open, all: integer;
  cr: TCastleRights;
begin
  ph := @pawn_hash^[cardinal(st^.pawnkey) and (PHASH_SIZE - 1)];
  inc(stats.phash_probes);
  if ph^.key = st^.pawnkey then begin
    inc(stats.phash_hits);
    exit;
  end;
  fillchar(ph^, sizeof(ph^), 0);
  ph^.key := st^.pawnkey;

  min_kp_dist[WHITE] := 127;
  min_kp_dist[BLACK] := 127;

  pawn_struct_w();
  pawn_struct_b();

  if min_kp_dist[WHITE] < 64 then
    inc(ph^.pawn_score, (min_kp_dist[BLACK] - min_kp_dist[WHITE]) * pair(0, 8));

  cr := st^.castle_rights;
  inc(ph^.pawn_score, pair(CR_BONUS[cr], 0));

  wksq := board^.kingsq[WHITE];
  bksq := board^.kingsq[BLACK];

  exposure := king_exposure_w(wksq);
  if cr and CR_W_SHORT <> 0 then begin
    castled_exposure := king_exposure_w(G1) + 11;
    exposure := min(exposure, castled_exposure);
  end;
  if cr and CR_W_LONG <> 0 then begin
    castled_exposure := king_exposure_w(C1) + 11;
    exposure := min(exposure, castled_exposure);
  end;
  ph^.king_exposure[WHITE] := exposure;

  exposure := king_exposure_b(bksq);
  if cr and CR_B_SHORT <> 0 then begin
    castled_exposure := king_exposure_b(G8) + 11;
    exposure := min(exposure, castled_exposure);
  end;
  if cr and CR_B_LONG <> 0 then begin
    castled_exposure := king_exposure_b(C8) + 11;
    exposure := min(exposure, castled_exposure);
  end;
  ph^.king_exposure[BLACK] := exposure;

  if (ord(wksq) xor ord(bksq)) and 4 <> 0 then begin
    if file_of(wksq) <= FILE_D then
      mask := TBB($0f0f0f0f0f0f0f0f)
    else
      mask := TBB($f0f0f0f0f0f0f0f0);
    b := board^.bb.w_pawns or board^.bb.b_pawns;
    inc(ph^.pawn_score, (popcnt_15(b and mask) - popcnt_15(b and not mask)) * pair(25, 8));
  end;

  open := popcnt_8bit[ph^.pawn_files[WHITE] and not ph^.pawn_files[BLACK]];
  all := popcnt_8bit[ph^.pawn_files[WHITE]];
  ph^.draw_weight[WHITE] := OPEN_FILE_MULT[open] * PAWN_COUNT_MULT[all];

  open := popcnt_8bit[ph^.pawn_files[BLACK] and not ph^.pawn_files[WHITE]];
  all := popcnt_8bit[ph^.pawn_files[BLACK]];
  ph^.draw_weight[BLACK] := OPEN_FILE_MULT[open] * PAWN_COUNT_MULT[all];

end;

function TEvaluator.king_exposure_w(sq: TSquare): TScore;
var
  storm, shield: integer;
  wp, bp, b: TBitboard;
  wr, br: TRank;
  i: integer;
  sf: PBitboard;
begin
  shield := 0;
  storm := 0;

  sf := @ShieldFiles[file_of(sq), 0];
  wp := board^.bb.w_pawns and not InFrontBB[BLACK, rank_of(sq)];
  bp := board^.bb.b_pawns;

  for i := 0 to 2 do begin
    b := wp and sf[i];
    if b <> 0 then
      wr := rank_of(BSF(b))
    else
      wr := RANK_1;
    inc(shield, PSHIELD[i, wr]);

    b := bp and sf[i];
    if b <> 0 then
      br := rank_of(BSF(b))
    else
      br := RANK_1;
    if integer(br) - integer(wr) = 1 then
      inc(storm, PSTORM[i, br] div 2)
    else
      inc(storm, PSTORM[i, br]);
  end;

  if shield = 0 then shield := 25;

  result := shield + storm;
end;

function TEvaluator.king_exposure_b(sq: TSquare): TScore;
var
  storm, shield: integer;
  wp, bp, b: TBitboard;
  wr, br: TRank;
  i: integer;
  sf: PBitboard;
begin
  shield := 0;
  storm := 0;

  sf := @ShieldFiles[file_of(sq), 0];
  bp := board^.bb.b_pawns and not InFrontBB[WHITE, rank_of(sq)];
  wp := board^.bb.w_pawns;

  for i := 0 to 2 do begin
    b := bp and sf[i];
    if b <> 0 then
      br := flip(rank_of(BSR(b)))
    else
      br := RANK_1;
    inc(shield, PSHIELD[i, br]);

    b := wp and sf[i];
    if b <> 0 then
      wr := flip(rank_of(BSR(b)))
    else
      wr := RANK_1;
    if integer(wr) - integer(br) = 1 then
      inc(storm, PSTORM[i, wr] div 2)
    else
      inc(storm, PSTORM[i, wr]);
  end;

  if shield = 0 then shield := 25;

  result := shield + storm;
end;

const
  SPACE_MASK_W = $000000003c3c3c00;
  SPACE_MASK_B = $003c3c3c00000000;

procedure TEvaluator.eval_development_w(space_weight: integer);
var
  score: TScore;
  space: integer;
  bb, b: TBitboard;
begin
  score := - pair(NOT_DEVELOPED[undeveloped[WHITE]], 0);
  if space_weight <> 0 then begin
    bb := board^.bb.w_pawns;
    b := SPACE_MASK_W and not(bb or atk.bp or (atk.black and not atk.white));
    bb := bb or bb shr 8;
    bb := bb or bb shr 16;
    space := popcnt_15(b) + popcnt_15(b and bb);
    inc(score, pair(space * space_weight div 8, 0));
  end;
  inc(total, score);
end;

procedure TEvaluator.eval_development_b(space_weight: integer);
var
  score: TScore;
  space: integer;
  bb, b: TBitboard;
begin
  score := - pair(NOT_DEVELOPED[undeveloped[BLACK]], 0);
  if space_weight <> 0 then begin
    bb := board^.bb.b_pawns;
    b := TBB(SPACE_MASK_B) and not(bb or atk.wp or (atk.white and not atk.black));
    bb := bb or TBB(bb shl 8);
    bb := bb or TBB(bb shl 16);
    space := popcnt_15(b) + popcnt_15(b and bb);
    inc(score, pair(space * space_weight div 8, 0));
  end;
  dec(total, score);
end;

procedure TEvaluator.eval_passed_pawns_w();
var
  pp_mask: byte;
  sq, block_sq: TSquare;
  f: TFile;
  r: TRank;
  blocker: TPiece;
  bonus, score: TScorePair;
  path: TBitboard;
  flags: integer;

begin
  score := 0;
  flags := st^.flags;
  pp_mask := ph^.passer_mask[WHITE];
  while pp_mask <> 0 do begin
    f := TFile(lsb_8bit[pp_mask]);
    sq := BSR(board^.bb.w_pawns and FileBB[f]);
    r := rank_of(sq);
    pp_mask := pp_mask and (pp_mask-1);
    if r <= RANK_3 then
      continue;

    bonus := 0;
    block_sq := delta_add(sq, DELTA_N);
    blocker := board^.squares[block_sq];

    path := ForwardBB[WHITE, sq];

    if blocker = NOPIECE then
      inc(bonus, PP_CAN_MOVE[r]);

    if path and board^.bb.white = 0 then
      inc(bonus, PP_CLEAR_OWN[r]);

    if path and board^.bb.black = 0 then
      inc(bonus, PP_CLEAR_OPP[r]);

    if path and (not atk.white) and atk.black = 0 then
      inc(bonus, PP_FREE[r]);

    if flags and FLAG_RVR <> 0 then begin
      if path and board^.bb.w_rooks <> 0 then
        if r >= RANK_6 then begin
          dec(bonus, pair(64, 64));
          if r = RANK_7 then
            dec(bonus, pair(192, 192));
        end;
      if path and board^.bb.w_king <> 0 then
        if BB_CrampFile[file_of(board^.kingsq[WHITE])] and board^.bb.b_rooks <> 0 then
          dec(bonus, pair(0, 2 shl (integer(r)-1)));
    end;

    if flags and FLAG_QVQ <> 0 then begin
      inc(bonus, PP_QVQ_BONUS[r]);
      if (r = RANK_7) and (path and board^.bb.w_queens <> 0) then
        dec(bonus, pair(20, 20));
    end;

    inc(score, bonus);
  end;

  inc(total, score);
end;

procedure TEvaluator.eval_passed_pawns_b();
var
  pp_mask: byte;
  sq, block_sq: TSquare;
  f: TFile;
  r: TRank;
  blocker: TPiece;
  bonus, score: TScorePair;
  path: TBitboard;
  flags: integer;

begin
  score := 0;
  flags := st^.flags;
  pp_mask := ph^.passer_mask[BLACK];
  while pp_mask <> 0 do begin
    f := TFile(lsb_8bit[pp_mask]);
    sq := BSF(board^.bb.b_pawns and FileBB[f]);
    r := rank_of(flip(sq));
    pp_mask := pp_mask and (pp_mask-1);
    if r <= RANK_3 then
      continue;

    bonus := 0;
    block_sq := delta_add(sq, DELTA_S);
    blocker := board^.squares[block_sq];

    path := ForwardBB[BLACK, sq];

    if blocker = NOPIECE then
      inc(bonus, PP_CAN_MOVE[r]);

    if path and board^.bb.black = 0 then
      inc(bonus, PP_CLEAR_OWN[r]);

    if path and board^.bb.white = 0 then
      inc(bonus, PP_CLEAR_OPP[r]);

    if path and ((not atk.black) and atk.white) = 0 then
      inc(bonus, PP_FREE[r]);

    if flags and FLAG_RVR <> 0 then begin
      if path and board^.bb.b_rooks <> 0 then
        if r >= RANK_6 then begin
          dec(bonus, pair(64, 64));
          if r = RANK_7 then
            dec(bonus, pair(192, 192));
        end;
      if path and board^.bb.b_king <> 0 then
        if BB_CrampFile[file_of(board^.kingsq[BLACK])] and board^.bb.w_rooks <> 0 then
          dec(bonus, pair(0, 2 shl (integer(r)-1)));
    end;

    if flags and FLAG_QVQ <> 0 then begin
      inc(bonus, PP_QVQ_BONUS[r]);
      if (r = RANK_7) and (path and board^.bb.b_queens <> 0) then
        dec(bonus, pair(20, 20));
    end;

    inc(score, bonus);
  end;

  dec(total, score);
end;

function TEvaluator.has_legal_moves(): boolean;
begin
  with board^ do
    if us = WHITE then
      result := (atk.wp and bb.black <> 0)
        or ((bb.w_pawns shl 8) and bb.empty <> 0)
        or (atk.wk and not (bb.white or atk.black) <> 0)
    else
      result := (atk.bp and bb.black <> 0)
          or ((bb.b_pawns shr 8) and bb.empty <> 0)
          or (atk.bk and not (bb.black or atk.white) <> 0)
end;

procedure TEvaluator.clear_cache();
begin
  if pawn_hash <> nil then
    fillchar(pawn_hash[0], sizeof(TPawnHash), 0);
  if eval_cache <> nil then
    fillchar(eval_cache[0], sizeof(TEvalCache), 0);
end;

procedure TEvaluator.clear_stats();
begin
  fillchar(stats, sizeof(stats), 0);
end;

procedure TEvaluator.cache_init();
begin
  if pawn_hash = nil then
    pawn_hash := PPawnHash(aligned_malloc(
      PHASH_SIZE * sizeof(TPawnHashEntry), raw_phash));

  if eval_cache = nil then
    eval_cache := PEvalCache(aligned_malloc(
      ECACHE_SIZE * sizeof(TEvalCacheEntry), raw_ecache));

  if mtrl_table = nil then
    mtrl_table := PMtrlTable(aligned_malloc(
      MCACHE_SIZE * sizeof(TMtrlInfo), raw_mcache));
end;

{$ifdef HasPrefetch}
procedure TEvaluator.prefetch(state: PStateInfo);
var
  e: ^TEvalCacheEntry;
  p: ^TPawnHashEntry;
begin
  e := @eval_cache^[cardinal(state^.hashkey) and (ECACHE_SIZE - 1)];
  system.prefetch(e^);
  p := @pawn_hash^[cardinal(state^.pawnkey) and (PHASH_SIZE - 1)];
  system.prefetch(p^);
end;
{$endif}

procedure TEvaluator.cache_destroy();
begin
  if mtrl_table <> nil then begin
    FreeMem(raw_mcache);
    mtrl_table := nil;
  end;
  if eval_cache <> nil then begin
    FreeMem(raw_ecache);
    eval_cache := nil;
  end;
  if pawn_hash <> nil then begin
    FreeMem(raw_phash);
    pawn_hash := nil;
  end;
end;

procedure TEvaluator.uncache(key: TKey);
begin
  eval_cache^[key and (ECACHE_SIZE - 1)] := 0;
end;

procedure init_eval();
var
  i: integer;

  procedure init_eg_fn(id: TEgFnID; fn: pointer; side: TSide = WHITE);
  begin
    eg_fn_tab[id].fn := fn;
    eg_fn_tab[id].side := side;
  end;

begin
  for i := low(SafetyMult) to high(SafetyMult) do
    SafetyMult[i] := min(50, i * i);

  init_eg_fn(EG_KPK, @TEgEval.eg_kpk, WHITE);
  init_eg_fn(EG_KKP, @TEgEval.eg_kpk, BLACK);
  init_eg_fn(EG_KXK, @TEgEval.eg_kxk, WHITE);
  init_eg_fn(EG_KKX, @TEgEval.eg_kxk, BLACK);
  init_eg_fn(EG_KBNK, @TEgEval.eg_kbnk, WHITE);
  init_eg_fn(EG_KKBN, @TEgEval.eg_kbnk, BLACK);
  init_eg_fn(EG_KBKP, @TEgEval.eg_kbkp, WHITE);
  init_eg_fn(EG_KPKB, @TEgEval.eg_kbkp, BLACK);
  init_eg_fn(EG_KRKN, @TEgEval.eg_krkn, WHITE);
  init_eg_fn(EG_KNKR, @TEgEval.eg_krkn, BLACK);
  init_eg_fn(EG_KRKP, @TEgEval.eg_krkp, WHITE);
  init_eg_fn(EG_KPKR, @TEgEval.eg_krkp, BLACK);
  init_eg_fn(EG_KQKP, @TEgEval.eg_kqkp, WHITE);
  init_eg_fn(EG_KPKQ, @TEgEval.eg_kqkp, BLACK);
  init_eg_fn(EG_KBPKB, @TEgEval.eg_kbpkb, WHITE);
  init_eg_fn(EG_KBKBP, @TEgEval.eg_kbpkb, BLACK);
  init_eg_fn(EG_KBPPKB, @TEgEval.eg_kbppkb, WHITE);
  init_eg_fn(EG_KBKBPP, @TEgEval.eg_kbppkb, BLACK);
  init_eg_fn(EG_KBKPP, @TEgEval.eg_kbkpp, WHITE);
  init_eg_fn(EG_KPPKB, @TEgEval.eg_kbkpp, BLACK);
  init_eg_fn(EG_KRPKR, @TEgEval.eg_krpkr, WHITE);
  init_eg_fn(EG_KRKRP, @TEgEval.eg_krpkr, BLACK);
  init_eg_fn(EG_KRPPKRP, @TEgEval.eg_krppkrp, WHITE);
  init_eg_fn(EG_KRPKRPP, @TEgEval.eg_krppkrp, BLACK);
  init_eg_fn(EG_KQKRP, @TEgEval.eg_kqkrp, WHITE);
  init_eg_fn(EG_KRPKQ, @TEgEval.eg_kqkrp, BLACK);
  init_eg_fn(EG_KBBKN, @TEgEval.eg_kbbkn, WHITE);
  init_eg_fn(EG_KNKBB, @TEgEval.eg_kbbkn, BLACK);
  init_eg_fn(EG_KM_W, @TEgEval.eg_minor, WHITE);
  init_eg_fn(EG_KM_B, @TEgEval.eg_minor, BLACK);
  init_eg_fn(EG_KP, @TEgEval.eg_pawn);
  init_eg_fn(EG_SIMPLE_DRAW, @TEgEval.simple_draw);

end;

initialization

init_eval();

end.
