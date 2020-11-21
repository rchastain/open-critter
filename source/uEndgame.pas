
unit uEndgame;

interface

uses
  uBitbase, uBitboard, uBoard, uEval, uMaterial, uPiece, uScore, uSquare;

{$include platform.inc}

type
  TEgEval = object(TEvaluator)
    function simple_draw(side: TSide; var score: TScore): boolean;
    function eg_kpk     (side: TSide; var score: TScore): boolean;
    function eg_kbnk    (side: TSide; var score: TScore): boolean;
    function eg_kxk     (side: TSide; var score: TScore): boolean;
    function eg_krkn    (side: TSide; var score: TScore): boolean;
    function eg_krkp    (side: TSide; var score: TScore): boolean;
    function eg_kbpkb   (side: TSide; var score: TScore): boolean;
    function eg_kbppkb  (side: TSide; var score: TScore): boolean;
    function eg_kbkpp   (side: TSide; var score: TScore): boolean;
    function eg_kbkp    (side: TSide; var score: TScore): boolean;
    function eg_kqkrp   (side: TSide; var score: TScore): boolean;
    function eg_kqkp    (side: TSide; var score: TScore): boolean;
    function eg_kbbkn   (side: TSide; var score: TScore): boolean;
    function eg_kpkp    (side: TSide; var score: TScore): boolean;
    function eg_krpkr   (side: TSide; var score: TScore): boolean;
    function eg_krppkrp (side: TSide; var score: TScore): boolean;
    function eg_minor   (side: TSide; var score: TScore): boolean;
    function eg_pawn    (side: TSide; var score: TScore): boolean;
    function kppk_draw_w(): boolean;
    function kppk_draw_b(): boolean;
  end;

const
  BB_G7H6: TBitboardPair    = (TBB($0040800000000000), TBB($0000000000804000));
  BB_A6B7: TBitboardPair    = (TBB($0002010000000000), TBB($0000000000010200));
  BB_F8G8: TBitboardPair    = (TBB($6000000000000000), TBB($0000000000000060));
  BB_B8C8: TBitboardPair    = (TBB($0600000000000000), TBB($0000000000000006));
  BB_ABC8AC7: TBitboardPair = (TBB($0705000000000000), TBB($0000000000000507));
  BB_FGH8FH7: TBitboardPair = (TBB($e0a0000000000000), TBB($000000000000a0e0));

implementation

const
  KBNK_MATE: array [TSquare] of byte = (
    105,  90,  75,  60,  45,  30,  15,   0,
     90,  75,  60,  45,  30,  15,   0,  15,
     75,  60,  45,  30,  15,   0,  15,  30,
     60,  45,  30,  15,   0,  15,  30,  45,
     45,  30,  15,   0,  15,  30,  45,  60,
     30,  15,   0,  15,  30,  45,  60,  75,
     15,   0,  15,  30,  45,  60,  75,  90,
      0,  15,  30,  45,  60,  75,  90, 105
  );

  KING_EDGE: array [TSquare] of byte = (
    100,  90,  80,  70,  70,  80,  90, 100,
     90,  70,  60,  50,  50,  60,  70,  90,
     80,  60,  40,  30,  30,  40,  60,  80,
     70,  50,  30,  20,  20,  30,  50,  70,
     70,  50,  30,  20,  20,  30,  50,  70,
     80,  60,  40,  30,  30,  40,  60,  80,
     90,  70,  60,  50,  50,  60,  70,  90,
    100,  90,  80,  70,  70,  80,  90, 100
  );

  KK_TROPISM: array [0 .. 7] of byte = (0, 0, 100, 80, 60, 40, 20, 10);

function TEgEval.simple_draw(side: TSide; var score: TScore): boolean;
var
  stm: TSide;
  safe_squares: TBitboard;
begin
  board^.update_state();
  result := true;
  score := SCORE_ZERO;

  if st^.phase.total = 1 then begin
    st^.flags := st^.flags or FLAG_DRAW;
    exit;
  end;

  if st^.checkersBB <> 0 then begin
    stm := board^.us;
    safe_squares := not (st^.attacks[flip(stm)] or board^.bb.pieces[stm]);
    if KingStep[board^.kingsq[stm]] and safe_squares = 0 then begin
      score := mated_in(board^.ply);
      st^.flags := st^.flags or FLAG_MATE_IN_1;
      exit;
    end;
  end;

  result := true;
end;

function kpk_draw(wksq, bksq, psq: TSquare; stm: TSide): boolean;
begin
  if file_of(psq) >= FILE_E then
  begin
    wksq := mirror(wksq);
    bksq := mirror(bksq);
    psq := mirror(psq);
  end;
  result := not probe_kpk(wksq, bksq, psq, stm);
end;

function TEgEval.eg_kpk(side: TSide; var score: TScore): boolean;
var
  wksq, bksq, psq: TSquare;
begin
  result := true;
  with board^ do begin
    wksq := relative(kingsq[side], side);
    bksq := relative(kingsq[flip(side)], side);
    psq := relative(find_lsb(bb.pawns[side]), side);
  end;

  if kpk_draw(wksq, bksq, psq, TSide(cardinal(board^.us) xor cardinal(side))) then
  begin
    score := SCORE_ZERO;
    st^.flags := st^.flags or FLAG_DRAW;
  end
  else
    score := ($600 + integer(rank_of(psq)) * 8) * sign[side];
end;

function TEgEval.eg_kxk(side: TSide; var score: TScore): boolean;
var
  xside: TSide;
  wksq, bksq: TSquare;
  b: TBitboard;
  bishop_pair: boolean;
begin
  result := true;
  xside := flip(side);

  with board^ do begin
    wksq := kingsq[side];
    bksq := kingsq[xside];
    update_state();
  end;

  if st^.mtrlkey = mkey_knnk[side] then begin
    score := SCORE_ZERO;
    if (board^.us = xside) or (KingStep[bksq] and not st^.attacks[side] <> 0) then
      if st^.checkersBB <> 0 then
        st^.flags := st^.flags or FLAG_DRAW;
    exit;
  end;

  if (board^.us = xside) then begin
    if (st^.checkersBB = 0) then
      if KingStep[bksq] and not st^.attacks[side] = 0 then begin
        score := SCORE_ZERO;
        st^.flags := st^.flags or FLAG_DRAW;
        exit;
      end;

    if board^.count.np[side] + board^.count.pawn[side] = 1 then begin
      b := board^.bb.pieces[side] xor board^.bb.king[side];
      if (KingStep[bksq] and b <> 0) and (st^.attacks[side] and b = 0) then begin
        score := SCORE_ZERO;
        st^.flags := st^.flags or FLAG_DRAW;
        exit;
      end;
    end;
  end;

  if side = WHITE then
    bishop_pair := st^.bishop_flags and BF_WHITE_PAIR = BF_WHITE_PAIR
  else
    bishop_pair := st^.bishop_flags and BF_BLACK_PAIR = BF_BLACK_PAIR;

  if (board^.count.pawn[side] = 0) and (board^.count.np[side] = 2) then
    if (board^.count.bishop[side] = 2) and not bishop_pair then
    begin
      score := SCORE_ZERO;
      st^.flags := st^.flags or FLAG_DRAW;
      result := true;
      exit;
    end;
  
  score := KING_EDGE[bksq] + KK_TROPISM[Distance[bksq, wksq]];
  score := score * sign[side];
  inc(score, mi^.mtrl_score);
end;

const
  krkn: array [0 .. 7] of shortint = (0, 0, 5, 10, 20, 30, 40, 80);

function TEgEval.eg_krkn(side: TSide; var score: TScore): boolean;
var
  xside: TSide;
  ksq, nsq: TSquare;
begin
  result := true;
  xside := flip(side);
  ksq := board^.kingsq[xside];
  nsq := TSquare(board^.piece_list.knights[xside, 0]);
  score := (KING_EDGE[ksq] + krkn[Distance[ksq, nsq]]) * sign[side];
end;

function TEgEval.eg_krkp(side: TSide; var score: TScore): boolean;
var
  wksq, bksq, rsq, psq, qsq: TSquare;
  xside: TSide;
  tempo: integer;
begin
  xside := flip(side);
  with board^ do begin
    wksq := relative(kingsq[side], side);
    bksq := relative(kingsq[xside], side);
    rsq := relative(TSquare(piece_list.rooks[side, 0]), side);
    psq := relative(find_lsb(bb.pawns[xside]), side);
    qsq := square(RANK_1, file_of(psq));
    tempo := integer(us = side);
  end;

  if ((file_of(wksq) = file_of(psq)) and (rank_of(wksq) < rank_of(psq))) or
    ((Distance[bksq, psq] - (1 - tempo) > 2) and (Distance[bksq, rsq] > 2)) then
    score := ( {EG_VROOK} $5B2 - Distance[wksq, psq] * 8)
  else if (rank_of(psq) < RANK_4) and (Distance[bksq, psq] = 1) and
    (rank_of(wksq) > RANK_3) and (Distance[wksq, psq] - tempo > 2) then
    score := 80 - Distance[wksq, psq] * 8
  else
    score := 200 - Distance[wksq, delta_add(psq, DELTA_S)] * 8
      + Distance[bksq, delta_add(psq, DELTA_S)] * 8
      + Distance[psq, qsq] * 8;

  score := score * sign[side];
  result := true;
end;

const
  bb_kxn = $0042000000004200;

function TEgEval.eg_kbnk(side: TSide; var score: TScore): boolean;
var
  xside: TSide;
  wksq, bksq, bsq, nsq, sq: TSquare;
  katt, bb: TBitboard;
  a: boolean;
begin
  xside := flip(side);
  with board^ do begin
    wksq := kingsq[side];
    bksq := kingsq[xside];
    bsq := TSquare(piece_list.bishops[side, 0]);
    nsq := TSquare(piece_list.knights[side, 0]);
    update_state();
  end;

  mi^.flags := mi^.flags or FLAG_NO_UPDATE;

  if board^.us = xside then begin
    katt := KingStep[bksq];
    if (katt and SquareBB[bsq] <> 0) and (st^.attacks[side] and SquareBB[bsq] = 0) or
      ((katt and SquareBB[nsq] <> 0) and (st^.attacks[side] and SquareBB[nsq] = 0) and
      not ((st^.checkersBB and board^.bb.bishops[side] <> 0) and
      (Direction[bsq, bksq] = Direction[bksq, nsq]))) then
    begin
      score := SCORE_ZERO;
      result := true;
      exit;
    end;

    if (st^.checkersBB = 0) and (katt and not st^.attacks[side] = 0) then
    begin
      score := SCORE_ZERO;
      result := true;
      exit;
    end;
  end
  else begin
    if (Distance[bksq, bsq] = 1) and (Distance[bksq, nsq] = 1) then
      if (Distance[wksq, nsq] > 2) and (Distance[wksq, bsq] >= 2) or
        (Distance[wksq, nsq] >= 2) and (Distance[wksq, bsq] > 2) then
        if (KnightStep[nsq] and SquareBB[bsq] = 0) or (Distance[wksq, nsq] > 2) then
        begin
          bb := KnightStep[nsq] and KnightStep[bsq];
          a := true;
          while bb <> 0 do begin
            sq := find_lsb(bb);
            if (Distance[sq, wksq] < 2) or (Distance[sq, wksq] = 2) and
              (Distance[sq, bksq] > 1) then
            begin
              a := false;
              break;
            end;
            clear_lsb(bb);
          end;
          if a then
            if (DiagonalBB[bsq] and SquareBB[nsq] = 0) or (SquareBB[bsq] and bb_edge <> 0) then
            begin
              score := SCORE_ZERO;
              result := true;
              exit;
            end;
        end;

    if (SquareBB[nsq] and bb_corners <> 0)
      and (SquareBB[bksq] and bb_kxn <> 0)
      and (Distance[wksq, nsq] > 3)
      and (Distance[bksq, nsq] = 1)
      and (board^.bishop_attacks(bsq) and board^.bishop_attacks(bksq) and not KingStep[bksq] = 0)
      and (board^.bishop_attacks(bsq) and KnightStep[nsq] = 0) then
    begin
      score := SCORE_ZERO;
      result := true;
      exit;
    end;
  end;

  if board^.bb.bishops[side] and BB_Light <> 0 then
  begin
    wksq := mirror(wksq);
    bksq := mirror(bksq);
  end;

  score := (Distance[wksq, bksq] + KBNK_MATE[bksq]) * sign[side]
    + mi^.mtrl_score + st^.psqval;
  result := true;
end;

function TEgEval.eg_kqkrp(side: TSide; var score: TScore): boolean;
var
  xside: TSide;
  wksq, bksq: TSquare;
  pawns, patk: TBitboard;
begin
  result := false;
  xside := flip(side);

  with board^ do begin
    wksq := kingsq[side];
    bksq := kingsq[xside];
    pawns := bb.pawns[xside];
  end;

  if (rank_of(relative(bksq, xside)) <= RANK_2) and
    (rank_of(relative(wksq, xside)) >= RANK_4) and
    (board^.bb.rooks[xside] and RankBB[relative_rank(RANK_3, xside)] <> 0)
    and (pawns and RankBB[relative_rank(RANK_2, xside)] <> 0) and
    (KingStep[bksq] and pawns <> 0) then
  begin
    if side = WHITE then
      patk := pawn_attk_b(pawns)
    else
      patk := pawn_attk_w(pawns);
    if patk and board^.bb.rooks[xside] <> 0 then begin
      score := SCORE_ZERO;
      result := true;
    end;
  end;
end;

function TEgEval.eg_kbpkb(side: TSide; var score: TScore): boolean;
var
  xside: TSide;
  psq, ksq, bbsq, wbsq: TSquare;
  b, pawn_path: TBitboard;
  opposite_bishops: boolean;
begin
  result := true;
  xside := flip(side);

  with board^ do begin
    bbsq := relative(TSquare(piece_list.bishops[xside, 0]), side);
    wbsq := relative(TSquare(piece_list.bishops[side, 0]), side);
    psq := relative(find_lsb(bb.pawns[side]), side);
    ksq := relative(kingsq[xside], side);
  end;

  opposite_bishops := st^.bishop_flags in [BF_OPPOSITE_1, BF_OPPOSITE_2];

  pawn_path := ForwardBB[WHITE, psq];
  if pawn_path and SquareBB[ksq] <> 0 then
    if opposite_bishops or (rank_of(ksq) <= RANK_6) or
      (sq_color(ksq) <> sq_color(wbsq)) then
    begin
      score := SCORE_ZERO;
      exit;
    end;

  if opposite_bishops then begin
    if rank_of(psq) <= RANK_5 then begin
      score := SCORE_ZERO;
      exit;
    end;
    b := board^.bishop_attacks(bbsq);
    if (b and pawn_path <> 0) and (Distance[bbsq, psq] >= 3) then begin
      score := SCORE_ZERO;
      exit;
    end;
  end;

  result := false;
end;

function TEgEval.eg_kqkp(side: TSide; var score: TScore): boolean;
var
  stm: TSide;
  psq, qsq, wksq, bksq: TSquare;
const
  A2C2 = (TBB(1) shl ord(A2)) or (TBB(1) shl ord(C2));
begin
  result := false;

  with board^ do begin
    wksq := kingsq[side];
    bksq := kingsq[flip(side)];
    qsq := TSquare(piece_list.queens[side, 0]);
    psq := find_lsb(bb.pawns[flip(side)]);
    stm := us;
  end;

  if file_of(psq) >= FILE_D then
  begin
    wksq := mirror(wksq);
    qsq := mirror(qsq);
    bksq := mirror(bksq);
    psq := mirror(psq);
  end;

  if side <> WHITE then
  begin
    wksq := flip(wksq);
    qsq := flip(qsq);
    bksq := flip(bksq);
    psq := flip(psq);
    stm := flip(stm);
  end;

  if (SquareBB[psq] and A2C2 = 0) or (rank_of(bksq) > RANK_4) then
    exit;

  if not probe_kqkp(wksq, bksq, qsq, psq, stm) then
    exit;

  st^.flags := st^.flags or FLAG_DRAW;
  score := SCORE_ZERO;
  result := true;
end;

function TEgEval.eg_kbbkn(side: TSide; var score: TScore): boolean;
var
  xside: TSide;
  bishop_pair: boolean;
  wksq, bksq, nsq: TSquare;
begin
  result := true;
  xside := flip(side);

  if side = WHITE then
    bishop_pair := st^.bishop_flags and BF_WHITE_PAIR = BF_WHITE_PAIR
  else
    bishop_pair := st^.bishop_flags and BF_BLACK_PAIR = BF_BLACK_PAIR;

  if not bishop_pair then begin
    score := SCORE_ZERO;
    st^.flags := st^.flags or FLAG_DRAW;
    exit;
  end;

  with board^ do begin
    wksq := kingsq[side];
    bksq := kingsq[xside];
    nsq := find_lsb(bb.knights[xside]);
  end;

  score := sign[side] * (
    KK_TROPISM[Distance[wksq, bksq]] +
    32 * Distance[wksq, nsq] +
    8 * (8 - popcnt_15(KnightStep[nsq] and not board^.bb.all[xside])) + $362);
end;

function eval_kpkp(var board: TBoard; mi: PMtrlInfo; out eval: TScore;
  var scale: TScales): boolean;
var
  wksq, bksq, wpsq, bpsq, qsq: TSquare;
  r_wksq, r_bksq, r_psq: TSquare;
begin
  result := false;

  wpsq := find_lsb(board.bb.w_pawns);
  bpsq := find_lsb(board.bb.b_pawns);

  if Distance[wpsq, bpsq] = 1 then
    exit;

  wksq := board.kingsq[WHITE];
  bksq := board.kingsq[BLACK];

  qsq := square(RANK_8, file_of(wpsq));
  if not is_set(BetweenBB[qsq, bksq], bpsq) then
  begin
    r_wksq := wksq;
    r_bksq := bksq;
    r_psq := wpsq;
    if file_of(r_psq) > FILE_D then
    begin
      r_wksq := mirror(r_wksq);
      r_bksq := mirror(r_bksq);
      r_psq := mirror(r_psq);
    end;
    if (rank_of(r_psq) < RANK_6) then
      if not probe_kpk(r_wksq, r_bksq, r_psq, board.us) then
        scale[WHITE] := SCALE_ZERO;
  end;

  qsq := square(RANK_1, file_of(bpsq));
  if not is_set(BetweenBB[qsq, wksq], wpsq) then
  begin
    r_wksq := flip(bksq);
    r_bksq := flip(wksq);
    r_psq := flip(bpsq);
    if file_of(r_psq) > FILE_D then
    begin
      r_wksq := mirror(r_wksq);
      r_bksq := mirror(r_bksq);
      r_psq := mirror(r_psq);
    end;
    if rank_of(r_psq) < RANK_6 then
      if not probe_kpk(r_wksq, r_bksq, r_psq, board.they) then
        scale[BLACK] := SCALE_ZERO;
  end;
end;

function TEgEval.eg_krpkr(side: TSide; var score: TScore): boolean;
var
  xside: TSide;
  wksq, bksq, wrsq, brsq: TSquare;
  psq, block_sq, qsq: TSquare;
  tempo: integer;
  fd: integer;
  pfile: TFile;
  rr: TRank;
begin
  result := true;
  xside := flip(side);

  with board^ do begin
    wksq := kingsq[side];
    bksq := kingsq[xside];
    wrsq := TSquare(piece_list.rooks[side, 0]);
    brsq := TSquare(piece_list.rooks[xside, 0]);
    psq := find_lsb(bb.pawns[side]);
  end;

  if file_of(psq) > FILE_D then
  begin
    wksq := mirror(wksq);
    bksq := mirror(bksq);
    wrsq := mirror(wrsq);
    brsq := mirror(brsq);
    psq := mirror(psq);
  end;

  if side = BLACK then
  begin
    wksq := flip(wksq);
    bksq := flip(bksq);
    wrsq := flip(wrsq);
    brsq := flip(brsq);
    psq := flip(psq);
  end;

  pfile := file_of(psq);
  rr := rank_of(psq);
  qsq := square(RANK_8, pfile);
  tempo := integer(board^.us = side);
  block_sq := delta_add(psq, DELTA_N);

  if (rr < RANK_6)
    and (Distance[bksq, qsq] <= 1)
    and (rank_of(wksq) < RANK_6)
    and ((rank_of(brsq) = RANK_6) or ((rr <= RANK_3) and (rank_of(wrsq) <> RANK_6))) then
  begin
    score := SCORE_ZERO;
    exit;
  end;

  if (rr = RANK_6)
    and (Distance[bksq, qsq] <= 1)
    and (integer(rank_of(wksq)) + tempo <= integer(RANK_6))
    and ((rank_of(brsq) = RANK_1) or ((tempo = 0)
    and (abs(integer(file_of(brsq)) - integer(pfile)) >= 3))) then
  begin
    score := SCORE_ZERO;
    exit;
  end;

  if (rr >= RANK_6)
    and (bksq = qsq)
    and (rank_of(brsq) = RANK_1)
    and ((tempo = 0) or (Distance[wksq, psq] >= 2)) then
  begin
    score := SCORE_ZERO;
    exit;
  end;

  if (rr < RANK_6)
    and (bksq = block_sq)
    and (Distance[wksq, psq] - tempo >= 2)
    and (Distance[wksq, brsq] - tempo >= 2) then
  begin
    score := SCORE_ZERO;
    exit;
  end;

  if (psq = A7)
    and (wrsq = A8)
    and ((bksq = H7) or (bksq = G7))
    and (file_of(brsq) = FILE_A)
    and ((rank_of(brsq) < RANK_4) or (file_of(wksq) >= FILE_D) or (rank_of(wksq) < RANK_6)) then
  begin
    score := SCORE_ZERO;
    exit;
  end;

  if (rr < RANK_6)
    and (bksq = block_sq)
    and (Distance[wksq, psq] - tempo >= 2)
    and (Distance[wksq, brsq] - tempo >= 2) then
  begin
    score := SCORE_ZERO;
    exit;
  end;

  if (rr = RANK_7)
    and (pfile <> FILE_A)
    and (file_of(wrsq) = pfile)
    and (wrsq <> qsq)
    and (Distance[wksq, qsq] < Distance[bksq, qsq] - 2 + tempo)
    and (Distance[wksq, qsq] < Distance[bksq, wrsq] + tempo) then
  begin
    score := SCORE_ZERO;
    exit;
  end;

  if (pfile <> FILE_A)
    and (file_of(wrsq) = pfile)
    and (wrsq < psq)
    and (Distance[wksq, qsq] < Distance[bksq, qsq] - 2 + tempo)
    and (Distance[wksq, block_sq] < Distance[bksq, block_sq] - 2 + tempo)
    and ((Distance[bksq, wrsq] + tempo >= 3)
          or (Distance[wksq, qsq] < distance[bksq, wrsq] + tempo)
              and (Distance[wksq, block_sq] < Distance[bksq, wrsq] + tempo)) then
  begin
    score := SCORE_ZERO;
    exit;
  end;

  if (rr <= RANK_4) and (bksq > wksq) then begin
    fd := file_distance(bksq, psq);
    if fd = 0 then
      score := (score * 10) div 64;
    if (fd = 1) and (Distance[wksq, bksq] > 2) then
      score := (score * (24 - 2 * Distance[wksq, bksq])) div 64;
  end;

  result := false;
end;

function TEgEval.eg_kpkp(side: TSide; var score: TScore): boolean;
var
  xside: TSide;
  wksq, bksq, wpsq: TSquare;
begin
  wpsq := relative(find_lsb(board^.bb.pawns[side]), side);
  result := false;
  if rank_of(wpsq) >= RANK_5 then
    if board^.bb.pawns[side] and (BB_FileA or BB_FileH) = 0 then
      exit;

  xside := flip(side);
  wksq := relative(board^.kingsq[side], side);
  bksq := relative(board^.kingsq[xside], side);

  if kpk_draw(wksq, bksq, wpsq, TSide(cardinal(board^.us) xor cardinal(side))) then
  begin
    score := SCORE_ZERO;
    result := true;
  end;
end;

function TEgEval.kppk_draw_w(): boolean;
const
  BB_B8C8 = TBB($0600000000000000);
  BB_F8G8 = TBB($6000000000000000);
var
  p, k: TBitboard;
  ksq: TSquare;
begin
  result := true;
  p := board^.bb.w_pawns;

  if p = 0 then exit;

  ksq := board^.kingsq[BLACK];
  if p and not BB_FileA = 0 then begin
    if st^.w_attacks and board^.bb.b_pawns = 0 then begin
      if Distance[A8, ksq] <= 1 then exit;
      if (file_of(ksq) = FILE_A) and (InFrontBB[WHITE, rank_of(ksq)] and p = 0) then exit;
    end;
  end
  else if p and not BB_FileH = 0 then begin
    if st^.w_attacks and board^.bb.b_pawns = 0 then begin
      if Distance[H8, ksq] <= 1 then exit;
      if (file_of(ksq) = FILE_H) and (InFrontBB[WHITE, rank_of(ksq)] and p = 0) then exit;
    end;
  end
  else if p and not BB_FileB = 0 then begin
    if (board^.squares[B6] = W_PAWN) and (board^.squares[B7] = B_PAWN) then begin
      k := KingStep[ksq] or board^.bb.b_king;
      if k and BB_B8C8 <> 0 then
        if Distance[B7, ksq] > 1 then exit;
    end;
  end
  else if p and not BB_FileG = 0 then begin
    if (board^.squares[G6] = W_PAWN) and (board^.squares[G7] = B_PAWN) then begin
      k := KingStep[ksq] or board^.bb.b_king;
      if k and BB_F8G8 <> 0 then
        if Distance[G7, ksq] > 1 then exit;
    end;
  end;
  result := false;
end;

function TEgEval.kppk_draw_b(): boolean;
const
  BB_B1C1 = TBB($0000000000000006);
  BB_F1G1 = TBB($0000000000000060);
var
  p, k: TBitboard;
  ksq: TSquare;
begin
  result := true;
  p := board^.bb.b_pawns;

  if p = 0 then exit;

  ksq := board^.kingsq[WHITE];
  if p and not BB_FileA = 0 then begin
    if st^.b_attacks and board^.bb.w_pawns = 0 then begin
      if Distance[A1, ksq] <= 1 then exit;
      if (file_of(ksq) = FILE_A) and (InFrontBB[BLACK, rank_of(ksq)] and p = 0) then exit;
    end;
  end
  else if p and not BB_FileH = 0 then begin
    if st^.w_attacks and board^.bb.b_pawns = 0 then begin
      if Distance[H8, ksq] <= 1 then exit;
      if (file_of(ksq) = FILE_H) and (InFrontBB[BLACK, rank_of(ksq)] and p = 0) then exit;
    end;
  end
  else if p and not BB_FileB = 0 then begin
    if (board^.squares[B3] = B_PAWN) and (board^.squares[B2] = W_PAWN) then begin
      k := KingStep[ksq] or board^.bb.w_king;
      if k and BB_B1C1 <> 0 then
        if Distance[B2, ksq] > 1 then exit;
    end;
  end
  else if p and not BB_FileG = 0 then begin
    if (board^.squares[G3] = B_PAWN) and (board^.squares[G2] = W_PAWN) then begin
      k := KingStep[ksq] or board^.bb.w_king;
      if k and BB_F1G1 <> 0 then
        if Distance[G2, ksq] > 1 then exit;
    end;
  end;
  result := false;
end;

function TEgEval.eg_pawn(side: TSide; var score: TScore): boolean;
var
  w, e, chk, occ: TBitboard;
  advanced: array [TSide] of cardinal;
  mult: integer;
  sq, qsq: TSquare;
  unstoppable: TScore;
  f: TFile;
  rr: TRank;
  pp_mask, stm: cardinal;
  s: PStateInfo;
begin
  result := true;
  get_pawn_info();
  s := st;
  with s^ do begin
    checkersBB := 0;
    pins[WHITE] := 0;
    pins[BLACK] := 0;
    w_attacks := KingStep[board^.kingsq[WHITE]];
    b_attacks := KingStep[board^.kingsq[BLACK]];
    atk.wk := w_attacks;
    atk.bk := b_attacks;
  end;

  if board^.bb.w_pawns or board^.bb.b_pawns = 0 then begin
    s^.flags := FLAG_DRAW;
    score := SCORE_ZERO;
    exit;
  end;

  stm := cardinal(board^.us);

  if stm = cardinal(WHITE) then begin
    atk.wp := pawn_attk_w(board^.bb.w_pawns);
    w := pawn_west_b(board^.bb.b_pawns);
    chk := w and board^.bb.w_king;
    if chk <> 0 then s^.checkersBB := s^.checkersBB or pawn_east_w(chk);
    e := pawn_east_b(board^.bb.b_pawns);
    chk := e and board^.bb.w_king;
    if chk <> 0 then s^.checkersBB := s^.checkersBB or pawn_west_w(chk);
    atk.bp := w or e;
  end
  else begin
    atk.bp := pawn_attk_b(board^.bb.b_pawns);
    w := pawn_west_w(board^.bb.w_pawns);
    chk := w and board^.bb.b_king;
    if chk <> 0 then s^.checkersBB := s^.checkersBB or pawn_east_b(chk);
    e := pawn_east_w(board^.bb.w_pawns);
    chk := e and board^.bb.b_king;
    if chk <> 0 then s^.checkersBB := s^.checkersBB or pawn_west_b(chk);
    atk.wp := w or e;
  end;

  with s^ do begin
    w_attacks := w_attacks or atk.wp;
    b_attacks := b_attacks or atk.bp;
    atk.white := w_attacks;
    atk.black := b_attacks;
  end;

  if s^.epsq <> NO_EP then
    if not has_legal_moves() then
      if s^.checkersBB = 0 then begin
        s^.flags := s^.flags or FLAG_DRAW;
        score := SCORE_ZERO;
        exit;
      end
      else begin
        s^.flags := s^.flags or FLAG_MATE;
        score := mated_in(board^.ply);
        exit;
      end;

  score := mi^.mtrl_score + eg(ph^.pawn_score) + eg(st^.psqval);

  if score > 0 then
    score := mi^.scale[WHITE] * score div 128
  else
    score := mi^.scale[BLACK] * score div 128;

  pp_mask := ph^.passer_mask[WHITE];
  advanced[WHITE] := 0;
  while (pp_mask <> 0) do begin
    f := file_of(TSquare(lsb_8bit[pp_mask]));
    pp_mask := pp_mask and (pp_mask - 1);
    sq := BSR(board^.bb.w_pawns and FileBB[f]);
    rr := rank_of(sq);
    qsq := square(RANK_8, f);
    if   (board^.bb.w_king and Shepherd[WHITE, sq] <> 0)
      or (Distance[qsq, board^.kingsq[BLACK]] > Distance[qsq, sq] + stm) then
    begin
      if ForwardBB[WHITE, sq] and board^.bb.white <> 0 then
        dec(rr);
      occ := board^.bb.occupied xor SquareBB[sq];
      if queen_attacks(qsq, occ) and board^.bb.b_king <> 0 then
        inc(rr);
      if cardinal(rr) > advanced[WHITE] then
        advanced[WHITE] := cardinal(rr);
    end;
  end;

  pp_mask := ph^.passer_mask[BLACK];
  advanced[BLACK] := 0;
  while (pp_mask <> 0) do begin
    f := file_of(TSquare(lsb_8bit[pp_mask]));
    pp_mask := pp_mask and (pp_mask - 1);
    sq := BSF(board^.bb.b_pawns and FileBB[f]);
    rr := rank_of(flip(sq));
    qsq := square(RANK_1, f);
    if   (board^.bb.b_king and Shepherd[BLACK, sq] <> 0)
      or (Distance[qsq, board^.kingsq[WHITE]] > Distance[qsq, sq] + 1 - stm) then
    begin
      if ForwardBB[BLACK, sq] and board^.bb.black <> 0 then
        dec(rr);
      occ := board^.bb.occupied xor SquareBB[sq];
      if queen_attacks(qsq, occ) and board^.bb.w_king <> 0 then
        inc(rr);
      if cardinal(rr) > advanced[BLACK] then
        advanced[BLACK] := cardinal(rr);
    end;
  end;

  if advanced[WHITE] > advanced[BLACK] then begin
    mult := advanced[WHITE] + (stm xor 1);
    unstoppable := $300 + $80 * mult;
    if board^.bb.b_pawns and InFrontBB[BLACK, TRank(8 - mult)] <> 0 then
      if ph^.passer_mask[BLACK] and (ph^.passer_mask[BLACK] - 1) <> 0 then
        unstoppable := unstoppable shr 1;
    inc(score, unstoppable);
  end;

  if advanced[BLACK] > advanced[WHITE] then begin
    mult := advanced[BLACK] + stm;
    unstoppable := $300 + $80 * mult;
    if board^.bb.w_pawns and InFrontBB[WHITE, TRank(mult - 1)] <> 0 then
      if ph^.passer_mask[WHITE] and (ph^.passer_mask[WHITE] - 1) <> 0 then
        unstoppable := unstoppable shr 1;
    dec(score, unstoppable);
  end;

  if score > 0 then begin
    if kppk_draw_w() then begin
      s^.tempo := 0;
      score := SCORE_ZERO;
      exit;
    end;
    if (board^.count.wp = 1) and (board^.count.bp = 1) then
      eg_kpkp(WHITE, score);
  end;

  if score < 0 then begin
    if kppk_draw_b() then begin
      s^.tempo := 0;
      score := SCORE_ZERO;
      exit;
    end;
    if (board^.count.wp = 1) and (board^.count.bp = 1) then
      eg_kpkp(BLACK, score);
  end;
end;

function TEgEval.eg_minor(side: TSide; var score: TScore): boolean;
var
  xside, stm: TSide;
  psq, bsq, nsq, wksq, bksq, chksq: TSquare;
  pawns, bkzone, knights, bishops, b: TBitboard;
  br, wr: TRank;
begin
  result := true;
  xside := flip(side);
  pawns := board^.bb.pawns[side];

  if pawns = 0 then begin
    score := SCORE_ZERO;
    exit;
  end;

  psq := TSquare(0);
  wksq := board^.kingsq[side];
  bksq := board^.kingsq[xside];

  if pawns and (pawns-1) = 0 then begin
    psq := find_lsb(pawns);
    if st^.mtrlkey = mkey_kbpkn[side] then begin
      bsq := TSquare(board^.piece_list.bishops[side, 0]);
      if (bksq = delta_add(psq, PawnPush[side])) then
        if (sq_color(bksq) <> sq_color(bsq)) then begin
          score := SCORE_ZERO;
          if (board^.us = side) then begin
            if atk.all[side] and board^.bb.knights[xside] = 0 then
              if atk.knight[xside] and not atk.all[side] <> 0 then begin
                nsq := TSquare(board^.piece_list.knights[side, 0]);
                if sq_color(nsq) <> sq_color(bsq) then
                  st^.flags := st^.flags or FLAG_DRAW;
              end;
          end
          else begin
            if atk.knight[xside] and not atk.all[side] <> 0 then
              st^.flags := st^.flags or FLAG_DRAW;
          end;
        end;
    end;

    if (board^.us <> side) and (pawns and atk.king[xside] and not atk.all[side] <> 0) then
    begin
      if st^.checkersBB = 0 then begin
        score := SCORE_ZERO;
        exit;
      end;
      chksq := find_lsb(st^.checkersBB);
      if Direction[chksq, bksq] <> Direction[bksq, psq] then begin
        score := SCORE_ZERO;
        exit;
      end;
    end;
  end;

  knights := board^.bb.knights[side];
  if knights <> 0 then begin

    if psq = relative(A7, side) then begin
      if Distance[relative(A8, side), bksq] <= 1 then begin
        result := false;
        if board^.us = side then begin
          if atk.knight[side] and SquareBB[relative(A8, side)] <> 0 then exit;
          if (Distance[wksq, psq] <= 2)
            and (atk.knight[side] and SquareBB[relative(C7, side)] <> 0)
            and (wksq <> relative(C8, side)) then exit;
        end
        else begin
          if Distance[wksq, psq] <= 1 then begin
            if (bksq = relative(B8, side)) then
              if (knights and atk.knight[side] and SquareBB[relative(C7, side)] <> 0) then
                exit;
            if (bksq = relative(A8, side)) then
              if (atk.knight[side] and SquareBB[relative(C7, side)] <> 0) then
                exit;
          end;
        end;
        score := SCORE_ZERO;
        result := true;
        exit;
      end;
      if wksq = relative(A8, side) then begin
        if bksq = relative(C7, side) then begin
          stm := TSide(knights and BB_Light = 0);
          if board^.us <> stm then begin
            score := SCORE_ZERO;
            result := true;
            exit;
          end;
        end;
        if bksq = relative(C8, side) then begin
          stm := TSide(knights and BB_Light = 0);
          if board^.us = stm then begin
            score := SCORE_ZERO;
            result := true;
            exit;
          end;
        end;
      end;
    end;

    if psq = relative(H7, side) then
      if Distance[relative(H8, side), bksq] <= 1 then begin
        result := false;
        if board^.us = side then begin
          if atk.knight[side] and SquareBB[relative(H8, side)] <> 0 then exit;
          if (Distance[wksq, psq] <= 2)
            and (atk.knight[side] and SquareBB[relative(F7, side)] <> 0)
            and (wksq <> relative(F8, side)) then exit;
        end
        else begin
          if Distance[wksq, psq] <= 1 then begin
            if (bksq = relative(G8, side)) then
              if (knights and atk.knight[side] and SquareBB[relative(F7, side)] <> 0) then
                exit;
            if (bksq = relative(H8, side)) then
              if (atk.knight[side] and SquareBB[relative(F7, side)] <> 0) then
                exit;
          end;
        end;
        score := SCORE_ZERO;
        result := true;
        exit;
      end;
      if wksq = relative(H8, side) then begin
        if bksq = relative(F7, side) then begin
          stm := TSide(knights and BB_Light = 0);
          if board^.us <> stm then begin
            score := SCORE_ZERO;
            result := true;
            exit;
          end;
        end;
        if bksq = relative(F8, side) then begin
          stm := TSide(knights and BB_Light = 0);
          if board^.us = stm then begin
            score := SCORE_ZERO;
            result := true;
            exit;
          end;
        end;
      end;
    result := false;
    exit;
  end;

  bkzone := board^.bb.king[xside] or atk.king[xside];
  bishops := board^.bb.bishops[side];
  if bishops and ColorBB[side] <> 0 then begin

    if pawns and not BB_FileH = 0 then
      if Distance[relative(H8, side), bksq] <= 1 then begin
        if (pawns and SquareBB[relative(H5, side)] = 0) or
           (board^.bb.pawns[xside] and BB_G7H6[side] <> BB_G7H6[side]) then
        begin
          score := SCORE_ZERO;
          result := true;
          exit;
        end;
      end;

    if pawns and not BB_FileG = 0 then
      if pawns and SquareBB[relative(G6, side)] <> 0 then
        if board^.bb.pawns[xside] and SquareBB[relative(G7, side)] <> 0 then
          if bkzone and BB_F8G8[side] <> 0 then begin
            score := SCORE_ZERO;
            result := true;
            exit;
          end;

    if pawns and not BB_FileA = 0 then
      if pawns and SquareBB[relative(A6, side)] <> 0 then
        if board^.bb.pawns[xside] and SquareBB[relative(A7, side)] <> 0 then
          if bkzone and SquareBB[relative(B8, side)] <> 0 then begin
            b := board^.bb.pawns[xside] and BB_FileB;
            if b = 0 then begin
              score := SCORE_ZERO;
              result := true;
              exit;
            end;
            if side = WHITE then begin
              br := rank_of(BSR(b));
              wr := rank_of(BSF(pawns));
            end
            else begin
              br := flip(rank_of(BSF(b)));
              wr := flip(rank_of(BSR(pawns)));
            end;
            if wr >= br then begin
              score := SCORE_ZERO;
              result := true;
              exit;
            end;
          end;

    if psq = relative(G6, side) then
      if bishops and SquareBB[relative(B7, side)] <> 0 then
        if Distance[relative(H8, side), bksq] <= 1 then begin
          score := SCORE_ZERO;
          result := true;
          exit;
        end;

    if psq = relative(B6, side) then
      if board^.bb.pawns[xside] and SquareBB[relative(B7, side)] <> 0 then
        if board^.bb.king[side] and BB_ABC8AC7[side] <> 0 then begin
          score := SCORE_ZERO;
          result := true;
          exit;
        end;
  end
  else begin
    if pawns and not BB_FileA = 0 then
      if Distance[relative(A8, side), bksq] <= 1 then begin
        if (pawns and SquareBB[relative(A5, side)] = 0) or
           (board^.bb.pawns[xside] and BB_A6B7[side] <> BB_A6B7[side]) then
        begin
          score := SCORE_ZERO;
          result := true;
          exit;
        end;
      end;

    if pawns and not BB_FileB = 0 then
      if pawns and SquareBB[relative(B6, side)] <> 0 then
        if board^.bb.pawns[xside] and SquareBB[relative(B7, side)] <> 0 then
          if bkzone and BB_B8C8[side] <> 0 then begin
            score := SCORE_ZERO;
            result := true;
            exit;
          end;

    if pawns and not BB_FileH = 0 then
      if pawns and SquareBB[relative(H6, side)] <> 0 then
        if board^.bb.pawns[xside] and SquareBB[relative(H7, side)] <> 0 then
          if bkzone and SquareBB[relative(G8, side)] <> 0 then begin
            b := board^.bb.pawns[xside] and BB_FileG;
            if b = 0 then begin
              score := SCORE_ZERO;
              result := true;
              exit;
            end;
            if side = WHITE then begin
              br := rank_of(BSR(b));
              wr := rank_of(BSF(pawns));
            end
            else begin
              br := flip(rank_of(BSF(b)));
              wr := flip(rank_of(BSR(pawns)));
            end;
            if wr >= br then begin
              score := SCORE_ZERO;
              result := true;
              exit;
            end;
          end;

    if psq = relative(B6, side) then
      if bishops and SquareBB[relative(A7, side)] <> 0 then
        if Distance[relative(A8, side), bksq] <= 1 then begin
          score := SCORE_ZERO;
          result := true;
          exit;
        end;

    if psq = relative(G6, side) then
      if board^.bb.pawns[xside] and SquareBB[relative(B7, side)] <> 0 then
        if board^.bb.king[side] and BB_FGH8FH7[side] <> 0 then begin
          score := SCORE_ZERO;
          result := true;
          exit;
        end;
  end;

  result := false;
end;

function TEgEval.eg_krppkrp(side: TSide; var score: TScore): boolean;
const
  mult: array [TRank] of integer = ( 0, 10, 10, 15, 20, 40, 0, 0 );
var
  pawns: TBitboard;
  rr: TRank;
  wpsq1, wpsq2, bksq: TSquare;
begin
  result := false;

  if score * sign[side] <= SCORE_ZERO then
    exit;

  with board^ do begin
    bksq := kingsq[flip(side)];
    pawns := bb.pawns[side];
  end;

  wpsq1 := find_lsb(pawns);
  wpsq2 := find_lsb(pawns and (pawns-1));

  if board^.pawn_is_passed(wpsq1, side) or board^.pawn_is_passed(wpsq2, side) then
    exit;

  rr := max(relative_rank(wpsq1, side), relative_rank(wpsq2, side));
  if (file_distance(bksq, wpsq1) <= 1) and (file_distance(bksq, wpsq2) <= 1) then
    if relative_rank(bksq, side) > rr then begin
      score := score * mult[rr] div 128;
      result := true;
      exit;
    end;
end;

function TEgEval.eg_kbkp(side: TSide; var score: TScore): boolean;
var
  xside: TSide;
  psq, wksq, bksq: TSquare;
  b, pawn_path: TBitboard;
begin
  result := false;
  xside := flip(side);

  if board^.us = side then begin
    if atk.bishop[side] and board^.bb.pawns[xside] <> 0 then begin
      score := SCORE_ZERO;
      result := true;
      exit;
    end;
  end
  else begin
    if side = WHITE then
      b := TBB(board^.bb.pawns[xside] shr 8) and TBB(BB_Rank1)
    else
      b := TBB(board^.bb.pawns[xside] shl 8) and TBB(BB_Rank8);

    if b and not atk.all[side] <> 0 then
      exit;

    if atk.all[xside] and board^.bb.bishops[side] <> 0 then begin
      if (relative_rank(find_lsb(board^.bb.pawns[xside]), xside) >= RANK_6) then
        exit;

      if (score * sign[side] < SCORE_ZERO) or (atk.all[side] and board^.bb.bishops[side] <> 0) then
        exit;

      bksq := relative(board^.kingsq[side], xside);
      if atk.pawn[xside] and board^.bb.bishops[side] <> 0 then begin
        psq := relative(TSquare(board^.piece_list.bishops[side, 0]), xside);
        wksq := relative(board^.kingsq[xside], xside);
        if (not kpk_draw(wksq, bksq, psq, BLACK)) then begin
          score := ($118 + integer(rank_of(psq))*8) * sign[side]
            + mi^.mtrl_score + st^.psqval;
          result := true;
          exit;
        end;
      end;

      if atk.king[xside] and board^.bb.bishops[side] <> 0 then begin
        psq := relative(find_lsb(board^.bb.pawns[xside]), xside);
        wksq := relative(TSquare(board^.piece_list.bishops[side, 0]), xside);
        if (not kpk_draw(wksq, bksq, psq, BLACK)) then begin
          score := ($118 + integer(rank_of(psq))*8) * sign[side]
            + mi^.mtrl_score + st^.psqval;
          result := true;
          exit;
        end;
      end;
      score := SCORE_ZERO;
      result := true;
      exit;
    end;
  end;

  pawn_path := ForwardBB[xside, find_lsb(board^.bb.pawns[xside])];
  if pawn_path and (atk.bishop[side] or board^.bb.bishops[side]) <> 0 then begin
    score := SCORE_ZERO;
    result := true;
    exit;
  end;

  result := false;
end;

function TEgEval.eg_kbppkb(side: TSide; var score: TScore): boolean;
var
  xside: TSide;
  pawns: TBitboard;
  ksq, wbsq, psq1, psq2: TSquare;
  block1, block2: TSquare;
begin
  result := false;

  if (not st^.bishop_flags in [BF_OPPOSITE_1, BF_OPPOSITE_2]) then exit;

  xside := flip(side);
  ksq := board^.kingsq[xside];
  wbsq := TSquare(board^.piece_list.bishops[side, 0]);

  if sq_color(ksq) = sq_color(wbsq) then exit;

  pawns := board^.bb.pawns[side];
  psq1 := find_lsb(pawns);
  psq2 := find_lsb(pawns and (pawns-1));

  if relative_rank(psq1, side) > relative_rank(psq2, side) then begin
    block1 := delta_add(psq1, PawnPush[side]);
    block2 := delta_add(psq2, PawnPush[side]);
  end
  else begin
    block1 := delta_add(psq2, PawnPush[side]);
    block2 := delta_add(psq1, PawnPush[side]);
  end;

  if file_of(psq1) = file_of(psq2) then begin
    if file_of(ksq) = file_of(psq1) then
      if relative_rank(ksq, side) >= relative_rank(block1, side) then begin
        score := SCORE_ZERO;
        result := true;
      end;
    exit;
  end;

  if file_distance(psq1, psq2) = 1 then begin
    if ksq = block1 then
      if ((atk.bishop[xside] or board^.bb.bishops[xside]) and SquareBB[block2] <> 0)
        or (rank_distance(psq1, psq2) >= 2) then
      begin
        score := SCORE_ZERO;
        result := true;
        exit;
      end;

    if ksq = block2 then
      if ((atk.bishop[xside] or board^.bb.bishops[xside]) and SquareBB[block1] <> 0)
        or (rank_distance(psq1, psq2) >= 2) then
      begin
        score := SCORE_ZERO;
        result := true;
        exit;
      end;
  end;
end;

function TEgEval.eg_kbkpp(side: TSide; var score: TScore): boolean;
var
  xside: TSide;
  pawns, b: TBitboard;
  wksq, psq1, psq2: TSquare;
begin
  xside := flip(side);
  with board^ do begin
    wksq := kingsq[side];
    pawns := bb.pawns[xside];
  end;
  psq1 := find_lsb(pawns);
  psq2 := find_lsb(pawns and (pawns-1));

  with board^ do
    b := bb.bishops[side] or bishop_attacks(TSquare(piece_list.bishops[side, 0]));

  if (file_of(wksq) = file_of(psq1)) or (b and ForwardBB[xside, psq1] <> 0) then
    if (file_of(wksq) = file_of(psq2)) or (b and ForwardBB[xside, psq2] <> 0)  then
    begin
      score := SCORE_ZERO;
      result := true;
      exit;
    end;

  result := false;
end;

end.
