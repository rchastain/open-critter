
unit uMovegen;
{.$warn 6058 off}

interface

uses
  uBitboard, uBoard, uHistory, uMove, uPiece, uScore, uSquare;

{$include platform.inc}

var
  MVV_LVA: array [TPiece, TPiece] of TScore;
  MVV_LVA_M: array [0..255] of TScore;

function gen_all(var b: TBoard; out ml: TMoveList): integer;
function gen_legal_moves(var b: TBoard; out ml: TMoveList): integer;
function gen_captures(var b: TBoard; ms: PMoveStack; target: TBitboard): PMoveStack; inline;
function gen_quiet(var b: TBoard; ms: PMoveStack): PMoveStack; inline;
function gen_evasions(var b: TBoard; ms: PMoveStack; target: TBitboard): integer; inline;
function gen_checks(var b: TBoard; ms: PMoveStack; target: TBitboard): PMoveStack; inline;
procedure gen_positional(var b: TBoard; ms: PMoveStack; target: TScore); inline;

function zugzwang_danger(var board: TBoard): boolean;

function gen_captures_w(var b: TBoard; ms: PMoveStack; target: TBitboard): PMoveStack;
function gen_captures_b(var b: TBoard; ms: PMoveStack; target: TBitboard): PMoveStack;

function gen_quiet_w(var b: TBoard; ms: PMoveStack): PMoveStack;
function gen_quiet_b(var b: TBoard; ms: PMoveStack): PMoveStack;

function gen_evasions_w(var b: TBoard; ms: PMoveStack; target: TBitboard): PMoveStack;
function gen_evasions_b(var b: TBoard; ms: PMoveStack; target: TBitboard): PMoveStack;

function gen_checks_w(var b: TBoard; ms: PMoveStack; target: TBitboard): PMoveStack;
function gen_checks_b(var b: TBoard; ms: PMoveStack; target: TBitboard): PMoveStack;

function gen_positional_w(var b: TBoard; ms: PMoveStack; target: TScore): PMoveStack;
function gen_positional_b(var b: TBoard; ms: PMoveStack; target: TScore): PMoveStack;

const
  EV_SORT: array [TPiece] of byte =
    ( 0, 0, 6, 6, 7, 7, 5, 5, 4, 4, 3, 3, 2, 2, 0, 0 );

implementation

const
  SCORE_QP = 6*16;
  SCORE_NP = 3*16;
  SCORE_EP = 2*16 - 1;

function gen_captures_w(var b: TBoard; ms: PMoveStack; target: TBitboard): PMoveStack;
var
  from_bb, to_bb: TBitboard;
  from_sq, to_sq: TSquare;
  lp: PShortInt;
  m: TMove;
  s: TScore;
begin
  if b.st^.w_attacks and target <> 0 then begin

    lp := @b.piece_list.wn;
    while lp^ <> -1 do begin
      from_sq := TSquare(lp^);
      to_bb := KnightStep[from_sq] and target;
      while to_bb <> 0 do begin
        to_sq := find_lsb(to_bb);
        ms^.move := mk_move(from_sq, to_sq, W_KNIGHT, b.squares[to_sq]);
        ms^.score := MVV_LVA[W_KNIGHT, b.squares[to_sq]];
        inc(ms);
        clear_lsb(to_bb);
      end;
      inc(lp);
    end;

    lp := @b.piece_list.wb;
    while lp^ <> -1 do begin
      from_sq := TSquare(lp^);
      to_bb := b.bishop_attacks(from_sq) and target;
      while to_bb <> 0 do begin
        to_sq := find_lsb(to_bb);
        ms^.move := mk_move(from_sq, to_sq, W_BISHOP, b.squares[to_sq]);
        ms^.score := MVV_LVA[W_BISHOP, b.squares[to_sq]];
        inc(ms);
        clear_lsb(to_bb);
      end;
      inc(lp);
    end;

    lp := @b.piece_list.wr;
    while lp^ <> -1 do begin
      from_sq := TSquare(lp^);
      to_bb := b.rook_attacks(from_sq) and target;
      while to_bb <> 0 do begin
        to_sq := find_lsb(to_bb);
        ms^.move := mk_move(from_sq, to_sq, W_ROOK, b.squares[to_sq]);
        ms^.score := MVV_LVA[W_ROOK, b.squares[to_sq]];
        inc(ms);
        clear_lsb(to_bb);
      end;
      inc(lp);
    end;

    lp := @b.piece_list.wq;
    while lp^ <> -1 do begin
      from_sq := TSquare(lp^);
      to_bb := b.queen_attacks(from_sq) and target;
      while to_bb <> 0 do begin
        to_sq := find_lsb(to_bb);
        ms^.move := mk_move(from_sq, to_sq, W_QUEEN, b.squares[to_sq]);
        ms^.score := MVV_LVA[W_QUEEN, b.squares[to_sq]];
        inc(ms);
        clear_lsb(to_bb);
      end;
      inc(lp);
    end;

    from_sq := b.kingsq[WHITE];
    to_bb := KingStep[from_sq] and target and not b.st^.b_attacks;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      ms^.move := mk_move(from_sq, to_sq, W_KING, b.squares[to_sq]);
      ms^.score := MVV_LVA[W_KING, b.squares[to_sq]];
      inc(ms);
      clear_lsb(to_bb);
    end;

    to_bb := pawn_east_w(b.bb.w_pawns and not BB_Rank7) and target;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      from_sq := TSquare(integer(to_sq) - DELTA_NE);
      ms^.move := mk_move(from_sq, to_sq, W_PAWN, b.squares[to_sq]);
      ms^.score := MVV_LVA[W_PAWN, b.squares[to_sq]];
      inc(ms);
      clear_lsb(to_bb);
    end;

    to_bb := pawn_west_w(b.bb.w_pawns and not BB_Rank7) and target;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      from_sq := delta_sub(to_sq, DELTA_NW);
      ms^.move := mk_move(from_sq, to_sq, W_PAWN, b.squares[to_sq]);
      ms^.score := MVV_LVA[W_PAWN, b.squares[to_sq]];
      inc(ms);
      clear_lsb(to_bb);
    end;
  end;

  from_bb := b.bb.w_pawns and BB_Rank7;
  while from_bb <> 0 do begin
    from_sq := find_lsb(from_bb);
    to_sq := delta_add(from_sq, DELTA_N);
    if (b.squares[to_sq] = NOPIECE) then begin
      ms^.move := mk_move(from_sq, to_sq, W_PAWN, NOPIECE, W_QUEEN);
      ms^.score := SCORE_QP; inc(ms);
      if KnightStep[to_sq] and b.bb.b_king <> 0 then begin
        ms^.move := mk_move(from_sq, to_sq, W_PAWN, NOPIECE, W_KNIGHT);
        ms^.score := SCORE_NP; inc(ms);
      end;
    end;
    if (from_sq <> A7) then begin
      to_sq := delta_add(from_sq, DELTA_NW);
      if (target and SquareBB[to_sq]) <> 0 then begin
        s := MVV_LVA[W_PAWN, b.squares[to_sq]];
        m := mk_move(from_sq, to_sq, W_PAWN, b.squares[to_sq]);
        ms^.move := mk_promotion(m, W_QUEEN); ms^.score := s + SCORE_QP; inc(ms);
        if KnightStep[to_sq] and b.bb.b_king <> 0 then begin
          ms^.move := mk_promotion(m, W_KNIGHT); ms^.score := s; inc(ms);
        end;
      end;
    end;
    if (from_sq <> H7) then begin
      to_sq := delta_add(from_sq, DELTA_NE);
      if (target and SquareBB[to_sq]) <> 0 then begin
        s := MVV_LVA[W_PAWN, b.squares[to_sq]];
        m := mk_move(from_sq, to_sq, W_PAWN, b.squares[to_sq]);
        ms^.move := mk_promotion(m, W_QUEEN); ms^.score := s + SCORE_QP; inc(ms);
        if KnightStep[to_sq] and b.bb.b_king <> 0 then begin
          ms^.move := mk_promotion(m, W_KNIGHT); ms^.score := s; inc(ms);
        end;
      end;
    end;
    clear_lsb(from_bb);
  end;

  to_sq := b.st^.epsq;
  if (to_sq <> NO_EP) then begin
    if (pawn_east_w(b.bb.w_pawns) and SquareBB[to_sq]) <> 0 then begin
      from_sq := delta_sub(to_sq, DELTA_NE);
      ms^.move := mk_ep(from_sq, to_sq, W_PAWN, B_PAWN);
      ms^.score := SCORE_EP; inc(ms);
    end;
    if (pawn_west_w(b.bb.w_pawns) and SquareBB[to_sq]) <> 0 then begin
      from_sq := delta_sub(to_sq, DELTA_NW);
      ms^.move := mk_ep(from_sq, to_sq, W_PAWN, B_PAWN);
      ms^.score := SCORE_EP; inc(ms);
    end;
  end;

  ms^.move64 := 0;
  result := ms;
end;

function gen_captures_b(var b: TBoard; ms: PMoveStack; target: TBitboard): PMoveStack;
var
  from_bb, to_bb: TBitboard;
  from_sq, to_sq: TSquare;
  lp: PShortInt;
  m: TMove;
  s: TScore;
begin
  if b.st^.b_attacks and target <> 0 then begin

    lp := @b.piece_list.bn;
    while lp^ <> -1 do begin
      from_sq := TSquare(lp^);
      to_bb := KnightStep[from_sq] and target;
      while to_bb <> 0 do begin
        to_sq := find_lsb(to_bb);
        ms^.move := mk_move(from_sq, to_sq, B_KNIGHT, b.squares[to_sq]);
        ms^.score := MVV_LVA[B_KNIGHT, b.squares[to_sq]];
        inc(ms);
        clear_lsb(to_bb);
      end;
      inc(lp);
    end;

    lp := @b.piece_list.bb;
    while lp^ <> -1 do begin
      from_sq := TSquare(lp^);
      to_bb := b.bishop_attacks(from_sq) and target;
      while to_bb <> 0 do begin
        to_sq := find_lsb(to_bb);
        ms^.move := mk_move(from_sq, to_sq, B_BISHOP, b.squares[to_sq]);
        ms^.score := MVV_LVA[B_BISHOP, b.squares[to_sq]];
        inc(ms);
        clear_lsb(to_bb);
      end;
      inc(lp);
    end;

    lp := @b.piece_list.br;
    while lp^ <> -1 do begin
      from_sq := TSquare(lp^);
      to_bb := b.rook_attacks(from_sq) and target;
      while to_bb <> 0 do begin
        to_sq := find_lsb(to_bb);
        ms^.move := mk_move(from_sq, to_sq, B_ROOK, b.squares[to_sq]);
        ms^.score := MVV_LVA[B_ROOK, b.squares[to_sq]];
        inc(ms);
        clear_lsb(to_bb);
      end;
      inc(lp);
    end;

    lp := @b.piece_list.bq;
    while lp^ <> -1 do begin
      from_sq := TSquare(lp^);
      to_bb := b.queen_attacks(from_sq) and target;
      while to_bb <> 0 do begin
        to_sq := find_lsb(to_bb);
        ms^.move := mk_move(from_sq, to_sq, B_QUEEN, b.squares[to_sq]);
        ms^.score := MVV_LVA[B_QUEEN, b.squares[to_sq]];
        inc(ms);
        clear_lsb(to_bb);
      end;
      inc(lp);
    end;

    from_sq := b.kingsq[BLACK];
    to_bb := KingStep[from_sq] and target and not b.st^.w_attacks;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      ms^.move := mk_move(from_sq, to_sq, B_KING, b.squares[to_sq]);
      ms^.score := MVV_LVA[B_KING, b.squares[to_sq]];
      inc(ms);
      clear_lsb(to_bb);
    end;

    to_bb := pawn_east_b(b.bb.b_pawns and not BB_Rank2) and target;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      from_sq := TSquare(integer(to_sq) - DELTA_SE);
      ms^.move := mk_move(from_sq, to_sq, B_PAWN, b.squares[to_sq]);
      ms^.score := MVV_LVA[B_PAWN, b.squares[to_sq]];
      inc(ms);
      clear_lsb(to_bb);
    end;

    to_bb := pawn_west_b(b.bb.b_pawns and not BB_Rank2) and target;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      from_sq := delta_sub(to_sq, DELTA_SW);
      ms^.move := mk_move(from_sq, to_sq, B_PAWN, b.squares[to_sq]);
      ms^.score := MVV_LVA[B_PAWN, b.squares[to_sq]];
      inc(ms);
      clear_lsb(to_bb);
    end;
  end;

  from_bb := b.bb.b_pawns and BB_Rank2;
  while from_bb <> 0 do begin
    from_sq := find_lsb(from_bb);
    to_sq := delta_add(from_sq, DELTA_S);
    if (b.squares[to_sq] = NOPIECE) then begin
      ms^.move := mk_move(from_sq, to_sq, B_PAWN, NOPIECE, B_QUEEN);
      ms^.score := SCORE_QP; inc(ms);
      if KnightStep[to_sq] and b.bb.w_king <> 0 then begin
        ms^.move := mk_move(from_sq, to_sq, B_PAWN, NOPIECE, B_KNIGHT);
        ms^.score := SCORE_NP; inc(ms);
      end;
    end;
    if (from_sq <> A2) then begin
      to_sq := delta_add(from_sq, DELTA_SW);
      if (target and SquareBB[to_sq]) <> 0 then begin
        s := MVV_LVA[B_PAWN, b.squares[to_sq]];
        m := mk_move(from_sq, to_sq, B_PAWN, b.squares[to_sq]);
        ms^.move := mk_promotion(m, B_QUEEN); ms^.score := s + SCORE_QP; inc(ms);
        if KnightStep[to_sq] and b.bb.w_king <> 0 then begin
          ms^.move := mk_promotion(m, B_KNIGHT); ms^.score := s; inc(ms);
        end;
      end;
    end;
    if (from_sq <> H2) then begin
      to_sq := delta_add(from_sq, DELTA_SE);
      if (target and SquareBB[to_sq]) <> 0 then begin
        s := MVV_LVA[B_PAWN, b.squares[to_sq]];
        m := mk_move(from_sq, to_sq, B_PAWN, b.squares[to_sq]);
        ms^.move := mk_promotion(m, B_QUEEN); ms^.score := s + SCORE_QP; inc(ms);
        if KnightStep[to_sq] and b.bb.w_king <> 0 then begin
          ms^.move := mk_promotion(m, B_KNIGHT); ms^.score := s; inc(ms);
        end;
      end;
    end;
    clear_lsb(from_bb);
  end;

  to_sq := b.st^.epsq;
  if (to_sq <> NO_EP) then begin
    if (pawn_east_b(b.bb.b_pawns) and SquareBB[to_sq]) <> 0 then begin
      from_sq := delta_sub(to_sq, DELTA_SE);
      ms^.move := mk_ep(from_sq, to_sq, B_PAWN, W_PAWN);
      ms^.score := SCORE_EP; inc(ms);
    end;
    if (pawn_west_b(b.bb.b_pawns) and SquareBB[to_sq]) <> 0 then begin
      from_sq := delta_sub(to_sq, DELTA_SW);
      ms^.move := mk_ep(from_sq, to_sq, B_PAWN, W_PAWN);
      ms^.score := SCORE_EP; inc(ms);
    end;
  end;

  ms^.move64 := 0;
  result := ms;
end;

function gen_quiet_w(var b: TBoard; ms: PMoveStack): PMoveStack;
var
  from_bb, to_bb: TBitboard;
  from_sq, to_sq: TSquare;
  lp: PShortInt;
  m: TMove;
begin
  if b.st^.castle_rights and CR_W_SHORT <> 0 then
    if b.bb.occupied and Empty_OO[WHITE] = 0 then
      if b.st^.b_attacks and Safe_OO[WHITE] = 0 then begin
        to_sq := square(RANK_1, b.start_file[START_ROOK_H]);
        ms^.move := mk_castle(b.kingsq[WHITE], to_sq, W_KING);
        ms^.score := hist.ordering[W_KING, to_sq];
        inc(ms);
      end;

  if b.st^.castle_rights and CR_W_LONG <> 0 then
    if b.bb.occupied and Empty_OOO[WHITE] = 0 then
      if b.st^.b_attacks and Safe_OOO[WHITE] = 0 then begin
        to_sq := square(RANK_1, b.start_file[START_ROOK_A]);
        ms^.move := mk_castle(b.kingsq[WHITE], to_sq, W_KING);
        ms^.score := hist.ordering[W_KING, to_sq];
        inc(ms);
      end;

  lp := @b.piece_list.wn;
  while lp^ <> -1 do begin
    from_sq := TSquare(lp^);
    to_bb := KnightStep[from_sq] and b.bb.empty;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      ms^.move := mk_move(from_sq, to_sq, W_KNIGHT);
      ms^.score := hist.ordering[W_KNIGHT, to_sq];
      inc(ms); clear_lsb(to_bb);
    end;
    inc(lp);
  end;

  lp := @b.piece_list.wb;
  while lp^ <> -1 do begin
    from_sq := TSquare(lp^);
    to_bb := b.bishop_attacks(from_sq) and b.bb.empty;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      ms^.move := mk_move(from_sq, to_sq, W_BISHOP);
      ms^.score := hist.ordering[W_BISHOP, to_sq];
      inc(ms); clear_lsb(to_bb);
    end;
    inc(lp);
  end;

  lp := @b.piece_list.wr;
  while lp^ <> -1 do begin
    from_sq := TSquare(lp^);
    to_bb := b.rook_attacks(from_sq) and b.bb.empty;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      ms^.move := mk_move(from_sq, to_sq, W_ROOK);
      ms^.score := hist.ordering[W_ROOK, to_sq];
      inc(ms); clear_lsb(to_bb);
    end;
    inc(lp);
  end;

  lp := @b.piece_list.wq;
  while lp^ <> -1 do begin
    from_sq := TSquare(lp^);
    to_bb := b.queen_attacks(from_sq) and b.bb.empty;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      ms^.move := mk_move(from_sq, to_sq, W_QUEEN);
      ms^.score := hist.ordering[W_QUEEN, to_sq];
      inc(ms); clear_lsb(to_bb);
    end;
    inc(lp);
  end;

  from_sq := b.kingsq[WHITE];
  to_bb := KingStep[from_sq] and b.bb.empty and not b.st^.b_attacks;
  while to_bb <> 0 do begin
    to_sq := find_lsb(to_bb);
    ms^.move := mk_move(from_sq, to_sq, W_KING);
    ms^.score := hist.ordering[W_KING, to_sq];
    inc(ms); clear_lsb(to_bb);
  end;

  to_bb := b.bb.w_pawns shl 8 and not BB_Rank8 and b.bb.empty;
  while to_bb <> 0 do begin
    to_sq := find_lsb(to_bb);
    from_sq := delta_sub(to_sq, DELTA_N);
    if (rank_of(to_sq) = RANK_3) then begin
      inc(to_sq, DELTA_N);
      if b.squares[to_sq] = NOPIECE then begin
        ms^.move := mk_dp(from_sq, to_sq, W_PAWN);
        ms^.score := hist.ordering[W_PAWN, to_sq];
        inc(ms);
      end;
      dec(to_sq, DELTA_N);
    end;
    ms^.move := mk_move(from_sq, to_sq, W_PAWN);
    ms^.score := hist.ordering[W_PAWN, to_sq];
    inc(ms);
    clear_lsb(to_bb);
  end;

  from_bb := b.bb.w_pawns and BB_Rank7;
  while from_bb <> 0 do begin
    from_sq := find_lsb(from_bb);
    if from_sq <> A7 then begin
      to_sq := delta_add(from_sq, DELTA_NW);
      if SquareBB[to_sq] and b.bb.black <> 0 then begin
        m := mk_move(from_sq, to_sq, W_PAWN, b.squares[to_sq]);
        if KnightStep[to_sq] and b.bb.b_king = 0 then begin
          ms^.move := mk_promotion(m, W_KNIGHT); ms^.score := 0; inc(ms);
        end;
        ms^.move := mk_promotion(m, W_ROOK); ms^.score := 0; inc(ms);
        ms^.move := mk_promotion(m, W_BISHOP); ms^.score := 0; inc(ms);
      end;
    end;
    if from_sq <> H7 then begin
      to_sq := delta_add(from_sq, DELTA_NE);
      if SquareBB[to_sq] and b.bb.black <> 0 then begin
        m := mk_move(from_sq, to_sq, W_PAWN, b.squares[to_sq]);
        if KnightStep[to_sq] and b.bb.b_king = 0 then begin
          ms^.move := mk_promotion(m, W_KNIGHT); ms^.score := 0; inc(ms);
        end;
        ms^.move := mk_promotion(m, W_ROOK); ms^.score := 0; inc(ms);
        ms^.move := mk_promotion(m, W_BISHOP); ms^.score := 0; inc(ms);
      end;
    end;
    to_sq := delta_add(from_sq, DELTA_N);
    if b.squares[to_sq] = NOPIECE then begin
      m := mk_move(from_sq, to_sq, W_PAWN);
      if KnightStep[to_sq] and b.bb.b_king = 0 then begin
        ms^.move := mk_promotion(m, W_KNIGHT); ms^.score := 0; inc(ms);
      end;
      ms^.move := mk_promotion(m, W_ROOK); ms^.score := 0; inc(ms);
      ms^.move := mk_promotion(m, W_BISHOP); ms^.score := 0; inc(ms);
    end;
    clear_lsb(from_bb);
  end;

  ms^.move64 := 0;
  result := ms;
end;

function gen_quiet_b(var b: TBoard; ms: PMoveStack): PMoveStack;
var
  from_bb, to_bb: TBitboard;
  from_sq, to_sq: TSquare;
  lp: PShortInt;
  m: TMove;
begin
  if b.st^.castle_rights and CR_B_SHORT <> 0 then
    if b.bb.occupied and Empty_OO[BLACK] = 0 then
      if b.st^.w_attacks and Safe_OO[BLACK] = 0 then begin
        to_sq := square(RANK_8, b.start_file[START_ROOK_H]);
        ms^.move := mk_castle(b.kingsq[BLACK], to_sq, B_KING);
        ms^.score := hist.ordering[B_KING, to_sq];
        inc(ms);
      end;

  if b.st^.castle_rights and CR_B_LONG <> 0 then
    if b.bb.occupied and Empty_OOO[BLACK] = 0 then
      if b.st^.w_attacks and Safe_OOO[BLACK] = 0 then begin
        to_sq := square(RANK_8, b.start_file[START_ROOK_A]);
        ms^.move := mk_castle(b.kingsq[BLACK], to_sq, B_KING);
        ms^.score := hist.ordering[B_KING, to_sq];
        inc(ms);
      end;

  lp := @b.piece_list.bn;
  while lp^ <> -1 do begin
    from_sq := TSquare(lp^);
    to_bb := KnightStep[from_sq] and b.bb.empty;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      ms^.move := mk_move(from_sq, to_sq, B_KNIGHT);
      ms^.score := hist.ordering[B_KNIGHT, to_sq];
      inc(ms); clear_lsb(to_bb);
    end;
    inc(lp);
  end;

  lp := @b.piece_list.bb;
  while lp^ <> -1 do begin
    from_sq := TSquare(lp^);
    to_bb := b.bishop_attacks(from_sq) and b.bb.empty;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      ms^.move := mk_move(from_sq, to_sq, B_BISHOP);
      ms^.score := hist.ordering[B_BISHOP, to_sq];
      inc(ms); clear_lsb(to_bb);
    end;
    inc(lp);
  end;

  lp := @b.piece_list.br;
  while lp^ <> -1 do begin
    from_sq := TSquare(lp^);
    to_bb := b.rook_attacks(from_sq) and b.bb.empty;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      ms^.move := mk_move(from_sq, to_sq, B_ROOK);
      ms^.score := hist.ordering[B_ROOK, to_sq];
      inc(ms); clear_lsb(to_bb);
    end;
    inc(lp);
  end;

  lp := @b.piece_list.bq;
  while lp^ <> -1 do begin
    from_sq := TSquare(lp^);
    to_bb := b.queen_attacks(from_sq) and b.bb.empty;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      ms^.move := mk_move(from_sq, to_sq, B_QUEEN);
      ms^.score := hist.ordering[B_QUEEN, to_sq];
      inc(ms); clear_lsb(to_bb);
    end;
    inc(lp);
  end;

  from_sq := b.kingsq[BLACK];
  to_bb := KingStep[from_sq] and b.bb.empty and not b.st^.w_attacks;
  while to_bb <> 0 do begin
    to_sq := find_lsb(to_bb);
    ms^.move := mk_move(from_sq, to_sq, B_KING);
    ms^.score := hist.ordering[B_KING, to_sq];
    inc(ms); clear_lsb(to_bb);
  end;

  to_bb := b.bb.b_pawns shr 8 and not BB_Rank1 and b.bb.empty;
  while to_bb <> 0 do begin
    to_sq := find_lsb(to_bb);
    from_sq := delta_sub(to_sq, DELTA_S);
    if (rank_of(to_sq) = RANK_6) then begin
      inc(to_sq, DELTA_S);
      if b.squares[to_sq] = NOPIECE then begin
        ms^.move := mk_dp(from_sq, to_sq, B_PAWN);
        ms^.score := hist.ordering[B_PAWN, to_sq];
        inc(ms);
      end;
      dec(to_sq, DELTA_S);
    end;
    ms^.move := mk_move(from_sq, to_sq, B_PAWN);
    ms^.score := hist.ordering[B_PAWN, to_sq];
    inc(ms);
    clear_lsb(to_bb);
  end;

  from_bb := b.bb.b_pawns and BB_Rank2;
  while from_bb <> 0 do begin
    from_sq := find_lsb(from_bb);
    if from_sq <> A2 then begin
      to_sq := delta_add(from_sq, DELTA_SW);
      if SquareBB[to_sq] and b.bb.white <> 0 then begin
        m := mk_move(from_sq, to_sq, B_PAWN, b.squares[to_sq]);
        if KnightStep[to_sq] and b.bb.w_king = 0 then begin
          ms^.move := mk_promotion(m, B_KNIGHT); ms^.score := 0; inc(ms);
        end;
        ms^.move := mk_promotion(m, B_ROOK); ms^.score := 0; inc(ms);
        ms^.move := mk_promotion(m, B_BISHOP); ms^.score := 0; inc(ms);
      end;
    end;
    if from_sq <> H2 then begin
      to_sq := delta_add(from_sq, DELTA_SE);
      if SquareBB[to_sq] and b.bb.white <> 0 then begin
        m := mk_move(from_sq, to_sq, B_PAWN, b.squares[to_sq]);
        if KnightStep[to_sq] and b.bb.w_king = 0 then begin
          ms^.move := mk_promotion(m, B_KNIGHT); ms^.score := 0; inc(ms);
        end;
        ms^.move := mk_promotion(m, B_ROOK); ms^.score := 0; inc(ms);
        ms^.move := mk_promotion(m, B_BISHOP); ms^.score := 0; inc(ms);
      end;
    end;
    to_sq := delta_add(from_sq, DELTA_S);
    if b.squares[to_sq] = NOPIECE then begin
      m := mk_move(from_sq, to_sq, B_PAWN);
      if KnightStep[to_sq] and b.bb.w_king = 0 then begin
        ms^.move := mk_promotion(m, B_KNIGHT); ms^.score := 0; inc(ms);
      end;
      ms^.move := mk_promotion(m, B_ROOK); ms^.score := 0; inc(ms);
      ms^.move := mk_promotion(m, B_BISHOP); ms^.score := 0; inc(ms);
    end;
    clear_lsb(from_bb);
  end;

  ms^.move64 := 0;
  result := ms;
end;

function gen_evasions_w(var b: TBoard; ms: PMoveStack; target: TBitboard): PMoveStack;
var
  from_bb, to_bb, mask, checkers: TBitboard;
  from_sq, to_sq: TSquare;
  ksq, chksq: TSquare;
  checker: TPiece;
  not_pinned: TBitboard;
  m: TMove;
begin
  ksq := b.kingsq[WHITE];
  mask := target and not (b.bb.white or b.st^.b_attacks);
  checkers := b.st^.checkersBB;
  chksq := find_lsb(checkers);
  checker := b.squares[chksq];
  if checker >= W_BISHOP then
    mask := mask and ShadowBB[chksq, ksq];

  clear_lsb(checkers);
  if checkers <> 0 then begin
    chksq := find_lsb(checkers);
    if b.squares[chksq] >= W_BISHOP then
      mask := mask and ShadowBB[chksq, ksq];
    to_bb := KingStep[ksq] and mask;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      ms^.move := mk_move(ksq, to_sq, W_KING, b.squares[to_sq]);
      clear_lsb(to_bb);
      inc(ms);
    end;
    ms^.move64 := 0;
    result := ms;
    exit;
  end;

  to_bb := KingStep[ksq] and mask;
  while to_bb <> 0 do begin
   to_sq := find_lsb(to_bb);
   ms^.move := mk_move(ksq, to_sq, W_KING, b.squares[to_sq]);
   clear_lsb(to_bb);
   inc(ms);
  end;

  mask := target and (SquareBB[chksq] or BetweenBB[ksq, chksq]);
  if mask = 0 then begin
    ms^.move64 := 0;
    result := ms;
    exit;
  end;

  not_pinned := not b.st^.pins[BLACK];

  if pawn_west_w(b.bb.w_pawns and not_pinned) and mask and b.bb.black <> 0 then
  begin
    to_sq := find_lsb(mask and b.bb.black);
    m := mk_move(delta_sub(to_sq, DELTA_NW), to_sq, W_PAWN, b.squares[to_sq]);
    if rank_of(to_sq) <> RANK_8 then
      ms^.move := m
    else begin
      ms^.move := mk_promotion(m, W_QUEEN); inc(ms);
      ms^.move := mk_promotion(m, W_KNIGHT); inc(ms);
      ms^.move := mk_promotion(m, W_ROOK); inc(ms);
      ms^.move := mk_promotion(m, W_BISHOP);
    end;
    inc(ms);
  end;

  if pawn_east_w(b.bb.w_pawns and not_pinned) and mask and b.bb.black <> 0 then
  begin
    to_sq := find_lsb(mask and b.bb.black);
    m := mk_move(delta_sub(to_sq, DELTA_NE), to_sq, W_PAWN, b.squares[to_sq]);
    if rank_of(to_sq) <> RANK_8 then
      ms^.move := m
    else begin
      ms^.move := mk_promotion(m, W_QUEEN); inc(ms);
      ms^.move := mk_promotion(m, W_KNIGHT); inc(ms);
      ms^.move := mk_promotion(m, W_ROOK); inc(ms);
      ms^.move := mk_promotion(m, W_BISHOP);
    end;
    inc(ms);
  end;

  from_bb := b.bb.w_pawns and ((b.bb.empty and mask) shr 8) and not_pinned;
  while from_bb <> 0 do begin
    from_sq := find_lsb(from_bb);
    m := mk_move(from_sq, delta_add(from_sq, DELTA_N), W_PAWN);
    if rank_of(from_sq) <> RANK_7 then
      ms^.move := m
    else begin
      ms^.move := mk_promotion(m, W_QUEEN); inc(ms);
      ms^.move := mk_promotion(m, W_KNIGHT); inc(ms);
      ms^.move := mk_promotion(m, W_ROOK); inc(ms);
      ms^.move := mk_promotion(m, W_BISHOP);
    end;
    clear_lsb(from_bb);
    inc(ms);
  end;

  from_bb := b.bb.w_pawns and ((b.bb.empty and mask) shr 16)
    and (b.bb.empty shr 8) and not_pinned and BB_Rank2;
  while from_bb <> 0 do begin
    from_sq := find_lsb(from_bb);
    ms^.move := mk_dp(from_sq, delta_add(from_sq, 2*DELTA_N), W_PAWN);
    clear_lsb(from_bb);
    inc(ms);
  end;

  from_bb := b.bb.w_knights and not_pinned;
  while from_bb <> 0 do begin
    from_sq := find_lsb(from_bb);
    to_bb := KnightStep[from_sq] and mask;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      ms^.move := mk_move(from_sq, to_sq, W_KNIGHT, b.squares[to_sq]);
      clear_lsb(to_bb);
      inc(ms);
    end;
    clear_lsb(from_bb);
  end;

  from_bb := b.bb.w_bishops and not_pinned;
  while from_bb <> 0 do begin
    from_sq := find_lsb(from_bb);
    to_bb := b.bishop_attacks(from_sq) and mask;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      ms^.move := mk_move(from_sq, to_sq, W_BISHOP, b.squares[to_sq]);
      clear_lsb(to_bb);
      inc(ms);
    end;
    clear_lsb(from_bb);
  end;

  from_bb := b.bb.w_rooks and not_pinned;
  while from_bb <> 0 do begin
    from_sq := find_lsb(from_bb);
    to_bb := b.rook_attacks(from_sq) and mask;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      ms^.move := mk_move(from_sq, to_sq, W_ROOK, b.squares[to_sq]);
      clear_lsb(to_bb);
      inc(ms);
    end;
    clear_lsb(from_bb);
  end;

  from_bb := b.bb.w_queens and not_pinned;
  while from_bb <> 0 do begin
    from_sq := find_lsb(from_bb);
    to_bb := b.queen_attacks(from_sq) and mask;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      ms^.move := mk_move(from_sq, to_sq, W_QUEEN, b.squares[to_sq]);
      clear_lsb(to_bb);
      inc(ms);
    end;
    clear_lsb(from_bb);
  end;

  if (b.st^.epsq <> NO_EP) and (checker = B_PAWN) then begin
    from_bb := PawnAttacksBB[BLACK, b.st^.epsq] and b.bb.w_pawns and not_pinned;
    while from_bb <> 0 do begin
      from_sq := find_lsb(from_bb);
      ms^.move := mk_ep(from_sq, b.st^.epsq, W_PAWN, B_PAWN);
      clear_lsb(from_bb);
      inc(ms);
    end;
  end;

  ms^.move64 := 0;
  result := ms;
end;

function gen_evasions_b(var b: TBoard; ms: PMoveStack; target: TBitboard): PMoveStack;
var
  from_bb, to_bb, mask, checkers: TBitboard;
  from_sq, to_sq: TSquare;
  ksq, chksq: TSquare;
  checker: TPiece;
  not_pinned: TBitboard;
  m: TMove;
begin
  ksq := b.kingsq[BLACK];
  mask := target and not (b.bb.black or b.st^.w_attacks);
  checkers := b.st^.checkersBB;
  chksq := find_lsb(checkers);
  checker := b.squares[chksq];
  if checker >= W_BISHOP then
    mask := mask and ShadowBB[chksq, ksq];

  clear_lsb(checkers);
  if checkers <> 0 then begin
    chksq := find_lsb(checkers);
    if b.squares[chksq] >= W_BISHOP then
      mask := mask and ShadowBB[chksq, ksq];
    to_bb := KingStep[ksq] and mask;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      ms^.move := mk_move(ksq, to_sq, B_KING, b.squares[to_sq]);
      clear_lsb(to_bb);
      inc(ms);
    end;
    ms^.move64 := 0;
    result := ms;
    exit;
  end;

  to_bb := KingStep[ksq] and mask;
  while to_bb <> 0 do begin
   to_sq := find_lsb(to_bb);
   ms^.move := mk_move(ksq, to_sq, B_KING, b.squares[to_sq]);
   clear_lsb(to_bb);
   inc(ms);
  end;

  mask := target and (SquareBB[chksq] or BetweenBB[ksq, chksq]);
  if mask = 0 then begin
    ms^.move64 := 0;
    result := ms;
    exit;
  end;

  not_pinned := not b.st^.pins[WHITE];

  if pawn_west_b(b.bb.b_pawns and not_pinned) and mask and b.bb.white <> 0 then
  begin
    to_sq := find_lsb(mask and b.bb.white);
    m := mk_move(delta_sub(to_sq, DELTA_SW), to_sq, B_PAWN, b.squares[to_sq]);
    if rank_of(to_sq) <> RANK_1 then
      ms^.move := m
    else begin
      ms^.move := mk_promotion(m, B_QUEEN); inc(ms);
      ms^.move := mk_promotion(m, B_ROOK); inc(ms);
      ms^.move := mk_promotion(m, B_BISHOP); inc(ms);
      ms^.move := mk_promotion(m, B_KNIGHT);
    end;
    inc(ms);
  end;

  if pawn_east_b(b.bb.b_pawns and not_pinned) and mask and b.bb.white <> 0 then
  begin
    to_sq := find_lsb(mask and b.bb.white);
    m := mk_move(delta_sub(to_sq, DELTA_SE), to_sq, B_PAWN, b.squares[to_sq]);
    if rank_of(to_sq) <> RANK_1 then
      ms^.move := m
    else begin
      ms^.move := mk_promotion(m, B_QUEEN); inc(ms);
      ms^.move := mk_promotion(m, B_KNIGHT); inc(ms);
      ms^.move := mk_promotion(m, B_ROOK); inc(ms);
      ms^.move := mk_promotion(m, B_BISHOP);
    end;
    inc(ms);
  end;

  from_bb := b.bb.b_pawns and ((b.bb.empty and mask) shl 8) and not_pinned;
  while from_bb <> 0 do begin
    from_sq := find_lsb(from_bb);
    m := mk_move(from_sq, delta_add(from_sq, DELTA_S), B_PAWN);
    if rank_of(from_sq) <> RANK_2 then
      ms^.move := m
    else begin
      ms^.move := mk_promotion(m, B_QUEEN); inc(ms);
      ms^.move := mk_promotion(m, B_KNIGHT); inc(ms);
      ms^.move := mk_promotion(m, B_ROOK); inc(ms);
      ms^.move := mk_promotion(m, B_BISHOP);
    end;
    clear_lsb(from_bb);
    inc(ms);
  end;

  from_bb := b.bb.b_pawns and ((b.bb.empty and mask) shl 16)
    and (b.bb.empty shl 8) and not_pinned and BB_Rank7;
  while from_bb <> 0 do begin
    from_sq := find_lsb(from_bb);
    ms^.move := mk_dp(from_sq, delta_add(from_sq, 2*DELTA_S), B_PAWN);
    clear_lsb(from_bb);
    inc(ms);
  end;

  from_bb := b.bb.b_knights and not_pinned;
  while from_bb <> 0 do begin
    from_sq := find_lsb(from_bb);
    to_bb := KnightStep[from_sq] and mask;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      ms^.move := mk_move(from_sq, to_sq, B_KNIGHT, b.squares[to_sq]);
      clear_lsb(to_bb);
      inc(ms);
    end;
    clear_lsb(from_bb);
  end;

  from_bb := b.bb.b_bishops and not_pinned;
  while from_bb <> 0 do begin
    from_sq := find_lsb(from_bb);
    to_bb := b.bishop_attacks(from_sq) and mask;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      ms^.move := mk_move(from_sq, to_sq, B_BISHOP, b.squares[to_sq]);
      clear_lsb(to_bb);
      inc(ms);
    end;
    clear_lsb(from_bb);
  end;

  from_bb := b.bb.b_rooks and not_pinned;
  while from_bb <> 0 do begin
    from_sq := find_lsb(from_bb);
    to_bb := b.rook_attacks(from_sq) and mask;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      ms^.move := mk_move(from_sq, to_sq, B_ROOK, b.squares[to_sq]);
      clear_lsb(to_bb);
      inc(ms);
    end;
    clear_lsb(from_bb);
  end;

  from_bb := b.bb.b_queens and not_pinned;
  while from_bb <> 0 do begin
    from_sq := find_lsb(from_bb);
    to_bb := b.queen_attacks(from_sq) and mask;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      ms^.move := mk_move(from_sq, to_sq, B_QUEEN, b.squares[to_sq]);
      clear_lsb(to_bb);
      inc(ms);
    end;
    clear_lsb(from_bb);
  end;

  if (b.st^.epsq <> NO_EP) and (checker = W_PAWN) then begin
    from_bb := PawnAttacksBB[WHITE, b.st^.epsq] and b.bb.b_pawns and not_pinned;
    while from_bb <> 0 do begin
      from_sq := find_lsb(from_bb);
      ms^.move := mk_ep(from_sq, b.st^.epsq, B_PAWN, W_PAWN);
      clear_lsb(from_bb);
      inc(ms);
    end;
  end;

  ms^.move64 := 0;
  result := ms;
end;

function gen_checks_w(var b: TBoard; ms: PMoveStack; target: TBB): PMoveStack;
var
  from_bb, to_bb, mask, pb, non_dc: TBitboard;
  orth, diag: TBitboard;
  from_sq, to_sq, ksq: TSquare;
  lp: PShortInt;
  p: TPiece;
begin
  ksq := b.kingsq[BLACK];
  target := not (target or b.bb.white);
  from_bb := b.st^.pins[WHITE] and b.bb.white;
  while from_bb <> 0 do begin
    from_sq := find_lsb(from_bb);
    clear_lsb(from_bb);
    p := b.squares[from_sq];
    case type_of(p) of
      PAWN:
        begin
          if rank_of(from_sq) <> RANK_7 then begin
            if file_of(from_sq) <> file_of(ksq) then begin
              to_sq := delta_add(from_sq, DELTA_N);
              if b.squares[to_sq] = NOPIECE then begin
                ms^.move := mk_move(from_sq, to_sq, W_PAWN);
                inc(ms);
                if rank_of(from_sq) = RANK_2 then begin
                  inc(to_sq, DELTA_N);
                  if b.squares[to_sq] = NOPIECE then begin
                    ms^.move := mk_dp(from_sq, to_sq, W_PAWN);
                    inc(ms);
                  end;
                end;
              end;
            end;
            if pawn_west_w(SquareBB[from_sq]) and b.bb.black and target <> 0 then begin
              to_sq := delta_add(from_sq, DELTA_NW);
              ms^.move := mk_move(from_sq, to_sq, W_PAWN, b.squares[to_sq]);
              inc(ms);
            end;
            if pawn_east_w(SquareBB[from_sq]) and b.bb.black and target <> 0 then begin
              to_sq := delta_add(from_sq, DELTA_NE);
              ms^.move := mk_move(from_sq, to_sq, W_PAWN, b.squares[to_sq]);
              inc(ms);
            end;
          end;
          continue;
        end;

      KING:
       to_bb := KingStep[from_sq] and not (AllDirBB[ksq] or b.st^.b_attacks);

      KNIGHT:
        to_bb := KnightStep[from_sq];

      BISHOP:
        to_bb := b.bishop_attacks(from_sq);

      ROOK:
        to_bb := b.rook_attacks(from_sq);
    else
      assert(false, 'gen_checks: bad dc candidate');
    end;
    to_bb := to_bb and target;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      ms^.move := mk_move(from_sq, to_sq, p, b.squares[to_sq]);
      inc(ms);
      clear_lsb(to_bb);
    end;
  end;

  non_dc := not b.st^.pins[WHITE];
  pb := b.bb.w_pawns and non_dc;
  mask := target and b.bb.black and PawnAttacksBB[BLACK, ksq] and (not BB_Rank8);
  to_bb := pawn_west_w(pb) and mask;
  if to_bb <> 0 then begin
    to_sq := find_lsb(to_bb);
    from_sq := delta_sub(to_sq, DELTA_NW);
    ms^.move := mk_move(from_sq, to_sq, W_PAWN, b.squares[to_sq]);
    inc(ms);
    clear_lsb(to_bb);
    if to_bb <> 0 then begin
      to_sq := find_lsb(to_bb);
      from_sq := delta_sub(to_sq, DELTA_NW);
      ms^.move := mk_move(from_sq, to_sq, W_PAWN, b.squares[to_sq]);
      inc(ms);
    end;
  end;
  to_bb := pawn_east_w(pb) and mask;
  if to_bb <> 0 then begin
    to_sq := find_lsb(to_bb);
    from_sq := delta_sub(to_sq, DELTA_NE);
    ms^.move := mk_move(from_sq, to_sq, W_PAWN, b.squares[to_sq]);
    inc(ms);
    clear_lsb(to_bb);
    if to_bb <> 0 then begin
      to_sq := find_lsb(to_bb);
      from_sq := delta_sub(to_sq, DELTA_NE);
      ms^.move := mk_move(from_sq, to_sq, W_PAWN, b.squares[to_sq]);
      inc(ms);
    end;
  end;

  if b.bb.w_queens or b.bb.w_rooks <> 0 then
    orth := b.rook_attacks(ksq);
  if b.bb.w_queens or b.bb.w_bishops <> 0 then
    diag := b.bishop_attacks(ksq);

  lp := @b.piece_list.wq;
  if lp^ <> -1 then begin
    mask := (orth or diag) and target;
    repeat
      from_sq := TSquare(lp^);
      if AllDirBB[from_sq] and mask <> 0 then begin
        to_bb := b.queen_attacks(from_sq) and mask;
        while to_bb <> 0 do begin
          to_sq := find_lsb(to_bb);
          ms^.move := mk_move(from_sq, to_sq, W_QUEEN, b.squares[to_sq]);
          inc(ms);
          clear_lsb(to_bb);
        end;
      end;
      inc(lp);
    until lp^ = -1;
  end;

  lp := @b.piece_list.wr;
  if lp^ <> -1 then begin
    mask := orth and target;
    repeat
      from_sq := TSquare(lp^);
      if SquareBB[from_sq] and non_dc <> 0 then
        if OrthogonalBB[from_sq] and mask <> 0 then begin
          to_bb := b.rook_attacks(from_sq) and mask;
          while to_bb <> 0 do begin
            to_sq := find_lsb(to_bb);
            ms^.move := mk_move(from_sq, to_sq, W_ROOK, b.squares[to_sq]);
            inc(ms);
            clear_lsb(to_bb);
          end;
        end;
      inc(lp);
    until lp^ = -1;
  end;

  lp := @b.piece_list.wb;
  if lp^ <> -1 then begin
    mask := diag and target;
    repeat
      from_sq := TSquare(lp^);
      if SquareBB[from_sq] and non_dc <> 0 then
        if DiagonalBB[from_sq] and mask <> 0 then begin
          to_bb := b.bishop_attacks(from_sq) and mask;
          while to_bb <> 0 do begin
            to_sq := find_lsb(to_bb);
            ms^.move := mk_move(from_sq, to_sq, W_BISHOP, b.squares[to_sq]);
            inc(ms);
            clear_lsb(to_bb);
          end;
        end;
      inc(lp);
    until lp^ = -1;
  end;

  lp := @b.piece_list.wn;
  if lp^ <> -1 then begin
    mask := KnightStep[ksq] and target;
    repeat
      from_sq := TSquare(lp^);
      if SquareBB[from_sq] and non_dc <> 0 then
        to_bb := KnightStep[from_sq] and mask;
        while to_bb <> 0 do begin
          to_sq := find_lsb(to_bb);
          ms^.move := mk_move(from_sq, to_sq, W_KNIGHT, b.squares[to_sq]);
          inc(ms);
          clear_lsb(to_bb);
        end;
      inc(lp);
    until lp^ = -1;
  end;

  if b.bb.b_king and not (BB_Ranks123 or BB_FileA) <> 0 then begin
    to_sq := delta_sub(ksq, DELTA_NE);
    if b.squares[to_sq] = NOPIECE then begin
      from_sq := delta_sub(to_sq, DELTA_N);
      if b.squares[from_sq] = W_PAWN then begin
        ms^.move := mk_move(from_sq, to_sq, W_PAWN);
        inc(ms);
      end
      else
        if b.squares[from_sq] = NOPIECE then
          if rank_of(from_sq) = RANK_3 then begin
            dec(from_sq, DELTA_N);
            if b.squares[from_sq] = W_PAWN then begin
              ms^.move := mk_dp(from_sq, to_sq, W_PAWN);
              inc(ms);
            end;
          end;
    end;
  end;

  if b.bb.b_king and not (BB_Ranks123 or BB_FileH) <> 0 then begin
    to_sq := delta_sub(ksq, DELTA_NW);
    if b.squares[to_sq] = NOPIECE then begin
      from_sq := delta_sub(to_sq, DELTA_N);
      if b.squares[from_sq] = W_PAWN then begin
        ms^.move := mk_move(from_sq, to_sq, W_PAWN);
        inc(ms);
      end
      else
        if b.squares[from_sq] = NOPIECE then
          if (rank_of(from_sq) = RANK_3) then begin
            dec(from_sq, DELTA_N);
            if b.squares[from_sq] = W_PAWN then begin
              ms^.move := mk_dp(from_sq, to_sq, W_PAWN);
              inc(ms);
            end;
          end;
    end;
  end;

  ms^.move64 := 0;
  result := ms;
end;

function gen_checks_b(var b: TBoard; ms: PMoveStack; target: TBitboard): PMoveStack;
var
  from_bb, to_bb, mask, pb, non_dc: TBitboard;
  orth, diag: TBitboard;
  from_sq, to_sq, ksq: TSquare;
  lp: PShortInt;
  p: TPiece;
begin
  ksq := b.kingsq[WHITE];
  target := not (target or b.bb.black);
  from_bb := b.st^.pins[BLACK] and b.bb.black;
  while from_bb <> 0 do begin
    from_sq := find_lsb(from_bb);
    clear_lsb(from_bb);
    p := b.squares[from_sq];
    case type_of(p) of
      PAWN:
        begin
          if rank_of(from_sq) <> RANK_2 then begin
            if file_of(from_sq) <> file_of(ksq) then begin
              to_sq := delta_add(from_sq, DELTA_S);
              if b.squares[to_sq] = NOPIECE then begin
                ms^.move := mk_move(from_sq, to_sq, B_PAWN);
                inc(ms);
                if rank_of(from_sq) = RANK_7 then begin
                  inc(to_sq, DELTA_S);
                  if b.squares[to_sq] = NOPIECE then begin
                    ms^.move := mk_dp(from_sq, to_sq, B_PAWN);
                    inc(ms);
                  end;
                end;
              end;
            end;
            if pawn_west_b(SquareBB[from_sq]) and b.bb.white and target <> 0 then begin
              to_sq := delta_add(from_sq, DELTA_SW);
              ms^.move := mk_move(from_sq, to_sq, B_PAWN, b.squares[to_sq]);
              inc(ms);
            end;
            if pawn_east_b(SquareBB[from_sq]) and b.bb.white and target <> 0 then begin
              to_sq := delta_add(from_sq, DELTA_SE);
              ms^.move := mk_move(from_sq, to_sq, B_PAWN, b.squares[to_sq]);
              inc(ms);
            end;
          end;
          continue;
        end;

      KING:
       to_bb := KingStep[from_sq] and not (AllDirBB[ksq] or b.st^.w_attacks);

      KNIGHT:
        to_bb := KnightStep[from_sq];

      BISHOP:
        to_bb := b.bishop_attacks(from_sq);

      ROOK:
        to_bb := b.rook_attacks(from_sq);
    else
      assert(false, 'gen_checks: bad dc candidate');
    end;
    to_bb := to_bb and target;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      ms^.move := mk_move(from_sq, to_sq, p, b.squares[to_sq]);
      inc(ms);
      clear_lsb(to_bb);
    end;
  end;

  non_dc := not b.st^.pins[BLACK];
  pb := b.bb.b_pawns and non_dc;
  mask := target and b.bb.white and PawnAttacksBB[WHITE, ksq] and (not BB_Rank1);
  to_bb := pawn_west_b(pb) and mask;
  if to_bb <> 0 then begin
    to_sq := find_lsb(to_bb);
    from_sq := delta_sub(to_sq, DELTA_SW);
    ms^.move := mk_move(from_sq, to_sq, B_PAWN, b.squares[to_sq]);
    inc(ms);
    clear_lsb(to_bb);
    if to_bb <> 0 then begin
      to_sq := find_lsb(to_bb);
      from_sq := delta_sub(to_sq, DELTA_SW);
      ms^.move := mk_move(from_sq, to_sq, B_PAWN, b.squares[to_sq]);
      inc(ms);
    end;
  end;
  to_bb := pawn_east_b(pb) and mask;
  if to_bb <> 0 then begin
    to_sq := find_lsb(to_bb);
    from_sq := delta_sub(to_sq, DELTA_SE);
    ms^.move := mk_move(from_sq, to_sq, B_PAWN, b.squares[to_sq]);
    inc(ms);
    clear_lsb(to_bb);
    if to_bb <> 0 then begin
      to_sq := find_lsb(to_bb);
      from_sq := delta_sub(to_sq, DELTA_SE);
      ms^.move := mk_move(from_sq, to_sq, B_PAWN, b.squares[to_sq]);
      inc(ms);
    end;
  end;

  if b.bb.b_queens or b.bb.b_rooks <> 0 then
    orth := b.rook_attacks(ksq);
  if b.bb.b_queens or b.bb.b_bishops <> 0 then
    diag := b.bishop_attacks(ksq);

  lp := @b.piece_list.bq;
  if lp^ <> -1 then begin
    mask := (orth or diag) and target;
    repeat
      from_sq := TSquare(lp^);
      if AllDirBB[from_sq] and mask <> 0 then begin
        to_bb := b.queen_attacks(from_sq) and mask;
        while to_bb <> 0 do begin
          to_sq := find_lsb(to_bb);
          ms^.move := mk_move(from_sq, to_sq, B_QUEEN, b.squares[to_sq]);
          inc(ms);
          clear_lsb(to_bb);
        end;
      end;
      inc(lp);
    until lp^ = -1;
  end;

  lp := @b.piece_list.br;
  if lp^ <> -1 then begin
    mask := orth and target;
    repeat
      from_sq := TSquare(lp^);
      if SquareBB[from_sq] and non_dc <> 0 then
        if OrthogonalBB[from_sq] and mask <> 0 then begin
          to_bb := b.rook_attacks(from_sq) and mask;
          while to_bb <> 0 do begin
            to_sq := find_lsb(to_bb);
            ms^.move := mk_move(from_sq, to_sq, B_ROOK, b.squares[to_sq]);
            inc(ms);
            clear_lsb(to_bb);
          end;
        end;
      inc(lp);
    until lp^ = -1;
  end;

  lp := @b.piece_list.bb;
  if lp^ <> -1 then begin
    mask := diag and target;
    repeat
      from_sq := TSquare(lp^);
      if SquareBB[from_sq] and non_dc <> 0 then
        if DiagonalBB[from_sq] and mask <> 0 then begin
          to_bb := b.bishop_attacks(from_sq) and mask;
          while to_bb <> 0 do begin
            to_sq := find_lsb(to_bb);
            ms^.move := mk_move(from_sq, to_sq, B_BISHOP, b.squares[to_sq]);
            inc(ms);
            clear_lsb(to_bb);
          end;
        end;
      inc(lp);
    until lp^ = -1;
  end;

  lp := @b.piece_list.bn;
  if lp^ <> -1 then begin
    mask := KnightStep[ksq] and target;
    repeat
      from_sq := TSquare(lp^);
      if SquareBB[from_sq] and non_dc <> 0 then
        to_bb := KnightStep[from_sq] and mask;
        while to_bb <> 0 do begin
          to_sq := find_lsb(to_bb);
          ms^.move := mk_move(from_sq, to_sq, B_KNIGHT, b.squares[to_sq]);
          inc(ms);
          clear_lsb(to_bb);
        end;
      inc(lp);
    until lp^ = -1;
  end;

  if b.bb.w_king and not (BB_Ranks678 or BB_FileA) <> 0 then begin
    to_sq := delta_sub(ksq, DELTA_SE);
    if b.squares[to_sq] = NOPIECE then begin
      from_sq := delta_sub(to_sq, DELTA_S);
      if b.squares[from_sq] = B_PAWN then begin
        ms^.move := mk_move(from_sq, to_sq, B_PAWN);
        inc(ms);
      end
      else
        if b.squares[from_sq] = NOPIECE then
          if rank_of(from_sq) = RANK_6 then begin
            dec(from_sq, DELTA_S);
            if b.squares[from_sq] = B_PAWN then begin
              ms^.move := mk_dp(from_sq, to_sq, B_PAWN);
              inc(ms);
            end;
          end;
    end;
  end;

  if b.bb.w_king and not (BB_Ranks678 or BB_FileH) <> 0 then begin
    to_sq := delta_sub(ksq, DELTA_SW);
    if b.squares[to_sq] = NOPIECE then begin
      from_sq := delta_sub(to_sq, DELTA_S);
      if b.squares[from_sq] = B_PAWN then begin
        ms^.move := mk_move(from_sq, to_sq, B_PAWN);
        inc(ms);
      end
      else
        if b.squares[from_sq] = NOPIECE then
          if (rank_of(from_sq) = RANK_6) then begin
            dec(from_sq, DELTA_S);
            if b.squares[from_sq] = B_PAWN then begin
              ms^.move := mk_dp(from_sq, to_sq, B_PAWN);
              inc(ms);
            end;
          end;
    end;
  end;

  ms^.move64 := 0;
  result := ms;
end;

function gen_checks(var b: TBoard; ms: PMoveStack; target: TBitboard): PMoveStack;
begin
  if b.us = WHITE then
    result := gen_checks_w(b, ms, target)
  else
    result := gen_checks_b(b, ms, target)
end;

function gen_positional_w(var b: TBoard; ms: PMoveStack; target: TScore): PMoveStack;
var
  to_bb, empty: TBitboard;
  from_sq, to_sq, sq: TSquare;
  lp: PShortInt;
  score: TScore;
  move: TMove;
begin
  empty := b.bb.empty;

  lp := @b.piece_list.wn;
  while lp^ <> -1 do begin
    from_sq := TSquare(lp^);
    to_bb := KnightStep[from_sq] and empty;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      move := mk_move(from_sq, to_sq, W_KNIGHT);
      score := hist.gain(move);
      if score >= target then begin
        ms^.move := move;
        ms^.score := score;
        inc(ms);
      end;
      clear_lsb(to_bb);
    end;
    inc(lp);
  end;

  lp := @b.piece_list.wb;
  while lp^ <> -1 do begin
    from_sq := TSquare(lp^);
    to_bb := b.bishop_attacks(from_sq) and empty;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      move := mk_move(from_sq, to_sq, W_BISHOP);
      score := hist.gain(move);
      if score >= target then begin
        ms^.move := move;
        ms^.score := score;
        inc(ms);
      end;
      clear_lsb(to_bb);
    end;
    inc(lp);
  end;

  lp := @b.piece_list.wr;
  while lp^ <> -1 do begin
    from_sq := TSquare(lp^);
    to_bb := b.rook_attacks(from_sq) and empty;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      move := mk_move(from_sq, to_sq, W_ROOK);
      score := hist.gain(move);
      if score >= target then begin
        ms^.move := move;
        ms^.score := score;
        inc(ms);
      end;
      clear_lsb(to_bb);
    end;
    inc(lp);
  end;

  lp := @b.piece_list.wq;
  while lp^ <> -1 do begin
    from_sq := TSquare(lp^);
    to_bb := b.queen_attacks(from_sq) and empty;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      move := mk_move(from_sq, to_sq, W_QUEEN);
      score := hist.gain(move);
      if score >= target then begin
        ms^.move := move;
        ms^.score := score;
        inc(ms);
      end;
      clear_lsb(to_bb);
    end;
    inc(lp);
  end;

  from_sq := b.kingsq[WHITE];
  to_bb := KingStep[from_sq] and empty and not b.st^.b_attacks;
  while to_bb <> 0 do begin
    to_sq := find_lsb(to_bb);
    move := mk_move(from_sq, to_sq, W_KING);
    score := hist.gain(move);
    if score >= target then begin
      ms^.move := move;
      ms^.score := score;
      inc(ms);
    end;
    clear_lsb(to_bb);
  end;

  to_bb := (b.bb.w_pawns shl 8) and empty and not BB_Rank8;
  while to_bb <> 0 do begin
    to_sq := find_lsb(to_bb);
    if rank_of(to_sq) = RANK_3 then begin
      sq := delta_add(to_sq, DELTA_N);
      if b.squares[sq] = NOPIECE then begin
        move := mk_dp(delta_sub(to_sq, DELTA_N), sq, W_PAWN);
        score := hist.gain(move);
        if score >= target then begin
          ms^.move := move;
          ms^.score := score;
          inc(ms);
        end;
      end;
    end;
    move := mk_move(delta_sub(to_sq, DELTA_N), to_sq, W_PAWN);
    score := hist.gain(move);
    if score >= target then begin
      ms^.move := move;
      ms^.score := score;
      inc(ms);
    end;
    clear_lsb(to_bb);
  end;

  ms^.move64 := 0;
  result := ms;
end;

function gen_positional_b(var b: TBoard; ms: PMoveStack; target: TScore): PMoveStack;
var
  to_bb, empty: TBitboard;
  from_sq, to_sq, sq: TSquare;
  lp: PShortInt;
  score: TScore;
  move: TMove;
begin
  empty := b.bb.empty;

  lp := @b.piece_list.bn;
  while lp^ <> -1 do begin
    from_sq := TSquare(lp^);
    to_bb := KnightStep[from_sq] and empty;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      move := mk_move(from_sq, to_sq, B_KNIGHT);
      score := hist.gain(move);
      if score >= target then begin
        ms^.move := move;
        ms^.score := score;
        inc(ms);
      end;
      clear_lsb(to_bb);
    end;
    inc(lp);
  end;

  lp := @b.piece_list.bb;
  while lp^ <> -1 do begin
    from_sq := TSquare(lp^);
    to_bb := b.bishop_attacks(from_sq) and empty;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      move := mk_move(from_sq, to_sq, B_BISHOP);
      score := hist.gain(move);
      if score >= target then begin
        ms^.move := move;
        ms^.score := score;
        inc(ms);
      end;
      clear_lsb(to_bb);
    end;
    inc(lp);
  end;

  lp := @b.piece_list.br;
  while lp^ <> -1 do begin
    from_sq := TSquare(lp^);
    to_bb := b.rook_attacks(from_sq) and empty;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      move := mk_move(from_sq, to_sq, B_ROOK);
      score := hist.gain(move);
      if score >= target then begin
        ms^.move := move;
        ms^.score := score;
        inc(ms);
      end;
      clear_lsb(to_bb);
    end;
    inc(lp);
  end;

  lp := @b.piece_list.bq;
  while lp^ <> -1 do begin
    from_sq := TSquare(lp^);
    to_bb := b.queen_attacks(from_sq) and empty;
    while to_bb <> 0 do begin
      to_sq := find_lsb(to_bb);
      move := mk_move(from_sq, to_sq, B_QUEEN);
      score := hist.gain(move);
      if score >= target then begin
        ms^.move := move;
        ms^.score := score;
        inc(ms);
      end;
      clear_lsb(to_bb);
    end;
    inc(lp);
  end;

  from_sq := b.kingsq[BLACK];
  to_bb := KingStep[from_sq] and empty and not b.st^.w_attacks;
  while to_bb <> 0 do begin
    to_sq := find_lsb(to_bb);
    move := mk_move(from_sq, to_sq, B_KING);
    score := hist.gain(move);
    if score >= target then begin
      ms^.move := move;
      ms^.score := score;
      inc(ms);
    end;
    clear_lsb(to_bb);
  end;

  to_bb := (b.bb.b_pawns shr 8) and empty and not BB_Rank1;
  while to_bb <> 0 do begin
    to_sq := find_lsb(to_bb);
    if rank_of(to_sq) = RANK_6 then begin
      sq := delta_add(to_sq, DELTA_S);
      if b.squares[sq] = NOPIECE then begin
        move := mk_dp(delta_sub(to_sq, DELTA_S), sq, B_PAWN);
        score := hist.gain(move);
        if score >= target then begin
          ms^.move := move;
          ms^.score := score;
          inc(ms);
        end;
      end;
    end;
    move := mk_move(delta_sub(to_sq, DELTA_S), to_sq, B_PAWN);
    score := hist.gain(move);
    if score >= target then begin
      ms^.move := move;
      ms^.score := score;
      inc(ms);
    end;
    clear_lsb(to_bb);
  end;

  ms^.move64 := 0;
  result := ms;
end;

function zugzwang_danger(var board: TBoard): boolean;
var
  movable: integer;
  safe_mask, pins: TBitboard;
  lp: PShortInt;
  sq: TSquare;
  side: TSide;
begin
  result := false;
  side := board.us;

  pins := board.st^.pins[flip(side)];
  safe_mask := not (board.st^.attacks[flip(side)] or board.bb.pieces[side]);

  if KingStep[board.kingsq[side]] and safe_mask <> 0 then
    movable := 1
  else
    movable := 0;

  lp := @board.piece_list.knights[side];
  while lp^ <> -1 do begin
    sq := TSquare(lp^);
    if SquareBB[sq] and pins = 0 then
      if KnightStep[sq] and safe_mask <> 0 then begin
        if movable = 1 then exit;
        inc(movable);
      end;
    inc(lp);
  end;

  lp := @board.piece_list.bishops[side];
  while lp^ <> -1 do begin
    sq := TSquare(lp^);
    if SquareBB[sq] and pins = 0 then
      if board.bishop_attacks(sq) and safe_mask <> 0 then begin
        if movable = 1 then exit;
        inc(movable);
      end;
    inc(lp);
  end;

  lp := @board.piece_list.rooks[side];
  while lp^ <> -1 do begin
    sq := TSquare(lp^);
    if SquareBB[sq] and pins = 0 then
      if board.rook_attacks(sq) and safe_mask <> 0 then begin
        if movable = 1 then exit;
        inc(movable);
      end;
    inc(lp);
  end;

  lp := @board.piece_list.queens[side];
  while lp^ <> -1 do begin
    sq := TSquare(lp^);
    if SquareBB[sq] and pins = 0 then
      if board.queen_attacks(sq) and safe_mask <> 0 then begin
        if movable = 1 then exit;
        inc(movable);
      end;
    inc(lp);
  end;

  result := true;
end;

{$ifdef RangeCheckingWasOn}
  {$undef RangeCheckingWasOn}
  {$RangeChecks On}
{$endif}

procedure init_mvv_lva();
const
  pval: array [0..15] of byte = ( 1, 1, 2, 2, 7, 7, 3, 3, 4, 4, 5, 5, 6, 6, 0, 0 );
var
  attacker, victim: integer;
  v: TScore;
begin
  for attacker := 0 to 15 do
    for victim := 0 to 15 do begin
      v := pval[victim]*16 - pval[attacker];
      MVV_LVA[TPiece(attacker), TPiece(victim)] := v;
      MVV_LVA_M[16*victim + attacker] := v;
    end;
  MVV_LVA[W_KING, NOPIECE] := 16;
  MVV_LVA[B_KING, NOPIECE] := 16;
  MVV_LVA_M[ord(W_KING)] := 16;
  MVV_LVA_M[ord(B_KING)] := 16;
end;

function gen_captures(var b: TBoard; ms: PMoveStack; target: TBitboard): PMoveStack;
begin
  if b.us = WHITE then
    result := gen_captures_w(b, ms, target)
  else
    result := gen_captures_b(b, ms, target)
end;

function gen_evasions(var b: TBoard; ms: PMoveStack; target: TBitboard): integer;
begin
  assert(b.st^.checkersBB <> 0);
  if b.us = WHITE then
    result := gen_evasions_w(b, ms, target) - ms
  else
    result := gen_evasions_b(b, ms, target) - ms
end;

function gen_quiet(var b: TBoard; ms: PMoveStack): PMoveStack;
begin
  if b.us = WHITE then
    result := gen_quiet_w(b, ms)
  else
    result := gen_quiet_b(b, ms)
end;

procedure gen_positional(var b: TBoard; ms: PMoveStack; target: TScore);
begin
  if b.us = WHITE then
    gen_positional_w(b, ms, target)
  else
    gen_positional_b(b, ms, target)
end;

function gen_all(var b: TBoard; out ml:  TMoveList): integer;
var
  m: PMoveStack;
begin
  if b.us = WHITE then begin
    if b.st^.checkersBB <> 0 then
      result := gen_evasions_w(b, @ml, BB_Full) - PMoveStack(@ml)
    else begin
      m := gen_captures_w(b, @ml, b.bb.black);
      result := gen_quiet_w(b, m) - PMoveStack(@ml);
    end
  end
  else begin
    if b.st^.checkersBB <> 0 then
      result := gen_evasions_b(b, @ml, BB_Full) - PMoveStack(@ml)
    else begin
      m := gen_captures_b(b, @ml, b.bb.white);
      result := gen_quiet_b(b, m) - PMoveStack(@ml);
    end;
  end;
end;

function gen_legal_moves(var b: TBoard; out ml: TMoveList): integer;
var
  num_moves, num_legal, i: integer;
begin
  num_moves := gen_all(b, ml);

  if b.st^.checkersBB = 0 then begin
    num_legal := 0;
    for i := 0 to num_moves-1 do
      if b.is_legal(ml[i].move) then begin
        ml[num_legal] := ml[i];
        inc(num_legal);
      end;
    num_moves := num_legal;
  end;
  result := num_moves;
end;

initialization

init_mvv_lva();

end.

