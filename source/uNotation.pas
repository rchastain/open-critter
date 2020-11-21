
unit uNotation;

interface

uses
  uBitboard, uBoard, uMove, uSquare, uPiece;

{$include platform.inc}

function move_to_san(m: TMove; var board: TBoard; c: PAnsiChar): PAnsiChar;
function san_to_move(const san: PAnsiChar; var board: TBoard): TMove;

implementation

uses
  uMovegen, uScore, uSystem, uUtil;

function move_to_san(m: TMove; var board: TBoard; c: PAnsiChar): PAnsiChar;
var
  attackers, samepieces: TBitboard;
  from_sq, to_sq: TSquare;
  u: TStateInfo;
  ml: TMoveList;
  p: TPiece;
begin
  result := c;

  from_sq := source_sq(m);
  to_sq := target_sq(m);

  if is_castle(m) then begin
    if to_sq > from_sq then begin
      strcpy(c, 'O-O');
      inc(c, 3);
    end
    else begin
      strcpy(c, 'O-O-O');
      inc(c, 5);
    end;
  end
  else begin
    p := piece_moved(m);
    if (p > B_PAWN) then begin
      c^ := piece_to_upper(p); inc(c);
      attackers := board.attackers(to_sq, board.us);
      samepieces := board.bb.piece[p] xor SquareBB[from_sq];
      if attackers and samepieces <> 0 then begin
        if samepieces and FileBB[file_of(from_sq)] = 0 then begin
          c^ := file_to_char(file_of(from_sq));
          inc(c);
        end
        else if samepieces and RankBB[rank_of(from_sq)] = 0 then begin
          c^ := rank_to_char(rank_of(from_sq));
          inc(c);
        end
        else begin
          square_to_str(from_sq, @c[0]);
          inc(c, 2);
        end;
      end;
    end
    else
      if is_capture(m) then begin
        c^ := file_to_char(file_of(from_sq));
        inc(c);
      end;

    if is_capture(m) then begin
      c^ := 'x';
      inc(c);
    end;

    square_to_str(to_sq, @c[0]); inc(c, 2);

    if is_promotion(m) then begin
      c[0] := '=';
      c[1] := piece_to_upper(promoted_to(m));
      inc(c, 2);
    end;

    if is_enpassant(m) then begin
      strcpy(c, '/ep');
      inc(c, 3);
    end;
  end;

  if board.is_check_full(m) then begin
    board.make_move_fast(m, u);
    gen_evasions(board, @ml, TBB(BB_Full));
    board.undo_move(m);
    if ml[0].move <> NOMOVE then
      c^ := '+'
    else
      c^ := '#';
    inc(c);
  end;
  c^ := #0;
end;

function alphanum(c: AnsiChar): boolean; inline;
begin
  result := c in ['0'..'9', 'a'..'z', 'A'..'Z'];
end;

function san_to_move(const san: PAnsiChar; var board: TBoard): TMove;
var
  tmp: TMoveString;
  from_sq, to_sq: TSquareEx;
  f: TFileEx; r: TRankEx;
  p, promotion, victim: TPiece;
  ptype: TPieceType;
  bb, nonpawns: TBitboard;
  move: TMove;
  us: TSide;
  s: PAnsiChar;
  i, j: integer;
  c: AnsiChar;
begin
  {$ifdef debug}
  printf({$I %FILE%} + ' ' + {$I %LINE%} + ' san_to_move(%s)' + LineEnding, san);
  {$endif}
  i := 0;
  j := 0;
  while (san[i] <> #0) and (i < 9) do
  begin
    if alphanum(san[i]) and (san[i] <> 'x') then
    begin
      tmp[j] := san[i];
      inc(j);
    end;
    inc(i);
  end;
  tmp[j] := #0;
  us := board.us;
  result := NOMOVE;

  s := @tmp;
  if stricmp(s, 'OO') then
  begin
    from_sq := board.start_square(START_KING);
    to_sq := board.start_square(START_ROOK_H);
    result := mk_castle(from_sq, to_sq, piece(KING, us));
    exit;
  end;
  if stricmp(s, 'OOO') then
  begin
    from_sq := board.start_square(START_KING);
    to_sq := board.start_square(START_ROOK_A);
    result := mk_castle(from_sq, to_sq, piece(KING, us));
    exit;
  end;

  ptype := PAWN;
  f := TFileEx(-1);
  r := TRankEx(-1);
  from_sq := TSquareEx(-1);
  to_sq := TSquareEx(-1);
  
  c := s^; inc(s);
  if c in ['A'..'Z'] then
  begin
    ptype := type_of(char_to_piece(c));
    c := s^; inc(s);
    if (ord(ptype) = 0) or (c = #0) then
      exit;
  end;

  if is_file_letter(c) then
  begin
    f := char_to_file(c);
    c := s^; inc(s);
    if c = #0 then
      exit;
    if is_rank_digit(c) then
    begin
      r := char_to_rank(c);
      to_sq := square(r, f);
      r := TRankEx(-1);
      f := TFileEx(-1);
      c := s^; inc(s);
    end;
  end else
  begin
    if not is_rank_digit(c) then
      exit;
    r := char_to_rank(c);
    c := s^; inc(s);
  end;

  if (c <> #0) and (s^ <> #0) then
    if is_file_letter(c) and is_rank_digit(s^) then
    begin
      from_sq := to_sq;
      to_sq := square(char_to_rank(s^), char_to_file(c));
      c := s[1];
    end;

  promotion := char_to_piece(c);
  if type_of(promotion) in [KNIGHT, BISHOP, ROOK, QUEEN] then
    promotion := piece(type_of(promotion), us)
  else
    promotion := NOPIECE;

  if to_sq = TSquareEx(-1) then
    exit;

  if from_sq <> TSquareEx(-1) then
  begin
    p := board.squares[from_sq];
    ptype := type_of(p);
    if p = piece(KING, us) then
    begin
      if ((Distance[from_sq, to_sq] > 1) and (rank_of(from_sq) = rank_of(to_sq)))
      or (board.squares[to_sq] = piece(ROOK, us)) then
      begin
        if from_sq > to_sq then
          to_sq := board.start_square(START_ROOK_A)
        else
          to_sq := board.start_square(START_ROOK_H);
        result := mk_castle(from_sq, to_sq, p);
        exit;
      end;
    end;
  end else
    p := piece(ptype, us);

  if ptype = PAWN then
  begin
    if (board.st^.epsq <> NO_EP) and (to_sq = board.st^.epsq) then
    begin
      if from_sq = TSquareEx(-1) then
      begin
        if f = TFileEx(-1) then
          exit;
        r := TRank(integer(rank_of(to_sq)) - sign[us]);
        from_sq := square(r, f);
      end;
      result := mk_ep(from_sq, to_sq, p, flip(p));
      exit;
    end;
    if from_sq = TSquareEx(-1) then
    begin
      if board.squares[to_sq] <> NOPIECE then
      begin
        if f = TFileEx(-1) then
          exit;
        r := TRank(integer(rank_of(to_sq)) - sign[us]);
        from_sq := square(r, f);
      end else
      begin
        from_sq := delta_sub(to_sq, PawnPush[us]);
        if board.squares[from_sq] <> p then
        begin
          if rank_of(from_sq) <> relative_rank(RANK_3, us) then
            exit;
          dec(from_sq, PawnPush[us]);
          if board.squares[from_sq] <> p then
            exit;
        end;
      end;
    end;

    if rank_of(relative(to_sq, us)) <= rank_of(relative(from_sq, us)) then
      exit;

    if abs(integer(from_sq) - integer(to_sq)) > 9 then
    begin
      result := mk_dp(from_sq, to_sq, p);
      exit;
    end;

    victim := board.squares[to_sq];
    if file_of(from_sq) = file_of(to_sq) then
    begin
      if victim <> NOPIECE then
        exit;
    end else
      if (victim = NOPIECE) or (side_of(victim) = us) then
        exit;

    if rank_of(to_sq) = relative_rank(RANK_8, us) then
    begin
      if promotion = NOPIECE then
        promotion := piece(QUEEN, us);
      result := mk_move(from_sq, to_sq, p, board.squares[to_sq], promotion);
      exit;
    end;
    result := mk_move(from_sq, to_sq, p, board.squares[to_sq]);
    exit;
  end;

  if from_sq = TSquareEx(-1) then
  begin
    nonpawns := not (board.bb.w_pawns or board.bb.b_pawns);
    bb := board.bb.piece[p] and board.attackers(to_sq, us) and nonpawns;
    while bb <> 0 do
    begin
      from_sq := find_lsb(bb);
      clear_lsb(bb);
      if (r <> TRankEx(-1)) and (r <> rank_of(from_sq))
      or (f <> TFileEx(-1)) and (f <> file_of(from_sq)) then
        continue;
      move := mk_move(from_sq, to_sq, p, board.squares[to_sq]);
      if board.hm_is_ok(move) then
      begin
        result := move;
        exit;
      end;
    end;
    exit;
  end;

  victim := board.squares[to_sq];
  if (victim <> NOPIECE) and (side_of(victim) = us) then
    exit;

  if (p = NOPIECE) or not board.can_reach(p, from_sq, to_sq) then
    exit;

  result := mk_move(from_sq, to_sq, p, board.squares[to_sq]);
end;

end.
