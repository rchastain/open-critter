
unit uSee;
{$Z4}

interface

uses
  uBitboard, uBoard, uMove, uPiece, uSquare;

{$include platform.inc}

function bad_see(var board: TBoard; move: TMove): boolean;

const
  SEE_VPAWN   = 1;
  SEE_VKNIGHT = 3;
  SEE_VBISHOP = 3;
  SEE_VROOK   = 5;
  SEE_VQUEEN  = 9;
  SEE_VKING   = 99;

  SEE_VALUE: array [TPiece] of integer = (
    0, 0,
    SEE_VPAWN, SEE_VPAWN, SEE_VKING, SEE_VKING, SEE_VKNIGHT, SEE_VKNIGHT,
    SEE_VBISHOP, SEE_VBISHOP, SEE_VROOK, SEE_VROOK, SEE_VQUEEN, SEE_VQUEEN,
    0, 0
  );

implementation

uses
  uNotation, uSystem;

function bad_see(var board: TBoard; move: TMove): boolean;
var
  from_sq, to_sq: TSquare;
  piece_val, capt_val, hanging: integer;
  not_pinned, attackers, occ, mask, b: TBitboard;
  queens_rooks, queens_bishops: TBitboard;
  sq, pinner_sq: TSquare;
  st: PStateInfo;
  us, they: TSide;
begin
  from_sq := source_sq(move);
  to_sq := target_sq(move);

  piece_val := SEE_VALUE[piece_moved(move)];
  capt_val := SEE_VALUE[capture_victim(move)];
  hanging := piece_val - capt_val;

  if hanging <= 0 then begin
    result := false;
    exit;
  end;

  us := board.us;
  they := flip(us);
  st := board.st;

  b := st^.pins[us] and board.bb.pieces[they];
  not_pinned := TBB(-1);
  while b <> 0 do begin
    sq := find_lsb(b);
    pinner_sq := TSquare(st^.pinners[us, sq]);
    if (pinner_sq <> from_sq) then
      if (AbsDir[to_sq, sq] <> AbsDir[sq, board.kingsq[they]]) then
        not_pinned := not_pinned xor SquareBB[sq];
    clear_lsb(b);
  end;

  if hanging > SEE_VPAWN then
    if PawnAttacksBB[us, to_sq] and board.bb.pawns[they] and not_pinned <> 0 then begin
      result := true;
      exit;
    end;

  attackers := (board.bb.w_knights or board.bb.b_knights) and KnightStep[to_sq] and not_pinned;

  if hanging > SEE_VKNIGHT then
    if board.bb.knights[they] and attackers <> 0 then begin
      result := true;
      exit;
    end;

  with board.bb do
    queens_bishops := (w_bishops or b_bishops or w_queens or b_queens) and not_pinned;

  occ := board.bb.occupied xor SquareBB[from_sq];

  attackers := attackers or (bishop_attacks(to_sq, occ) and queens_bishops);

  if hanging > SEE_VBISHOP then
    if board.bb.bishops[they] and attackers <> 0 then begin
      result := true;
      exit;
    end;

  with board.bb do
    queens_rooks := (w_rooks or b_rooks or w_queens or b_queens) and not_pinned;

  attackers := attackers or (rook_attacks(to_sq, occ) and queens_rooks);

  if hanging > SEE_VROOK then
    if board.bb.rooks[they] and attackers <> 0 then begin
      result := true;
      exit;
    end;

  attackers := attackers or (KingStep[to_sq] and (board.bb.w_king or board.bb.b_king));
  attackers := attackers or (PawnAttacksBB[they, to_sq] and board.bb.pawns[us]);
  attackers := attackers or (PawnAttacksBB[us, to_sq] and board.bb.pawns[they] and not_pinned);
  attackers := attackers and occ;

  dec(capt_val, piece_val);
  repeat
    b := board.bb.pawns[they] and attackers;
    if b <> 0 then begin
      occ := occ xor (b and not (b - 1));
      piece_val := SEE_VPAWN;
      attackers := attackers or (bishop_attacks(to_sq, occ) and queens_bishops);
      attackers := attackers and occ;
    end
    else begin
      b := board.bb.knights[they] and attackers;
      if b <> 0 then begin
        attackers := attackers xor (b and not (b - 1));
        piece_val := SEE_VKNIGHT;
      end
      else begin
        b := board.bb.bishops[they] and attackers;
        if b <> 0 then begin
          occ := occ xor (b and not (b - 1));
          piece_val := SEE_VBISHOP;
          attackers := attackers or
            (bishop_attacks(to_sq, occ) and queens_bishops);
          attackers := attackers and occ;
        end
        else begin
          b := board.bb.rooks[they] and attackers;
          if b <> 0 then begin
            occ := occ xor (b and not (b - 1));
            piece_val := SEE_VROOK;
            attackers := attackers or
              (rook_attacks(to_sq, occ) and queens_rooks);
            attackers := attackers and occ;
          end
          else begin
            b := board.bb.queens[they] and attackers;
            if b <> 0 then begin
              mask := b and not (b - 1);
              piece_val := SEE_VQUEEN;
              occ := occ xor mask;
              if mask and OrthogonalBB[to_sq] <> 0 then
                attackers := attackers or
                  (rook_attacks(to_sq, occ) and queens_rooks)
              else
                attackers := attackers or
                  (bishop_attacks(to_sq, occ) and queens_bishops);
              attackers := attackers and occ;
            end
            else begin
              b := board.bb.king[they] and attackers;
              if b = 0 then begin
                result := false;
                exit;
              end;
              piece_val := SEE_VKING * 2;
            end;
          end;
        end;
      end;
    end;

    inc(capt_val, piece_val);
    if (capt_val < 0) then begin
      result := true;
      exit;
    end;
    b := board.bb.pawns[us] and attackers;
    if b <> 0 then begin
      occ := occ xor (b and not (b - 1));
      piece_val := SEE_VPAWN;
      attackers := attackers or (bishop_attacks(to_sq, occ) and queens_bishops);
      attackers := attackers and occ;
    end
    else begin
      b := board.bb.knights[us] and attackers;
      if b <> 0 then begin
        attackers := attackers xor (b and not (b - 1));
        piece_val := SEE_VKNIGHT;
      end
      else begin
        b := board.bb.bishops[us] and attackers;
        if b <> 0 then begin
          occ := occ xor (b and not (b - 1));
          piece_val := SEE_VBISHOP;
          attackers := attackers or
            (bishop_attacks(to_sq, occ) and queens_bishops);
          attackers := attackers and occ;
        end
        else begin
          b := board.bb.rooks[us] and attackers;
          if b <> 0 then begin
            occ := occ xor (b and not (b - 1));
            piece_val := SEE_VROOK;
            attackers := attackers or
              (rook_attacks(to_sq, occ) and queens_rooks);
            attackers := attackers and occ;
          end
          else begin
            b := board.bb.queens[us] and attackers;
            if b <> 0 then begin
              mask := b and not (b - 1);
              piece_val := SEE_VQUEEN;
              occ := occ xor mask;
              if mask and OrthogonalBB[to_sq] <> 0 then
                attackers := attackers or
                  (rook_attacks(to_sq, occ) and queens_rooks)
              else
                attackers := attackers or
                  (bishop_attacks(to_sq, occ) and queens_bishops);
              attackers := attackers and occ;
            end
            else begin
              b := board.bb.king[us] and attackers;
              if b = 0 then begin
                result := true;
                exit;
              end;
              if capt_val > SEE_VKING then begin
                result := false;
                exit;
              end;
              piece_val := SEE_VKING * 3;
            end;
          end;
        end;
      end;
    end;

    dec(capt_val, piece_val);

  until capt_val >= 0;

  result := false;
end;

function see_test(var board: TBoard; fen: PAnsiChar; san: PAnsiChar;
  expected_result: boolean): boolean; overload;
var
  move: TMove;
  see: boolean;
begin
  board.load_fen(fen);
  move := san_to_move(san, board);
  see := bad_see(board, move);
  result := see = expected_result;
  if not result then begin
    printf('Bad SEE result!' + LineEnding);
    printf('FEN: %s' + LineEnding, fen);
    printf('Move: %s' + LineEnding, san);
  end;
end;

end.
