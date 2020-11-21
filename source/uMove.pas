
unit uMove;
{$Z4}

interface

uses
  uPiece, uScore, uSquare;

{$include platform.inc}

const
  NOMOVE   = 0;

type
  TMove = NOMOVE .. $4ffffff;
  PMove = ^TMove;

const
  SHIFT_TO        = 6;
  SHIFT_PIECE     = 12;
  SHIFT_VICTIM    = 16;
  SHIFT_PROMOTION = 20;

  MASK_SQUARE      = $3f;
  MASK_PIECE       = $0f;
  MASK_PTYPE       = MASK_PIECE xor 1;
  MASK_FROMTO      = $0fff;
  MASK_PIECEFROMTO = $ffff;

  MF_CASTLE     = $01000000;
  MF_ENPASSANT  = $02000000;
  MF_DOUBLEPUSH = $04000000;
  MF_PROMOTION  = MASK_PIECE shl SHIFT_PROMOTION;
  MF_SPECIAL    = MF_CASTLE or MF_ENPASSANT or MF_PROMOTION;
  MF_CAPTURE    = MASK_PIECE shl SHIFT_VICTIM;

  QUIET_MASK    = MF_CAPTURE or MF_ENPASSANT or MF_PROMOTION;
  SLIDING_MASK  = SLIDER_MASK shl SHIFT_PIECE;

  MAX_MOVES = 256;

type
  PMoveStack = ^TMoveStack;
  TMoveStack = record
    case integer of
      0: (move: TMove; score: TScore);
      1: (move64: uint64);
  end;
  TMoveList = array [0..MAX_MOVES-1] of TMoveStack;
  PMoveList = ^TMoveList;
  TMoveString = array [0..11] of AnsiChar;

function source_sq(m: TMove): TSquare; inline;
function target_sq(m: TMove): TSquare; inline;
function fromto(m: TMove): cardinal; inline;
function piece_moved(m: TMove): TPiece; inline;
function capture_victim(m: TMove): TPiece; inline;
function promoted_to(m: TMove): TPiece; inline;

function is_capture(m: TMove): boolean; inline;
function is_enpassant(m: TMove): boolean; inline;
function is_castle(m: TMove): boolean; inline;
function is_doublepush(m: TMove): boolean; inline;
function is_promotion(m: TMove): boolean; inline;
function is_special(m: TMove): boolean; inline;
function is_quiet(m: TMove): boolean; inline;
function is_pawn_push(m: TMove): boolean; inline;
function is_pawn_move(m: TMove): boolean; inline;
function is_pawn_capture(m: TMove): boolean; inline;
function is_king_move(m: TMove): boolean; inline;
function non_king_move(m: TMove): boolean; inline;
function is_np_capture(m: TMove): boolean; inline;
function is_sliding(m: TMove): boolean; inline;
function reversed(m: TMove): TMove; inline;

function mk_move(f, t: TSquare; p: TPiece): TMove; overload; inline;
function mk_move(f, t: TSquare; p, victim: TPiece): TMove; overload; inline;
function mk_move(f, t: TSquare; p, victim, promo: TPiece): TMove; overload; inline;
function mk_ep(f, t: TSquare; p, victim: TPiece): TMove; inline;
function mk_promotion(move: TMove; promo: TPiece): TMove; inline;
function mk_castle(f, t: TSquare; p: TPiece): TMove; inline;
function mk_dp(f, t: TSquare; p: TPiece): TMove; inline;

function move_to_string(m: TMove; c: PAnsiChar): PAnsiChar;
function move_to_notation(m: TMove; c: PAnsiChar): PAnsiChar;

var
  chess_960: boolean = false;
  cmove: TMoveString;

implementation

uses
  uSystem;

function source_sq(m: TMove): TSquare; inline;
begin
  result := TSquare(m and MASK_SQUARE);
end;

function target_sq(m: TMove): TSquare; inline;
begin
  result := TSquare((m shr SHIFT_TO) and MASK_SQUARE);
end;

function fromto(m: TMove): cardinal; inline;
begin
  result := m and MASK_FROMTO;
end;

function piece_moved(m: TMove): TPiece; inline;
begin
  result := TPiece((m shr SHIFT_PIECE) and MASK_PIECE);
end;

function capture_victim(m: TMove): TPiece; inline;
begin
  result := TPiece((m shr SHIFT_VICTIM) and MASK_PIECE);
end;

function promoted_to(m: TMove): TPiece; inline;
begin
  result := TPiece((m shr SHIFT_PROMOTION) and MASK_PIECE);
end;

function is_capture(m: TMove): boolean; inline;
begin
  result := (m and MF_CAPTURE) <> 0;
end;

function is_enpassant(m: TMove): boolean; inline;
begin
  result := (m and MF_ENPASSANT) <> 0;
end;

function is_castle(m: TMove): boolean; inline;
begin
  result := (m and MF_CASTLE) <> 0;
end;

function is_promotion(m: TMove): boolean; inline;
begin
  result := (m and MF_PROMOTION) <> 0;
end;

function is_doublepush(m: TMove): boolean; inline;
begin
  result := (m and MF_DOUBLEPUSH) <> 0;
end;

function is_special(m: TMove): boolean; inline;
begin
  result := (m and MF_SPECIAL) <> 0;
end;

function is_quiet(m: TMove): boolean; inline;
begin
  result := (m and QUIET_MASK) = 0;
end;

function is_pawn_push(m: TMove): boolean; inline;
const
  MASK = MF_PROMOTION or (MASK_PTYPE shl SHIFT_PIECE);
  PAWN = cardinal(W_PAWN) shl SHIFT_PIECE;
begin
  result := (m and MASK) = PAWN;
end;

function is_pawn_move(m: TMove): boolean; inline;
const
  MASK = MASK_PTYPE shl SHIFT_PIECE;
  PAWN = cardinal(W_PAWN) shl SHIFT_PIECE;
begin
  result := (m and MASK) = PAWN;
end;

function is_pawn_capture(m: TMove): boolean; inline;
const
  MOVE_MASK = MASK_PTYPE shl SHIFT_VICTIM;
  PAWN_MASK = cardinal(W_PAWN) shl SHIFT_VICTIM;
begin
  result := (m and MOVE_MASK) = PAWN_MASK;
end;

function is_king_move(m: TMove): boolean; inline;
const
  MOVE_MASK = MASK_PTYPE shl SHIFT_PIECE;
  KING_MASK = cardinal(W_KING) shl SHIFT_PIECE;
begin
  result := (m and MOVE_MASK) = KING_MASK;
end;

function non_king_move(m: TMove): boolean; inline;
const
  MOVE_MASK = MASK_PTYPE shl SHIFT_PIECE;
  KING_MASK = cardinal(W_KING) shl SHIFT_PIECE;
begin
  result := (m and MOVE_MASK) <> KING_MASK;
end;

function is_np_capture(m: TMove): boolean; inline;
const
  MOVE_MASK = MASK_PIECE shl SHIFT_VICTIM;
  PAWN_MASK = cardinal(B_PAWN) shl SHIFT_VICTIM;
begin
  result := (m and MOVE_MASK) > PAWN_MASK;
end;

function is_sliding(m: TMove): boolean; inline;
begin
  result := m and SLIDING_MASK <> 0;
end;

function reversed(m: TMove): TMove; inline;
begin
  result :=
       (m and not MASK_FROMTO)
    or ((m shr SHIFT_TO) and MASK_SQUARE)
    or ((m and MASK_SQUARE) shl SHIFT_TO);
end;

function mk_move(f, t: TSquare; p: TPiece): TMove;
begin
  result := cardinal(p) shl SHIFT_PIECE
    or cardinal(t) shl SHIFT_TO
    or cardinal(f);
end;

function mk_move(f, t: TSquare; p, victim: TPiece): TMove;
begin
  result := cardinal(p) shl SHIFT_PIECE
    or cardinal(victim) shl SHIFT_VICTIM
    or cardinal(t) shl SHIFT_TO
    or cardinal(f);
end;

function mk_move(f, t: TSquare; p, victim, promo: TPiece): TMove;
begin
  result := cardinal(promo) shl SHIFT_PROMOTION
    or cardinal(victim) shl SHIFT_VICTIM
    or cardinal(p) shl SHIFT_PIECE
    or cardinal(t) shl SHIFT_TO
    or cardinal(f);
end;

function mk_ep(f, t: TSquare; p, victim: TPiece): TMove;
begin
  result := MF_ENPASSANT
    or cardinal(p) shl SHIFT_PIECE
    or cardinal(victim) shl SHIFT_VICTIM
    or cardinal(t) shl SHIFT_TO
    or cardinal(f);
end;

function mk_promotion(move: TMove; promo: TPiece): TMove;
begin
  result := move or cardinal(promo) shl SHIFT_PROMOTION;
end;

function mk_castle(f, t: TSquare; p: TPiece): TMove;
begin
  result := MF_CASTLE
    or cardinal(p) shl SHIFT_PIECE
    or cardinal(t) shl SHIFT_TO
    or cardinal(f);
end;

function mk_dp(f, t: TSquare; p: TPiece): TMove;
begin
  result := MF_DOUBLEPUSH
    or cardinal(p) shl SHIFT_PIECE
    or cardinal(t) shl SHIFT_TO
    or cardinal(f);
end;

function move_to_string(m: TMove; c: PAnsiChar): PAnsiChar;
var
  from_sq, to_sq: TSquare;
begin
  result := c;
  if m = NOMOVE then
  begin
    strcpy(c, 'null');
    exit;
  end;

  from_sq := source_sq(m);
  to_sq := target_sq(m);

  if is_castle(m) and not Chess_960 then
  begin
    if from_sq > to_sq then
      if from_sq = E1 then
        strcpy(c, 'e1c1')
      else
        strcpy(c, 'e8c8')
    else
      if from_sq = E1 then
        strcpy(c, 'e1g1')
      else
        strcpy(c, 'e8g8')
  end else
  begin
    square_to_str(from_sq, @c[0]);
    square_to_str(to_sq, @c[2]);
    c[4] := #0; c[5]:= #0;
    if is_promotion(m) then
      c[4] := piece_to_lower(promoted_to(m));
  end;
end;

function move_to_notation(m: TMove; c: PAnsiChar): PAnsiChar;
var
  from_sq, to_sq: TSquare;
begin
  result := c;
  if (m = NOMOVE) then begin
    strcpy(c, 'null');
    exit;
  end;

  from_sq := source_sq(m);
  to_sq := target_sq(m);

  if is_castle(m) then begin
    if to_sq > from_sq then
      strcpy(c, 'O-O')
    else
      strcpy(c, 'O-O-O');
    exit;
  end;

  if type_of(piece_moved(m)) <> PAWN then begin
    c^ := piece_to_upper(piece_moved(m));
    inc(c);
  end;

  square_to_str(from_sq, @c[0]); inc(c, 2);
  if is_capture(m) or is_enpassant(m) then begin
    c^ := 'x';
    inc(c);
  end;
  square_to_str(to_sq, @c[0]); inc(c, 2);

  if is_promotion(m) then begin
    c[0] := '=';
    c[1] := piece_to_upper(promoted_to(m));
    inc(c, 2);
  end;

  c^ := #0;
  if is_enpassant(m) then
    strcpy(c, '/ep');
end;

end.
