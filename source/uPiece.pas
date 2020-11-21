
unit uPiece;
{$Z4}

interface

{$include platform.inc}

type
  TSide = ( WHITE, BLACK );

  TPieceType = ( PT_NONE, PAWN, KING, KNIGHT, BISHOP, ROOK, QUEEN, PT_7 );

  TPiece = (
    NOPIECE,  NOPIECE2,
    W_PAWN,   B_PAWN,
    W_KING,   B_KING,
    W_KNIGHT, B_KNIGHT,
    W_BISHOP, B_BISHOP,
    W_ROOK,   B_ROOK,
    W_QUEEN,  B_QUEEN,
    OCCUPIED, VACANT
  );

type
  TPieceCount = packed record
    case integer of
      0: (by_piece: array [TPiece] of byte);
      1: (w_np, b_np, wp, bp, wk, bk, wn, bn, wb, bb, wr, br, wq, bq: byte);
      2: (by_type: array [TPieceType, TSide] of byte);
      3: (np, pawn, king, knight, bishop, rook, queen: array [TSide] of byte);
  end;

const
  SLIDER_MASK = $08;
  W_PIECES = NOPIECE;
  B_PIECES = NOPIECE2;
  SideChar: array [TSide] of AnsiChar = 'wb';
  PieceChars: array [TPiece] of AnsiChar = '  PpKkNnBbRrQq';
  PieceNames: array [TPieceType] of PAnsiChar =
   ( '?', 'pawn', 'king', 'knight', 'bishop', 'rook', 'queen', '?' );

function flip(side: TSide): TSide; inline; overload;
function flip(piece: TPiece): TPiece; inline; overload;
function is_slider(piece: TPiece): boolean; inline;
function side_of(piece: TPiece): TSide; inline;
function type_of(piece: TPiece): TPieceType; inline;
function piece(pt: TPieceType; side: TSide): TPiece; inline;
function piece_to_char(p: TPiece): AnsiChar; inline;
function piece_to_upper(p: TPiece): AnsiChar; inline;
function piece_to_lower(p: TPiece): AnsiChar; inline;
function char_to_piece(c: AnsiChar): TPiece; inline;
function side_to_char(side: TSide): AnsiChar; inline;
function color_to_char(side: TSide): AnsiChar; inline;

procedure clear_piece_counts(out pcnt: TPieceCount); inline;

procedure init_piece();

var
  CharToPiece: array ['A'..'z'] of TPiece;

implementation

function side_of(piece: TPiece): TSide; inline;
begin
  result := TSide(integer(piece) and 1);
end;

function type_of(piece: TPiece): TPieceType; inline;
begin
  result := TPieceType(cardinal(piece) shr 1);
end;

function flip(side: TSide): TSide; inline;
begin
  result := TSide(cardinal(side) xor 1);
end;

function flip(piece: TPiece): TPiece; inline;
begin
  result := TPiece(cardinal(piece) xor 1);
end;

function is_slider(piece: TPiece): boolean; inline;
begin
  result := (cardinal(piece) and SLIDER_MASK) <> 0;
end;

function piece(pt: TPieceType; side: TSide): TPiece; inline;
begin
  result := TPiece((cardinal(pt) shl 1) or cardinal(side));
end;

function piece_to_char(p: TPiece): AnsiChar; inline;
begin
  result := PieceChars[p];
end;

function piece_to_upper(p: TPiece): AnsiChar; inline;
begin
  result := PieceChars[TPiece(cardinal(p) and not cardinal(BLACK))];
end;

function piece_to_lower(p: TPiece): AnsiChar; inline;
begin
  result := PieceChars[TPiece(cardinal(p) or cardinal(BLACK))];
end;

function char_to_piece(c: AnsiChar): TPiece;
begin
  if c in ['A'..'z'] then // Roland
    result := TPiece(CharToPiece[c])
  else
    result := NOPIECE;
end;

function side_to_char(side: TSide): AnsiChar; inline;
begin
  result := SideChar[side];
end;

function color_to_char(side: TSide): AnsiChar; inline;
begin
  result := SideChar[side];
end;

procedure clear_piece_counts(out pcnt: TPieceCount); inline;
var
  p: PIntegerArray;
begin
  p := PIntegerArray(@pcnt);
  p^[0] := 0;
  p^[1] := 0;
  p^[2] := 0;
  p^[3] := 0;
end;

procedure init_piece();
var
  p: TPiece;
begin
  fillchar(CharToPiece, sizeof(CharToPiece), 0);
  for p := W_PAWN to B_QUEEN do
    CharToPiece[piece_to_char(p)] := p;
end;

initialization

init_piece();

end.
