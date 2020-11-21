
unit uSquare; {$Z4}

interface

uses
  uPiece;

{$include platform.inc}

type
  TSquareEx = (
    SQUARE_NONE = -1,
    A1, B1, C1, D1, E1, F1, G1, H1,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A8, B8, C8, D8, E8, F8, G8, H8
  );
  
  TSquare = A1..H8;

  TRankEx = ( RANK_NONE = -1, RANK_1, RANK_2, RANK_3, RANK_4, RANK_5, RANK_6, RANK_7, RANK_8 );
  TFileEx = ( FILE_NONE = -1, FILE_A, FILE_B, FILE_C, FILE_D, FILE_E, FILE_F, FILE_G, FILE_H );

  TRank = RANK_1..RANK_8;
  TFile = FILE_A..FILE_H;

  TDelta = integer;

const
  DELTA_SSW = -17;
  DELTA_SS = -16;
  DELTA_SSE = -15;
  DELTA_SWW = -10;
  DELTA_SW = -9;
  DELTA_S = -8;
  DELTA_SE = -7;
  DELTA_SEE = -6;
  DELTA_W = -1;
  DELTA_ZERO = 0;
  DELTA_E = 1;
  DELTA_NWW = 6;
  DELTA_NW = 7;
  DELTA_N = 8;
  DELTA_NE = 9;
  DELTA_NEE = 10;
  DELTA_NNW = 15;
  DELTA_NN = 16;
  DELTA_NNE = 17;

  NO_EP = A1;
  PawnPush: array [TSide] of integer = ( DELTA_N, DELTA_S );
  PawnBase: array [TSide] of TRank = ( RANK_2, RANK_7 );
  FLIP_MASK = $38;

function flip(rank: TRank): TRank; overload; inline;
function flip(sq: TSquare): TSquare; overload; inline;
function mirror(sq: TSquare): TSquare; overload; inline;

function square(r: TRank; f: TFile): TSquare; inline;
function rank_of(sq: TSquare): TRank; inline;
function file_of(sq: TSquare): TFile; inline;
function same_rank(sq1, sq2: TSquare): integer; inline;
function same_file(sq1, sq2: TSquare): integer; inline;

function relative(sq: TSquare; side: TSide): TSquare; inline;
function relative_rank(sq: TSquare; side: TSide): TRank; overload; inline;
function relative_rank(r: TRank; side: TSide): TRank; overload; inline;

function is_rank_digit(c: AnsiChar): boolean; inline;
function is_file_letter(c: AnsiChar): boolean; inline;
function file_to_char(f: TFile): AnsiChar; inline;
function rank_to_char(r: TRank): AnsiChar; inline;
function char_to_file(c: AnsiChar): TFile; inline;
function char_to_rank(c: AnsiChar): TRank; inline;
function square_to_str(sq: TSquare; c: PAnsiChar): PAnsiChar; inline;

function file_distance(sq1, sq2: TSquare): integer; inline;
function rank_distance(sq1, sq2: TSquare): integer; inline;
function square_distance(sq1, sq2: TSquare): integer; inline;

function valid_file(f: TFile): boolean;
function valid_rank(r: TRank): boolean;
function valid_square(isq: integer): boolean;
function delta_add(sq: TSquare; delta: TDelta): TSquare; inline;
function delta_sub(sq: TSquare; delta: TDelta): TSquare; inline;

function min(const a, b: TSquare): TSquare; overload; inline;
function max(const a, b: TSquare): TSquare; overload; inline;
function min(const a, b: integer): integer; overload; inline;
function max(const a, b: integer): integer; overload; inline;
function min(const a, b: TRank): TRank; overload; inline;
function max(const a, b: TRank): TRank; overload; inline;

implementation

function flip(rank: TRank): TRank; inline;
begin
  result := TRank(cardinal(rank) xor 7);
end;

function flip(sq: TSquare): TSquare; inline;
begin
  result := TSquare(cardinal(sq) xor FLIP_MASK);
end;

function mirror(sq: TSquare): TSquare; inline;
begin
  result := TSquare(cardinal(sq) xor 7);
end;

function square(r: TRank; f: TFile): TSquare; inline;
begin
  result := TSquare(cardinal(r) shl 3 + cardinal(f));
end;

function file_of(sq: TSquare): TFile; inline;
begin
  result := TFile(cardinal(sq) and 7);
end;

function rank_of(sq: TSquare): TRank; inline;
begin
  result := TRank(cardinal(sq) shr 3);
end;

function same_rank(sq1, sq2: TSquare): integer; inline;
begin
  result := (cardinal(sq1) xor cardinal(sq2)) and $38;
end;

function same_file(sq1, sq2: TSquare): integer; inline;
begin
  result := (cardinal(sq1) xor cardinal(sq2)) and 7;
end;

function relative(sq: TSquare; side: TSide): TSquare; inline;
begin
  result := TSquare(cardinal(sq) xor (cardinal(side) * FLIP_MASK));
end;

function relative_rank(sq: TSquare; side: TSide): TRank; overload; inline;
begin
  result := rank_of(relative(sq, side));
end;

function relative_rank(r: TRank; side: TSide): TRank; overload; inline;
begin
  result := TRank(cardinal(r) xor (cardinal(side) * 7));
end;

function is_rank_digit(c: AnsiChar): boolean; inline;
begin
  result := (c in ['1'..'8']);
end;

function is_file_letter(c: AnsiChar): boolean; inline;
begin
  result := (c in ['a'..'h']);
end;

function file_to_char(f: TFile): AnsiChar; inline;
begin
  result := AnsiChar(ord('a') + integer(f));
end;

function rank_to_char(r: TRank): AnsiChar; inline;
begin
  result := AnsiChar(ord('1') + integer(r));
end;

function char_to_file(c: AnsiChar): TFile; inline;
begin
  result := TFile(ord(c) - ord('a'));
end;

function char_to_rank(c: AnsiChar): TRank; inline;
begin
  result := TRank(ord(c) - ord('1'));
end;

function file_distance(sq1, sq2: TSquare): integer; inline;
begin
  result := abs(integer(file_of(sq1)) - integer(file_of(sq2)));
end;

function rank_distance(sq1, sq2: TSquare): integer; inline;
begin
  result := abs(integer(integer(sq1) shr 3) - integer(integer(sq2) shr 3));
end;

function square_distance(sq1, sq2: TSquare): integer; inline;
begin
  result := max(rank_distance(sq1, sq2), file_distance(sq1, sq2));
end;

function square_to_str(sq: TSquare; c: PAnsiChar): PAnsiChar;
begin
  c[0] := file_to_char(file_of(sq));
  c[1] := rank_to_char(rank_of(sq));
  c[2] := #0;
  result := c;
end;

function valid_rank(r: TRank): boolean;
begin
  result := (integer(r) in [0..7]);
end;

function valid_file(f: TFile): boolean;
begin
  result := (integer(f) in [0..7]);
end;

function valid_square(isq: integer): boolean;
begin
  result := isq in [0 .. 63];
end;

function delta_add(sq: TSquare; delta: TDelta): TSquare; inline;
begin
  result := TSquare(integer(sq) + delta);
end;

function delta_sub(sq: TSquare; delta: TDelta): TSquare; inline;
begin
  result := TSquare(integer(sq) - delta);
end;

function max(const a, b: integer): integer;
begin
  if a > b then result := a else result := b;
end;

function min(const a, b: integer): integer;
begin
  if a < b then result := a else result := b;
end;

function max(const a, b: TSquare): TSquare;
begin
  if a > b then result := a else result := b;
end;

function min(const a, b: TSquare): TSquare;
begin
  if a < b then result := a else result := b;
end;

function min(const a, b: TRank): TRank;
begin
  if a < b then result := a else result := b;
end;

function max(const a, b: TRank): TRank;
begin
  if a > b then result := a else result := b;
end;


end.
