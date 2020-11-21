
unit uBitboard;
{$Z4}

interface

uses
  uPiece, uMagic, uSquare;

{$include platform.inc}

type
  TBB = uint64;
  TBitboard = uint64;
  PBitboard = ^TBitboard;
  TBitboardPair = array [TSide] of TBitboard;

const
  ASM_FIND_LSB = 1;

  BB_Empty    = TBB($0000000000000000);
  BB_Full     = TBB($ffffffffffffffff);

  BB_FileA    = TBB($0101010101010101);
  BB_FileB    = TBB($0202020202020202);
  BB_FileC    = TBB($0404040404040404);
  BB_FileD    = TBB($0808080808080808);
  BB_FileE    = TBB($1010101010101010);
  BB_FileF    = TBB($2020202020202020);
  BB_FileG    = TBB($4040404040404040);
  BB_FileH    = TBB($8080808080808080);

  BB_Rank1    = TBB($00000000000000ff);
  BB_Rank2    = TBB($000000000000ff00);
  BB_Rank3    = TBB($0000000000ff0000);
  BB_Rank4    = TBB($00000000ff000000);
  BB_Rank5    = TBB($000000ff00000000);
  BB_Rank6    = TBB($0000ff0000000000);
  BB_Rank7    = TBB($00ff000000000000);
  BB_Rank8    = TBB($ff00000000000000);

  BB_OutpostW = TBB($00007e7e7e000000);
  BB_OutpostB = TBB($0000007e7e7e0000);

  BB_Light    = TBB($55aa55aa55aa55aa);
  BB_Dark     = TBB($aa55aa55aa55aa55);

  BB_Ranks18  = TBB($ff000000000000ff);
  BB_FilesAH  = TBB($8181818181818181);
  BB_Corners  = TBB($8100000000000081);
  BB_Edge     = TBB($ff818181818181ff);

  BB_Ranks78  = TBB($ffff000000000000);
  BB_Ranks12  = TBB($000000000000ffff);
  BB_Ranks123 = TBB($0000000000ffffff);
  BB_Ranks678 = TBB($ffffff0000000000);

  BB_A1H1     = TBB($0000000000000081);
  BB_A8H8     = TBB($8100000000000000);
  BB_AH67     = TBB($0081810000000000);
  BB_AH23     = TBB($0000000000818100);

  ColorBB : TBitboardPair = ( TBB(BB_Light), TBB(BB_Dark) );
  NonRank7: TBitboardPair = ( not BB_Rank7, not BB_Rank2 );

  BB_CrampFile: array [TFile] of TBitboard = (
    BB_FileB, 0, 0, 0, 0, 0, 0, BB_FileG
  );

  FileBB: array [TFile] of TBitboard = (
    TBB(BB_FileA), TBB(BB_FileB), TBB(BB_FileC), TBB(BB_FileD),
    TBB(BB_FileE), TBB(BB_FileF), TBB(BB_FileG), TBB(BB_FileH)
  );

  RankBB: array [TRank] of TBitboard = (
    TBB(BB_Rank1), TBB(BB_Rank2), TBB(BB_Rank3), TBB(BB_Rank4),
    TBB(BB_Rank5), TBB(BB_Rank6), TBB(BB_Rank7), TBB(BB_Rank8)
  );

  AdjacentLeftBB: array [TFile] of TBitboard = (
    TBB($0000000000000000),
    TBB($0101010101010101),
    TBB($0202020202020202),
    TBB($0404040404040404),
    TBB($0808080808080808),
    TBB($1010101010101010),
    TBB($2020202020202020),
    TBB($4040404040404040)
  );

  AdjacentRightBB: array [TFile] of TBitboard = (
    TBB($0202020202020202),
    TBB($0404040404040404),
    TBB($0808080808080808),
    TBB($1010101010101010),
    TBB($2020202020202020),
    TBB($4040404040404040),
    TBB($8080808080808080),
    TBB($0000000000000000)
  );

  AdjacentFilesBB: array [TFile] of TBitboard = (
    TBB($0202020202020202),
    TBB($0505050505050505),
    TBB($0a0a0a0a0a0a0a0a),
    TBB($1414141414141414),
    TBB($2828282828282828),
    TBB($5050505050505050),
    TBB($a0a0a0a0a0a0a0a0),
    TBB($4040404040404040)
  );

  ThisAndAdjacentFilesBB: array [TFile] of TBitboard = (
    TBB($0303030303030303),
    TBB($0707070707070707),
    TBB($0e0e0e0e0e0e0e0e),
    TBB($1c1c1c1c1c1c1c1c),
    TBB($3838383838383838),
    TBB($7070707070707070),
    TBB($e0e0e0e0e0e0e0e0),
    TBB($c0c0c0c0c0c0c0c0)
  );

  InFrontBB: array [TSide, TRank] of TBitboard = (
  (
    TBB($ffffffffffffff00),
    TBB($ffffffffffff0000),
    TBB($ffffffffff000000),
    TBB($ffffffff00000000),
    TBB($ffffff0000000000),
    TBB($ffff000000000000),
    TBB($ff00000000000000),
    TBB($0000000000000000) ),
  (
    TBB($0000000000000000),
    TBB($00000000000000ff),
    TBB($000000000000ffff),
    TBB($0000000000ffffff),
    TBB($00000000ffffffff),
    TBB($000000ffffffffff),
    TBB($0000ffffffffffff),
    TBB($00ffffffffffffff) )
  );

  WestOf: array [TFile] of TBitboard = (
    TBB($0000000000000000),
    TBB($0101010101010101),
    TBB($0303030303030303),
    TBB($0707070707070707),
    TBB($0f0f0f0f0f0f0f0f),
    TBB($1f1f1f1f1f1f1f1f),
    TBB($3f3f3f3f3f3f3f3f),
    TBB($7f7f7f7f7f7f7f7f)
  );

  EastOf: array [TFile] of TBitboard = (
    TBB($fefefefefefefefe),
    TBB($fcfcfcfcfcfcfcfc),
    TBB($f8f8f8f8f8f8f8f8),
    TBB($f0f0f0f0f0f0f0f0),
    TBB($e0e0e0e0e0e0e0e0),
    TBB($c0c0c0c0c0c0c0c0),
    TBB($8080808080808080),
    TBB($0000000000000000)
  );

  OutpostSquares: TBitboardPair = (
    TBB($00007e7e7e000000),
    TBB($0000007e7e7e0000)
  );

  HiddenWest: array [TSide, TFile] of TBitboard = (
  (
    TBB($0000000000000000),
    TBB($0001010000000000),
    TBB($0003030000000000),
    TBB($0006060000000000),
    TBB($000c0c0000000000),
    TBB($0018180000000000),
    TBB($0030300000000000),
    TBB($0060600000000000) ),
  (
    TBB($0000000000000000),
    TBB($0000000000010100),
    TBB($0000000000030300),
    TBB($0000000000060600),
    TBB($00000000000c0c00),
    TBB($0000000000181800),
    TBB($0000000000303000),
    TBB($0000000000606000) )
  );

  HiddenEast: array [TSide, TFile] of TBitboard = (
  (
    TBB($0006060000000000),
    TBB($000c0c0000000000),
    TBB($0018180000000000),
    TBB($0030300000000000),
    TBB($0060600000000000),
    TBB($00c0c00000000000),
    TBB($0080800000000000),
    TBB($0000000000000000) ),
  (
    TBB($0000000000060600),
    TBB($00000000000c0c00),
    TBB($0000000000181800),
    TBB($0000000000303000),
    TBB($0000000000606000),
    TBB($0000000000c0c000),
    TBB($0000000000808000),
    TBB($0000000000000000) )
  );

  EDGE = 0;
  MIDDLE = 1;
  CENTER = 2;

procedure set_bit(var b: TBitboard; sq: TSquare); inline;
procedure xor_bit(var b: TBitboard; sq: TSquare); inline;

procedure clear_lsb(var b: TBitboard); inline;

function is_set(bb: TBitboard; sq: TSquare): boolean; inline;

function bishop_attacks(sq: TSquare; occ: TBitboard): TBitboard; inline;
function rook_attacks(sq: TSquare; occ: TBitboard): TBitboard; inline;
function queen_attacks(sq: TSquare; occ: TBitboard): TBitboard; inline;

function find_lsb(b: TBitboard): TSquare; inline;

function BSF(b: TBitboard): TSquare; inline;
function BSR(b: TBitboard): TSquare; inline;
function popcnt(b: TBitboard): integer;
function popcnt_15(b: TBitboard): integer;

function file_bb(sq: TSquare): TBitboard; overload; inline;
function rank_bb(sq: TSquare): TBitboard; overload; inline;
function in_front_bb(Side: TSide; Sq: TSquare): TBitboard; inline;
function adjacent_files(sq: TSquare): TBitboard; inline; overload;
function this_and_adjacent_files(sq: TSquare): TBitboard; inline; overload;

function sq_color(sq: TSquare): TSide; inline;

function pawn_west_w(pawns: TBitboard): TBitboard; inline;
function pawn_east_w(pawns: TBitboard): TBitboard; inline;
function pawn_attk_w(pawns: TBitboard): TBitboard; inline;

function pawn_west_b(pawns: TBitboard): TBitboard; inline;
function pawn_east_b(pawns: TBitboard): TBitboard; inline;
function pawn_attk_b(pawns: TBitboard): TBitboard; inline;

function fwd_one_w(pawns: TBitboard): TBitboard; inline;
function fwd_two_w(pawns: TBitboard): TBitboard; inline;
function fwd_one_b(pawns: TBitboard): TBitboard; inline;
function fwd_two_b(pawns: TBitboard): TBitboard; inline;

type
  TDirection = byte;

const
  DIR_NONE = 0;

  DIR_E    = $01;
  DIR_W    = $11;
  DIR_N    = $02;
  DIR_S    = $12;
  DIR_NW   = $04;
  DIR_SE   = $14;
  DIR_NE   = $08;
  DIR_SW   = $18;

  DIR_DIAG = $0c;
  DIR_ORTH = $03;
  DIR_ANY  = $0f;

procedure init_bitboard();

type
  TBAttacksDB = array [0..$147f] of TBitboard;
  TRAttacksDB = array [0..$18fff] of TBitboard;
  PBAttacksDB = ^TBAttacksDB;
  PRAttacksDB = ^TBAttacksDB;

  TDistance = array [TSquare] of byte;
  PDistance = ^TDistance;

  TMagic32 = record
    lo, hi: cardinal;
  end;

var
  Direction, AbsDir: array [TSquare, TSquare] of TDirection;
  SquareBB, EpMaskBB, KingStep, KnightStep: array [TSquare] of TBitboard;
  DiagonalBB, OrthogonalBB, AllDirBB: array [TSquare] of TBitboard;
  PawnAttacksBB, PassedMaskBB, ForwardBB: array [TSide, TSquare] of TBitboard;
  BAttacksDB: TBAttacksDB;
  RAttacksDB: TRAttacksDB;
  ShieldFiles: array [TFile, 0..2] of TBitboard;
  BetweenBB: array [TSquare, TSquare] of TBitboard;
  ShadowBB: array [TSquare, TSquare] of TBitboard;
  PopCnt_8bit, lsb_8bit, msb_8bit: array [0..255] of byte;
  Distance: array [TSquare, TSquare] of byte;
  DistanceKP: array [TSide, TSquare, TSquare] of byte;
  Islands: array [0..255] of byte;
  Connection: array [TSquare] of TBitboard;
  Shepherd: array [TSide, TSquare] of TBitboard;
  RIndex, BIndex: array [TSquare] of PBitboard;

{$ifndef cpu64}
  BMask, RMask, BMult32, RMult32: array [TSquare] of TMagic32;
{$else}
  BMask, RMask: array [TSquare] of TBitboard;
{$endif}

implementation

const
  KnightDeltas: array [0..7] of TDelta = (
    DELTA_NNW, DELTA_NNE, DELTA_NWW, DELTA_NEE,
    DELTA_SWW, DELTA_SEE, DELTA_SSW, DELTA_SSE
  );

  KingDeltas: array [0..7] of TDelta = (
    DELTA_NW, DELTA_N, DELTA_NE, DELTA_W,
    DELTA_E, DELTA_SW, DELTA_S, DELTA_SE
  );

  FileAlign: array [TFile] of TFile = (
    FILE_B, FILE_B, FILE_C, FILE_D, FILE_E, FILE_F, FILE_G, FILE_G
  );

procedure clear_lsb(var b: TBB); inline;
begin
  b := b and TBB(b-1);
end;

function is_set(bb: TBitboard; sq: TSquare): boolean; inline;
begin
  result:=(bb and SquareBB[sq] <> 0);
end;

procedure set_bit(var b: TBitboard; sq: TSquare); inline;
begin
  b:=(b or SquareBB[Sq]);
end;

procedure xor_bit(var b: TBitboard; sq: TSquare); inline;
begin
  b:=(b xor SquareBB[Sq]);
end;

function file_bb(sq: TSquare): TBitboard; overload; inline;
begin
  result := FileBB[file_of(sq)];
end;

function file_bb(f: TFile): TBitboard; overload; inline;
begin
  result := FileBB[f];
end;

function rank_bb(sq: TSquare): TBitboard; overload; inline;
begin
  result := RankBB[rank_of(sq)];
end;

function rank_bb(r: TRank): TBitboard; overload; inline;
begin
  result := RankBB[r];
end;

function in_front_bb(side: TSide; sq: TSquare): TBitboard; inline;
begin
  result := InFrontBB[side, rank_of(sq)];
end;

function adjacent_files(f: TFile): TBitboard; inline; overload;
begin
  result := AdjacentFilesBB[f];
end;

function adjacent_files(sq: TSquare): TBitboard; inline; overload;
begin
  result := AdjacentFilesBB[file_of(sq)];
end;

function this_and_adjacent_files(f: TFile): TBitboard; inline; overload;
begin
  result := ThisAndAdjacentFilesBB[f];
end;

function this_and_adjacent_files(sq: TSquare): TBitboard; inline; overload;
begin
  result := ThisAndAdjacentFilesBB[file_of(sq)];
end;

function sq_color(sq: TSquare): TSide; inline;
begin
  result := TSide($aa55aa55 shr integer(sq) and 1);
end;

function pawn_west_w(pawns: TBitboard): TBitboard; inline;
begin
  result := TBB((pawns and not BB_FileA) shl 7);
end;

function pawn_east_w(pawns: TBitboard): TBitboard; inline;
begin
  result := TBB((pawns and not BB_FileH) shl 9);
end;

function pawn_attk_w(pawns: TBitboard): TBitboard; inline;
begin
  result := TBB((pawns and not BB_FileA) shl 7) or TBB((pawns and not BB_FileH) shl 9);
end;

function pawn_west_b(pawns: TBitboard): TBitboard; inline;
begin
  result := TBB((pawns and not BB_FileA) shr 9);
end;

function pawn_east_b(pawns: TBitboard): TBitboard; inline;
begin
  result := TBB((pawns and not BB_FileH) shr 7);
end;

function pawn_attk_b(pawns: TBitboard): TBitboard; inline;
begin
  result := TBB((pawns and not BB_FileA) shr 9) or TBB((pawns and not BB_FileH) shr 7);
end;

function fwd_one_w(pawns: TBitboard): TBitboard; inline;
begin
  result := TBB(pawns shl 8);
end;

function fwd_two_w(pawns: TBitboard): TBitboard; inline;
begin
  result := TBB(pawns shl 16);
end;

function fwd_one_b(pawns: TBitboard): TBitboard; inline;
begin
  result := TBB(pawns shr 8);
end;

function fwd_two_b(pawns: TBitboard): TBitboard; inline;
begin
  result := TBB(pawns shr 16);
end;

{$ifdef cpu64}
{$include magic64.inc}
{$else}
{$include magic32.inc}
{$endif}

function queen_attacks(sq: TSquare; occ: TBitboard): TBitboard; inline;
begin
  result := rook_attacks(sq, occ) or bishop_attacks(sq, occ);
end;

procedure add_attack(fromsq: TSquare; delta: TDelta; var b: TBitboard);
var tosq: integer;
begin
  tosq := integer(fromsq) + integer(delta);
  if (tosq in [0..63]) and (file_distance(fromsq, TSquare(tosq)) < 3) then
    set_bit(b, TSquare(tosq));
end;

procedure init_masks();
var
  sq: TSquare;
  f: TFile; r: TRank;
  i, j, k: integer;
  b, north2, south2: TBitboard;

begin
  for sq := A4 to H5 do
    EpMaskBB[sq] := RankBB[rank_of(sq)] and AdjacentFilesBB[file_of(sq)];

  for sq := A1 to H8 do begin
    r := rank_of(sq);
    f := file_of(sq);
    ForwardBB[WHITE, sq] := InFrontBB[WHITE, r] and FileBB[f];
    ForwardBB[BLACK, sq] := InFrontBB[BLACK, r] and FileBB[f];
    PassedMaskBB[WHITE, sq] := InFrontBB[WHITE, r] and ThisAndAdjacentFilesBB[f];
    PassedMaskBB[BLACK, sq] := InFrontBB[BLACK, r] and ThisAndAdjacentFilesBB[f];
  end;

  for sq := A1 to H8 do begin
    DiagonalBB[sq] := bishop_attacks(sq, BB_EMPTY);
    OrthogonalBB[sq] := file_bb(sq) xor rank_bb(sq);
    AllDirBB[sq] := OrthogonalBB[sq] or DiagonalBB[sq];
    PawnAttacksBB[WHITE, sq] := BB_EMPTY;
    PawnAttacksBB[BLACK, sq] := BB_EMPTY;
    KnightStep[sq] := BB_EMPTY;
    for i := 0 to 7 do begin
      add_attack(sq, KnightDeltas[i], KnightStep[sq]);
      add_attack(sq, KingDeltas[i], KingStep[sq]);
    end;
    if (file_of(sq) <> FILE_A) then begin
      add_attack(sq, DELTA_NW, PawnAttacksBB[WHITE, sq]);
      add_attack(sq, DELTA_SW, PawnAttacksBB[BLACK, sq]);
    end;
    if (file_of(sq) <> FILE_H) then begin
      add_attack(sq, DELTA_NE, PawnAttacksBB[WHITE, sq]);
      add_attack(sq, DELTA_SE, PawnAttacksBB[BLACK, sq]);
    end;
  end;

  for sq := A1 to H8 do begin
    r := rank_of(sq);
    f := file_of(sq);
    north2:= RankBB[r]; south2 := RankBB[r];
    if (r < RANK_8) then begin
      north2 := north2 or RankBB[succ(r)];
      if (r < RANK_7) then
        north2 := north2 or RankBB[succ(succ(r))];
    end;
    if (r > RANK_1) then begin
      south2 := south2 or RankBB[pred(r)];
      if (r > RANK_2) then
        south2 := south2 or RankBB[pred(pred(r))];
    end;
    Connection[sq] := AdjacentFilesBB[f] and (north2 or south2);
  end;

  for sq := A1 to H8 do begin
    f := file_of(sq);
    r := rank_of(sq);
    b := AdjacentFilesBB[f];
    Shepherd[WHITE, sq] := BB_EMPTY;
    Shepherd[BLACK, sq] := BB_EMPTY;
    if (f <> FILE_A) and (f <> FILE_H) then
      b := b or FileBB[f];
    if (r >= RANK_5) then begin
      Shepherd[WHITE, sq] := Shepherd[WHITE, sq] or (b and BB_Rank7);
      if (r >= RANK_6) then
        Shepherd[WHITE, sq] := Shepherd[WHITE, sq] or (b and BB_Rank8);
    end;
    if (r <= RANK_4) then begin
      Shepherd[BLACK, sq] := Shepherd[BLACK, sq] or (b and BB_Rank2);
      if (r <= RANK_3) then
        Shepherd[BLACK, sq] := Shepherd[BLACK, sq] or (b and BB_Rank1);
    end;
  end;

  for f := FILE_A to FILE_D do begin
    ShieldFiles[f, EDGE] := file_bb(pred(FileAlign[f]));
    ShieldFiles[f, MIDDLE] := file_bb(FileAlign[f]);
    ShieldFiles[f, CENTER] := file_bb(succ(FileAlign[f]));
  end;

  for f := FILE_E to FILE_H do begin
    ShieldFiles[f, EDGE] := file_bb(succ(FileAlign[f]));
    ShieldFiles[f, MIDDLE] := file_bb(FileAlign[f]);
    ShieldFiles[f, CENTER] := file_bb(pred(FileAlign[f]));
  end;

  for i := 0 to 255 do begin
    j := i; k := 0;
    while (j <> 0) do begin
      inc(k);
      j := j and (j - 1);
    end;
    PopCnt_8bit[i] := k;
    lsb_8bit[i] := 8;
    msb_8bit[i] := 0;
    for j := 0 to 7 do begin
      if (i and (1 shl j)) <> 0 then begin
        if (lsb_8bit[i] = 8) then lsb_8bit[i] := j;
        msb_8bit[i] := j;
      end;
    end;
  end;
  msb_8bit[0] := 8;

  for i := 1 to 255 do begin
    j := 0; k:= 1;
    for f := FILE_A to FILE_H do begin
      if (i and (1 shl ord(f))) <> 0 then begin
        if k <> 0 then inc(j);
        k := 0;
      end
      else
        k := 1;
    end;
    Islands[i] := j-1;
  end;
  Islands[0] := 0;
end;

var
  DirDelta: array [0..31] of TDelta;

procedure init_directions();
var
  from_sq, to_sq: TSquare;
  sq1, sq2, rd, fd: integer;
  dir: TDirection;
begin

  DirDelta[DIR_E] := DELTA_E;
  DirDelta[DIR_W] := DELTA_W;
  DirDelta[DIR_N] := DELTA_N;
  DirDelta[DIR_S] := DELTA_S;
  DirDelta[DIR_NW] := DELTA_NW;
  DirDelta[DIR_SE] := DELTA_SE;
  DirDelta[DIR_NE] := DELTA_NE;
  DirDelta[DIR_SW] := DELTA_SW;

  for sq1 := ord(A1) to ord(H8) do
    for sq2 := ord(A1) to ord(H8) do begin
      dir := DIR_NONE;
      if sq1 <> sq2 then begin
        if (sq1 and 7) = (sq2 and 7) then
          if sq1 > sq2 then dir := DIR_S else dir := DIR_N;
        if (sq1 shr 3) = (sq2 shr 3) then
          if sq1 > sq2 then dir := DIR_W else dir := DIR_E;
        if (sq1 and 7 - sq2 and 7) = (sq1 shr 3 - sq2 shr 3) then
          if sq1 > sq2 then dir := DIR_SW else dir := DIR_NE;
        if (sq2 and 7 - sq1 and 7) = (sq1 shr 3 - sq2 shr 3) then
          if sq1 > sq2 then dir := DIR_SE else dir := DIR_NW;
      end;
      Direction[TSquare(sq1), TSquare(sq2)] := dir;
      AbsDir[TSquare(sq1), TSquare(sq2)] := TDirection(ord(dir) and $0f);
    end;

  for from_sq := A1 to H8 do begin
    for to_sq := A1 to H8 do begin
      rd := rank_distance(from_sq, to_sq);
      fd := file_distance(from_sq, to_sq);
      Distance[from_sq, to_sq] := byte(max(rd, fd));
      if from_sq > to_sq then begin
        DistanceKP[WHITE, from_sq, to_sq] := max(2*fd, 1*rd);
        DistanceKP[BLACK, from_sq, to_sq] := max(2*fd, 2*rd);
      end
      else begin
        DistanceKP[WHITE, from_sq, to_sq] := max(2*fd, 2*rd);
        DistanceKP[BLACK, from_sq, to_sq] := max(2*fd, 1*rd);
      end;
    end;
  end;
end;

procedure init_between();
var
  sq, from_sq, to_sq: TSquare;
  f: TFile; r: TRank;
  delta: TDelta;
  dir: integer;
  b: TBitboard;
begin
  init_directions();
  for from_sq := A1 to H8 do
    for to_sq := A1 to H8 do begin
      BetweenBB[from_sq, to_sq] := BB_Empty;
      dir := Direction[from_sq, to_sq];
      if (dir <> DIR_NONE) then begin
        delta := DirDelta[dir];
        sq := delta_add(from_sq, delta);
        while (sq <> to_sq) do begin
          set_bit(BetweenBB[from_sq, to_sq], sq);
          sq := delta_add(sq, delta);
        end;
      end;
    end;

  for from_sq := A1 to H8 do
    for to_sq := A1 to H8 do begin
      f := file_of(to_sq); r := rank_of(to_sq);
      dir := Direction[from_sq, to_sq];
      b := BB_EMPTY;
      if (dir <> DIR_NONE) then begin
        if (dir and DIR_DIAG) <> 0 then
          b := DiagonalBB[to_sq]
        else
          b:= OrthogonalBB[to_sq];
        case dir of
          DIR_E:  b := b and EastOf[f];
          DIR_W:  b := b and WestOf[f];
          DIR_N:  b := b and InFrontBB[WHITE, r];
          DIR_S:  b := b and InFrontBB[BLACK, r];
          DIR_NW: b := b and InFrontBB[WHITE, r] and WestOf[f];
          DIR_SE: b := b and InFrontBB[BLACK, r] and EastOf[f];
          DIR_NE: b := b and InFrontBB[WHITE, r] and EastOf[f];
          DIR_SW: b := b and InFrontBB[BLACK, r] and WestOf[f];
        end;
      end;
      ShadowBB[from_sq, to_sq] := not b;
    end;
end;

type
  TDeltas = array [0..3] of integer;

{$ifopt R+}{$define RangeCheckWasOn}{$R-}{$endif}
function slider_attacks(sq: TSquare; occ, mask: TBitboard; const deltas: TDeltas): TBitboard;
var
  i: integer;
  s: TSquare;
begin
  result := BB_Empty;
  for i := 0 to 3 do begin
    s := delta_add(sq, deltas[i]);
    while valid_square(integer(s)) and (SquareBB[s] and mask <> 0) do begin
      if (square_distance(s, delta_sub(s, deltas[i])) <> 1) then break;
      set_bit(result, s);
      if SquareBB[s] and occ <> 0 then break;
      inc(s, deltas[i]);
    end;
  end;
end;
{$ifdef RangeCheckWasOn}{$R+}{$undef RangeCheckWasOn}{$endif}

function index_to_bb(index: integer; mask: TBitboard): TBitboard;
var
  i: integer;
begin
  result := BB_Empty;
  i := 0;
  while (mask <> 0) do begin
    if (index and (1 shl i) <> 0) then
      set_bit(result, find_lsb(mask));
    inc(i);
    clear_lsb(mask);
  end;
end;

const
  RookDeltas: TDeltas = ( DELTA_N, DELTA_S, DELTA_E, DELTA_W );
  BishDeltas: TDeltas = ( DELTA_NW, DELTA_SE, DELTA_NE, DELTA_SW );

procedure init_magic();
var
  j, k, ofs: cardinal;
  sq: TSquare;
  occ, b: TBitboard;
  start: integer;
begin
  SquareBB[A1] := 1;
  for sq := B1 to H8 do
    SquareBB[sq] := TBB(SquareBB[pred(sq)] shl 1);

  start := 0;
  for sq := A1 to H8 do begin
    b := slider_attacks(sq, 0, not BB_Edge, BishDeltas);
    BIndex[sq] := @BAttacksDB[start];
    {$ifndef cpu64}
      BMult32[sq].lo := BMult[sq] and $ffffffff;
      BMult32[sq].hi := BMult[sq] shr 32;
      BMask[sq].lo := b and $ffffffff;
      BMask[sq].hi := b shr 32;
    {$else}
      BMask[sq] := b;
    {$endif}
    j := (1 shl (BITS - BShift[sq]));
    for k := 0 to j - 1 do begin
      occ := index_to_bb(k, b);
      ofs := start + bishop_idx(sq, occ);
      BAttacksDB[ofs] := slider_attacks(sq, occ, BB_Full, BishDeltas);
    end;
    inc(start, j);
  end;

  start := 0;
  for sq := A1 to H8 do begin
    b := (FileBB[file_of(sq)] or not BB_FilesAH)
     and (RankBB[rank_of(sq)] or not BB_Ranks18);
    b := slider_attacks(sq, 0, b, RookDeltas);
    RIndex[sq] := @RAttacksDB[start];
    {$ifndef cpu64}
      RMult32[sq].lo := RMult[sq] and $ffffffff;
      RMult32[sq].hi := RMult[sq] shr 32;
      RMask[sq].lo := b and $ffffffff;
      RMask[sq].hi := b shr 32;
    {$else}
      RMask[sq] := b;
    {$endif}
    j := (1 shl (BITS - RShift[sq]));
    for k := 0 to j - 1 do begin
      occ := index_to_bb(k, b);
      ofs := start + rook_idx(sq, occ);
      RAttacksDB[ofs] := slider_attacks(sq, occ, BB_Full, RookDeltas);
    end;
    inc(start, j);
  end;
end;

procedure init_bitboard();
begin
  init_magic();
  init_masks();
  init_between();
end;

end.
