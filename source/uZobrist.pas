
unit uZobrist;

interface

uses
  uPiece, uSquare, uMove;

{$include platform.inc}

type
  TKey = uint64;
  TKey32 = cardinal;

var
  KeyPieces: array [TPiece, TSquare] of TKey;
  KeyCastle: array [0..15] of TKey;
  KeyEp    : array [TFile] of TKey;
  KeyStm   : array [TSide] of TKey;
  KeyMtrl  : array [TPiece, 0..11] of TKey32;

const
  KeyWtm = TKey(-1);

function exclusion_key(side: TSide; move: TMove): TKey; inline;

implementation

const
  rnd_seed: array [0..54] of cardinal = (
    $5414d5f4, $b3935330, $d0773e27, $ac62a182, $5db20c92,
    $c1e618ac, $d003926a, $171fa3b3, $9ad12101, $762172c1,
    $bc5d9dc3, $07b72a70, $6e5ac890, $f7be54d1, $0d6332fa,
    $56ba8fbe, $eba585ec, $571421c5, $96b3c079, $12eb9b92,
    $c07c0978, $0700c5bd, $31a43513, $74eba4aa, $4ec55db5,
    $2c91c2e9, $208771f6, $304ee204, $1e004673, $f3ac22f9,
    $a8561feb, $ded13435, $d03e7513, $dae66bc2, $01c20be1,
    $46638997, $fb26ce62, $01f87fc3, $c0016099, $44429270,
    $b04f4481, $f31b8ad7, $86dd7a46, $03e72091, $96a22589,
    $74dcb7d4, $12d0fc03, $474a16e8, $2ff5717c, $54bded92,
    $783a9487, $e767978c, $3052d5cd, $16eca191, $8eaf6d15
  );

var
  rnd_tab: array [0..54] of cardinal;
  rnd_j, rnd_k: integer;

procedure init_random();
var
  i: integer;
begin
  for i := 0 to 54 do
    rnd_tab[i] := rnd_seed[i];
  rnd_j := 23;
  rnd_k := 54;
end;

{$push}
{$Q-}
{$R-}
function random32(): cardinal;
begin
  inc(rnd_tab[rnd_k], rnd_tab[rnd_j]);
  result := rnd_tab[rnd_k];
  dec(rnd_j); if rnd_j < 0 then rnd_j := 54;
  dec(rnd_k); if rnd_k < 0 then rnd_k := 54;
end;
{$pop}

function random64(): TKey;
begin
  result := TKey(TKey(random32()) shl 32) or TKey(random32());
end;

procedure init_zobrist();
var
  sq: TSquare;
  f: TFile;
  p: TPiece;
  i, j: integer;
begin
  init_random();

  for sq := A1 to H8 do begin
    for i := 0 to 15 do
      KeyPieces[TPiece(i), sq] := random64();
  end;

  for f := FILE_A to FILE_H do
    KeyEP[f] := random64();

  KeyCastle[0] := 0;
  for i := 1 to 15 do begin
    if (i and (i-1)) <> 0 then begin
      KeyCastle[i] := 0;
      j := 1;
      while j < 16 do begin
        if (i and j) <> 0 then
          KeyCastle[i] := KeyCastle[i] xor KeyCastle[j];
        j := j shl 1;
      end;
    end
    else
      KeyCastle[i] := random64();
  end;

  KeyStm[WHITE] := TKey(KeyWtm);
  KeyStm[BLACK] := 0;

  for p := W_PAWN to B_QUEEN do
    for i := 0 to 11 do
      KeyMtrl[p, i] := random32();

end;

function exclusion_key(side: TSide; move: TMove): TKey;
begin
  result := KeyPieces[TPiece(side), source_sq(move)]
        xor KeyPieces[TPiece(side), target_sq(move)];
end;

initialization

init_zobrist();

end.
