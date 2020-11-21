
{$ifopt Q+}{$define OverflowCheckWasOn}{$Q-}{$endif}

function bishop_idx(sq: TSquare; occ: TBitboard): integer; inline;
var h, l: cardinal;
begin
  l := (cardinal(occ and $ffffffff) and BMask[sq].lo) * BMult32[sq].lo;
  h := (cardinal(occ shr 32) and BMask[sq].hi) * BMult32[sq].hi;
  result := (l xor h) shr BShift[sq];
end;

function rook_idx(sq: TSquare; occ: TBitboard): integer; inline;
var h, l: cardinal;
begin
  l := (cardinal(occ and $ffffffff) and RMask[sq].lo) * RMult32[sq].lo;
  h := (cardinal(occ shr 32) and RMask[sq].hi) * RMult32[sq].hi;
  result := (l xor h) shr RShift[sq];
end;

function bishop_attacks(sq: TSquare; occ: TBitboard): TBitboard;
var h, l: cardinal;
begin
{$ifdef FPC}
  l := lo(occ);
  h := hi(occ);
{$else}
  l := cardinal(occ);
  h := occ shr 32;
{$endif}
  l := l and BMask[sq].lo;
  h := h and BMask[sq].hi;
  l := l * BMult32[sq].lo;
  h := h * BMult32[sq].hi;
  result := BIndex[sq][(l xor h) shr BShift[sq]];
end;

function rook_attacks(sq: TSquare; occ: TBitboard): TBitboard;
var h, l: cardinal;
begin
  {$ifdef FPC}
    l := lo(occ);
    h := hi(occ);
  {$else}
    l := cardinal(occ);
    h := occ shr 32;
  {$endif}
  l := l and RMask[sq].lo;
  h := h and RMask[sq].hi;
  l := l * RMult32[sq].lo;
  h := h * RMult32[sq].hi;
  result := RIndex[sq][(l xor h) shr RShift[sq]];
end;

{$ifdef HasBitscan}

function find_lsb(b: TBitboard): TSquare; inline;
begin
  if (lo(b) <> 0) then
    result := TSquare(BsfDWord(lo(b)))
  else
    result := TSquare(BsfDWord(hi(b))+32)
end;

function BSF(b: TBitboard): TSquare; inline;
begin
  if (lo(b) <> 0) then
    result := TSquare(BsfDWord(lo(b)))
  else
    result := TSquare(BsfDWord(hi(b))+32)
end;

function BSR(b: TBitboard): TSquare; inline;
begin
  if (hi(b) <> 0) then
    result := TSquare(BsfDWord(hi(b))+32)
  else
    result := TSquare(BsfDWord(lo(b)))
end;

{$else}
{$endif}

function popcnt(b: TBitboard): integer;
var
  v, w: cardinal;
begin
{$ifdef FPC}
  v := hi(b);
  w := lo(b);
{$else}
  v := b shr 32;
  w := b and $ffffffff;
{$endif}
  dec(v, (v shr 1) and $55555555);
  dec(w, (w shr 1) and $55555555);
  v := ((v shr 2) and $33333333) + (v and $33333333);
  w := ((w shr 2) and $33333333) + (w and $33333333);
  v := ((v shr 4) + v) and $0f0f0f0f;
  inc(v, ((w shr 4) + w) and $0f0f0f0f);
  result := (v * $01010101) shr 24;
end;

function popcnt_15(b: TBitboard): integer;
var
  v, w: cardinal;
begin
{$ifdef FPC}
  v := hi(b);
  w := lo(b);
{$else}
  v := b shr 32;
  w := b and $ffffffff;
{$endif}
  dec(v, (v shr 1) and $55555555);
  dec(w, (w shr 1) and $55555555);
  v := ((v shr 2) and $33333333) + (v and $33333333);
  w := ((w shr 2) and $33333333) + (w and $33333333);
  inc(v, w);
  result := (v * $11111111) shr 28;
end;

{$ifdef OverflowCheckWasOn}{$Q+}{$undef OverflowCheckWasOn}{$endif}
