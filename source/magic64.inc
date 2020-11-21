
{$push}
{$Q-}
{$R-}
function bishop_idx(sq: TSquare; occ: TBitboard): TBitboard;
begin
  result := ((occ and BMask[sq]) * BMult[sq]) shr BShift[sq];
end;

function rook_idx(sq: TSquare; occ: TBitboard): TBitboard;
begin
  result := ((occ and RMask[sq]) * RMult[sq]) shr RShift[sq];
end;

function bishop_attacks(sq: TSquare; occ: TBitboard): TBitboard;
begin
  result := BIndex[sq][((occ and BMask[sq]) * BMult[sq]) shr BShift[sq]];
end;

function rook_attacks(sq: TSquare; occ: TBitboard): TBitboard;
begin
  result := RIndex[sq][((occ and RMask[sq]) * RMult[sq]) shr RShift[sq]];
end;

{$ifdef HasBitscan}
function BSF(b: TBitboard): TSquare; inline;
begin
  result := TSquare(BsfQWord(b));
end;

function BSR(b: TBitboard): TSquare; inline;
begin
  result := TSquare(BsrQWord(b));
end;

function find_lsb(b: TBitboard): TSquare; inline;
begin
  result := TSquare(BsfQWord(b));
end;
{$else}
{$endif}

//{$ifopt R+}{$define RangeCheckWasOn}{$R-}{$endif}
function popcnt(b: TBitboard): integer;
begin
  b := b - ((b shr 1) and $5555555555555555);
  b := ((b shr 2) and $3333333333333333) + (b and $3333333333333333);
  b := ((b shr 4) + b) and $0f0f0f0f0f0f0f0f;
  b := b * $0101010101010101;
  result := integer(b shr 56);
end;

function popcnt_15(b: TBitboard): integer;
begin
  b := b - ((b shr 1) and $5555555555555555);
  b := ((b shr 2) and $3333333333333333) + (b and $3333333333333333);
  b := b * $1111111111111111;
  result := integer(b shr 60);
end;
//{$ifdef RangeCheckWasOn}{$R+}{$undef RangeCheckWasOn}{$endif}
{$pop}
