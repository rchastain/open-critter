
unit uBitbase;

interface

uses
  uPiece, uSquare;

{$include platform.inc}

function probe_kqkp(wksq, bksq, qsq, psq: TSquare; stm: TSide): boolean;
function probe_kpk(wksq, bksq, psq: TSquare; stm: TSide): boolean;

implementation

{.$warnings off}
{$include kpk.inc}
{$include kqkp.inc}
{.$warnings on}

const
  KPK_SIZE  = 2 * 64 * 64 * 24 div 8;
  KQKP_SIZE = 2 * 64 * 64 * 32 div 8;

type
  TBitbaseKPK  = array [0 .. KPK_SIZE - 1] of byte;
  TBitbaseKQKP = array [0 .. 2*KQKP_SIZE - 1] of byte;

var
  KPK: TBitbaseKPK;
  KQKP: TBitbaseKQKP;

function kpk_index(wksq, bksq, psq: TSquare; stm: TSide): integer; inline;
var
  pawn_idx: integer;
begin
  assert(file_of(psq) <= FILE_D);
  pawn_idx := ((ord(rank_of(psq)) - 1) shl 2) + ord(file_of(psq));
  result := pawn_idx shl 13 + ord(wksq) shl 7 + ord(bksq) shl 1 + ord(stm);
end;

function kqkp_index(wksq, bksq, qsq: TSquare; stm: TSide): integer; inline;
begin
  assert(rank_of(bksq) <= RANK_4);
  result := ord(stm) shl 17 + ord(bksq) shl 12 + ord(qsq) shl 6 + ord(wksq);
end;

function probe_kpk(wksq, bksq, psq: TSquare; stm: TSide): boolean;
var
  i: integer;
begin
  i := kpk_index(wksq, bksq, psq, stm);
  result := KPK[i shr 3] and (1 shl (i and 7)) <> 0;
end;

function probe_kqkp(wksq, bksq, qsq, psq: TSquare; stm: TSide): boolean;
var
  i: integer;
begin
  i := kqkp_index(wksq, bksq, qsq, stm);
  if psq = A2 then
    result := KQKP[i shr 3] and (1 shl (i and 7)) <> 0
  else
    result := KQKP[KQKP_SIZE + i shr 3] and (1 shl (i and 7)) <> 0
end;

procedure decompress(const input; length: integer; out output);
var
  ip, ip_limit, op, ref: PByte;
  ctrl, len, ofs: integer;
  code: byte; b: byte;
  loop: boolean;
begin
  ip := @input; op := @output; ip_limit := ip + length;
  ctrl := ip^ and 31; inc(ip); loop := true;
  while loop do begin
    ref := op; len := ctrl shr 5; ofs := (ctrl and 31) shl 8;
    if ctrl >= 32 then begin
      dec(len); dec(ref, ofs);
      if len = 6 then
        repeat code := ip^; inc(ip); inc(len, code); until code <> 255;
      code := ip^; inc(ip);
      dec(ref, code);
      if (code = 255) and (ofs = (31 shl 8)) then begin
        ofs := ip[0] shl 8 + ip[1]; inc(ip, 2);
        ref := op - ofs - 8191;
      end;
      loop := ip < ip_limit; if loop then begin ctrl := ip^; inc(ip); end;
      if ref = op then begin
        inc(len, 3); b := ref[-1];
        repeat op[0] := b; inc(op); dec(len); until len = 0;
      end
      else begin
        inc(len, 3); dec(ref);
        repeat op^ := ref^; inc(ref); inc(op); dec(len); until len = 0;
      end;
    end
    else begin
      inc(ctrl); repeat op^ := ip^; inc(op); inc(ip); dec(ctrl); until ctrl = 0;
      loop := ip < ip_limit; if loop then begin ctrl := ip^; inc(ip); end;
    end;
  end;
end;

initialization

decompress(KPK_DATA, sizeof(KPK_DATA), KPK);
decompress(KQKP_DATA, sizeof(KQKP_DATA), KQKP);

end.
