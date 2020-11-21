
unit uHistory;

interface

uses
  uMove, uPiece, uScore, uSquare;

{$include platform.inc}

type
  THistory = object
    ordering: array [TPiece, TSquare] of word;
    gaintab: array [0..$ffff] of smallint;
    procedure clear(phase: TMtrlStage);
    procedure record_good_move(move: TMove; depth: integer);
    procedure record_bad_move(move: TMove; depth: integer);
    function get_score(piece: TPiece; sq: TSquare): TScore; overload; inline;
    function get_score(move: TMove): TScore; overload; inline;
    function gain(move: TMove): TScore; inline;
    procedure set_psnl_gain(move: TMove; score: TScore);
    procedure dump();
  end;

const
  MAX_HIST = $10000;

var
  hist: THistory;

implementation

uses
  uPsq, uSystem;

procedure THistory.clear(phase: TMtrlStage);
var
  p: TPiece;
  from_sq, to_sq: TSquare;
  m: TMove;
begin
  fillchar(gaintab, sizeof(gaintab), 0);
  p := W_PAWN;
  while p <= W_QUEEN do begin
    for to_sq := A1 to H8 do begin
      ordering[p, to_sq] := 4 * scale(phase, PSQ[p, to_sq]) + $800;
      ordering[succ(p), to_sq] := $800 - 4 * scale(phase, PSQ[succ(p), to_sq]);
      for from_sq := A1 to H8 do begin
        m := mk_move(from_sq, to_sq, p);
        gaintab[m] := max(0, scale(phase, PSQ[p, to_sq] - PSQ[p, from_sq]));
        m := mk_move(from_sq, to_sq, succ(p));
        gaintab[m] := max(0, -scale(phase, PSQ[succ(p), to_sq] - PSQ[succ(p), from_sq]));
      end;
    end;
    inc(p, 2);
  end;
end;

procedure THistory.record_bad_move(move: TMove; depth: integer);
var
  x: PWord;
begin
  x := @ordering[piece_moved(move), target_sq(move)];
  dec(x^, (x^ * depth) shr 8);
end;

procedure THistory.record_good_move(move: TMove; depth: integer);
var
  x: PWord;
begin
  x := @ordering[piece_moved(move), target_sq(move)];
  inc(x^, (($ff00 - x^) * depth) shr 8);
end;

function THistory.get_score(piece: TPiece; sq: TSquare): TScore;
begin
  result := TScore(ordering[piece, sq]);
end;

function THistory.get_score(move: TMove): TScore;
begin
  result := TScore(ordering[piece_moved(move), target_sq(move)]);
end;

procedure THistory.dump();
var
  f: TextFile;
  p: TPiece; sq: TSquare;
  c: array [0..7] of AnsiChar;
begin
  AssignFile(f, 'history.log');
  {$I-}
  Rewrite(f);
  {$I+}
  for p := W_PAWN to B_QUEEN do
  begin
    writeln(f, LineEnding + 'history table for piece: ', color_to_char(side_of(p)), ' ', PieceNames[type_of(p)]);
    for sq := A1 to H8 do
    begin
      if file_of(sq) = FILE_A then writeln(f);
      sprintf(c, '%4x ', ordering[p, sq]); write(f, c);
    end;
  end;
  CloseFile(f);
end;

function THistory.gain(move: TMove): TScore;
begin
  result := TScore(gaintab[move and MASK_PIECEFROMTO]);
end;

procedure THistory.set_psnl_gain(move: TMove; score: TScore);
var
  index: cardinal;

{$ifdef LOG_POSGAIN}
  c: array [0..255] of AnsiChar;
  f: textfile;
{$endif}

begin
  index := move and MASK_PIECEFROMTO;

{$ifdef LOG_POSGAIN}
  AssignFile(f, 'makemove.log'); {$I-} Append(f);
  if IOResult <> 0 then Rewrite(f); {$I+}
  if IOResult = 0 then begin
    sprintf(@c, '%8x: %d + %d = %d' + LineEnding, move, score, gaintab[index],
      gaintab[index] + (score - gaintab[index]) div 16);
    write(f, PAnsiChar(@c));
  end;
  CloseFile(f);
{$endif}

  inc(gaintab[index], (score - gaintab[index]) div 16);
end;

end.
