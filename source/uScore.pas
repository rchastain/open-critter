
unit uScore;

interface

uses
  uPiece;

{$include platform.inc}

type
  TScore = {integer}int64;
  TScorePair = {integer}int64;
  TScale = integer;
  TScoreString = array [0..9] of AnsiChar;

const
  SCORE_ZERO   = 0;
  SCORE_INF    = 32767;
  SCORE_MATE   = 32767;
  SCORE_WIN    = 18000;
  RESIGN       = 32000;

  SCALE_ZERO   = 0;
  SCALE_NORMAL = 128;

  STAGE_OPENING = 32;
  STAGE_ENDGAME = 0;

  EG_VQUEEN = $b19;

  sign: array [TSide] of integer = ( 1, -1 );

type
  TMtrlStage = STAGE_ENDGAME .. STAGE_OPENING;

function eg(s: TScorePair): TScore; inline;
function mg(s: TScorePair): TScore; inline;

function mate_in(ply: integer): TScore; inline;
function mated_in(ply: integer): TScore; inline;

function pair(mg_val, eg_val: TScore): TScorePair; inline;
function scale(phase: TMtrlStage; score: TScorePair): TScore; //inline;

function score_from_tt(score: TScore; ply: integer): TScore;
function score_to_tt(score: TScore; ply: integer): TScore;

function score_to_uci(score: TScore; buf: PAnsiChar): PAnsiChar;
function score_to_string(score: TScore; buf: PAnsiChar): PAnsiChar;

function valid_score(score: TScore): boolean;

procedure print_score(phase: TMtrlStage; name: PAnsiChar; w, b: TScorePair);

implementation

uses
  uSystem;

function eg(s: TScorePair): TScore; inline;
begin
  result := smallint(s);
end;

function mg(s: TScorePair): TScore; inline;
begin
  result := smallint((s + $8000) shr 16);
end;

function pair(mg_val, eg_val: TScore): TScorePair; inline;
begin
  result := TScorePair(mg_val shl 16 + eg_val);
end;

function mate_in(ply: integer): TScore; inline;
begin
  result := +SCORE_MATE - ply;
end;

function mated_in(ply: integer): TScore; inline;
begin
  result := -SCORE_MATE + ply
end;

function score_from_tt(score: TScore; ply: integer): TScore;
begin
  if score >= RESIGN then
    result := score - ply
  else
    if score <= -RESIGN then
      result := score + ply
  else
    result := score;
end;

function score_to_tt(score: TScore; ply: integer): TScore;
begin
  if score >= RESIGN then
    result := score + ply
  else
    if score <= -RESIGN then
      result := score - ply
  else
    result := score;
end;

function scale(phase: TMtrlStage; score: TScorePair): TScore; inline;
begin
  result :=
    (mg(score) * phase + eg(score) * (STAGE_OPENING - phase)) div STAGE_OPENING;
end;

function score_to_cp(score: TScore): TScore; inline;
begin
  result := (score * 100) div 256;
end;

function score_to_string(score: TScore; buf: PAnsiChar): PAnsiChar;
var f: double;
begin
  if score > RESIGN then
    if score <= SCORE_INF then
      sprintf(buf, '+M%d', (SCORE_MATE - score + 1) div 2)
    else
      sprintf(buf, '+INF')
  else
    if score < -RESIGN then
      if score >= -SCORE_INF then
        sprintf(buf, '-M%d', (SCORE_MATE + score + 1) div 2)
      else
        sprintf(buf, '-INF')
    else begin
      f := score_to_cp(score) / 100;
      sprintf(buf, '%+.2f', f);
    end;
  result := buf;
end;

function score_to_uci(score: TScore; buf: PAnsiChar): PAnsiChar;
begin
  if (score > RESIGN) then
    sprintf(buf, 'mate %d', (SCORE_MATE - score + 1) div 2)
  else
    if (score < -RESIGN) then
      sprintf(buf, 'mate -%d', (SCORE_MATE + score + 1) div 2)
    else
      sprintf(buf, 'cp %d', score_to_cp(score));
  result := buf;
end;

function valid_score(score: TScore): boolean;
begin
  result := (score >= -SCORE_INF) and (score <= SCORE_INF);
end;

procedure print_score(phase: TMtrlStage; name: PAnsiChar; w, b: TScorePair);
var
  ws, bs: TScore;
  cscore: array [0..6] of TScoreString;
begin
  ws := scale(phase, w);
  bs := scale(phase, b);
  printf('%12s |%6s |%6s %6s %6s |%6s %6s %6s' + LineEnding,
    name,
    score_to_string(ws + bs, @cscore[0]),
    score_to_string(ws, @cscore[1]),
    score_to_string(mg(w), @cscore[2]),
    score_to_string(eg(w), @cscore[3]),
    score_to_string(bs, @cscore[4]),
    score_to_string(mg(b), @cscore[5]),
    score_to_string(eg(b), @cscore[6])
  );
end;

end.
