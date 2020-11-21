
unit uTest;

interface

uses
  uUtil;

{$include platform.inc}

procedure run_test(var p: TParser);

implementation

uses
  uBoard, uEngine, uEPD, uEval, uEvalDebug, uFen, uScore, uMove, uMoveGen,
  uSystem;

function test_eval(var b: TBoard; depth: integer): uint64;
var
  eval_orig, eval_flipped: TScore;
  ml: TMoveList;
  e: PEvalDump;
  u: TStateInfo;
  mptr: PMoveStack;
  in_chk: boolean;
  s1, s2: TScoreString;
  fen: TFenString;
begin
  if depth = 1 then begin
    result := gen_legal_moves(b, ml);
    exit;
  end;
  result := 0;
  e := PEvalDump(evaluator);
  in_chk := b.st^.checkersBB <> 0;
  gen_all(b, ml);
  mptr := @ml;
  while mptr^.move <> NOMOVE do begin
    if in_chk or b.is_legal(mptr^.move) then begin
      b.make_move(mptr^.move, u);

      e^.uncache(b.st^.hashkey);
      e^.evaluate(b, 0, 0, 0);
      eval_orig := b.st^.eval;

      flip_board(b);

      e^.uncache(b.st^.hashkey);
      e^.evaluate(b, 0, 0, 0);
      eval_flipped := b.st^.eval;

      flip_board(b);
      if eval_orig <> eval_flipped then begin
        printf('eval assymetry (orig = %s, flipped = %s)' + LineEnding,
          score_to_string(eval_orig, s1), score_to_string(eval_flipped, s2));
        printf('original: %s' + LineEnding, encode_fen(b, fen));
        e^.uncache(b.st^.hashkey);
        e^.print_eval();
        flip_board(b);
        printf('flipped: %s' + LineEnding, encode_fen(b, fen));
        e^.uncache(b.st^.hashkey);
        e^.print_eval();
        flip_board(b);
        readln;
      end;
      inc(result, test_eval(b, depth-1));
      b.undo_move(mptr^.move);
    end;
    inc(mptr);
  end;
end;

procedure run_test(var p: TParser);
var
  test, suite: PAnsiChar;
  depth: integer;
begin
  test := p.get_next();
  if p.eof() then exit;
  suite := p.get_next();
  depth := atoi(p.get_next());
  if depth = 0 then depth := 4;
  if stricmp(test, 'eval') then
    epd_perft(suite, depth, @test_eval)
  else begin
    printf('unknown test %s' + LineEnding, test);
    exit;
  end;
  printf('OK' + LineEnding);
  engine.reset();
end;

end.
