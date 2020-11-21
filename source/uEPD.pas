
unit uEPD;

interface

uses
  uBoard;

{$include platform.inc}

type
  TTestProc = function(var b: TBoard; depth: integer): uint64;

procedure epd_test(file_name: PAnsiChar; time_limit: integer; early_exit: boolean);
procedure epd_perft(file_name: PAnsiChar; max_depth: integer; test_proc: TTestProc);

implementation

uses
  uBook, uEngine, uEval, uGtb, uHistory, uMaterial, uMove, uNotation,
  uSmp, uScore, uSearchDef, uSystem, uTranstab, uUtil;

type
  TProblemType = ( BEST_MOVE, AVOID_MOVE );

  TEpdEntry = record
    problem_type: TProblemType;
    fen: PAnsiChar;
    movecnt, pscnt: integer;
    moves: array [0..63] of TMove;
    partial: array [0..63] of TMoveStack;
  end;

function open_EPD(file_name: PAnsiChar; out epd: TextFile): boolean;
var
  fname: array[0..255] of AnsiChar;
begin
  {$I-}
  result := true;
  strncpy(fname, file_name, 256);
  AssignFile(epd, ansistring(fname)); Reset(epd);
  if IOResult = 0 then exit;

  strncpy(fname, file_name, 256);
  strncat(fname, '.epd', 256);
  AssignFile(epd, ansistring(fname)); Reset(epd);
  if IOResult = 0 then exit;

  strncpy(fname, 'tests/epd/', 256);
  strncat(fname, file_name, 256);
  AssignFile(epd, ansistring(fname)); Reset(epd);
  if IOResult = 0 then exit;

  strncat(fname, '.epd', 256);
  AssignFile(epd, ansistring(fname)); Reset(epd);
  if IOResult = 0 then exit;
  {$I+}
  result := false;
end;

var
  epd_entry: TEpdEntry;
  solution_depth: integer;
  solution_move: TMove;
  solution_time: time_t;
  early_abort: boolean;

procedure epd_info(depth: integer; score: TScore; bound: integer; pv: PMove);
var
  stats: PSearchStats;
  speed, i: integer;
  solved: boolean;
  sign: AnsiChar;
  time: TTimeString;
  score_str: TScoreString;
  pv_str: array [0..1023] of AnsiChar;
begin
  stats := engine.sum_stats();
  if pv = nil then exit;

  solution_move := pv^;
  with epd_entry do begin
    solved := problem_type <> BEST_MOVE;
    for i := 0 to movecnt - 1 do
      if moves[i] = solution_move then begin
        solved := not solved;
        break;
      end;
  end;

  if solved then begin
    highlight_on;
    if solution_depth = 0 then begin
      solution_depth := depth;
      solution_time := engine.time_elapsed + 1;
    end;
    if early_abort and (depth >= 14) and (bound <> UPPER_BOUND) then
      if (depth - solution_depth >= 3) then
        engine.stop := true;
  end
  else begin
    solution_depth := 0;
    solution_time := 0;
  end;

  case bound of
    LOWER_BOUND: sign := '+';
    UPPER_BOUND: sign := '-';
    else sign := ' ';
  end;

  time_to_string(engine.time_elapsed, time);
  if engine.time_elapsed <> 0 then
    speed := stats^.nodes * 1000 div engine.time_elapsed
  else
    speed := 0;

  pv_to_string(pv, engine.board, pv_str);
  printf('  %2d/%2d%c %5s %9'+fmt64+'u %7d %6s %s' + LineEnding, depth, stats^.maxply+1, sign,
    time, stats^.nodes, speed, score_to_string(score, score_str), pv_str);

  highlight_off;
end;

procedure epd_test(file_name: PAnsiChar; time_limit: integer; early_exit: boolean);
var
  credit, partial_credit, max_credit: integer;
  positions, solved, i: integer;
  time_total, time_rated: time_t;
  solutions, id, ps: PAnsiChar;
  c0_move, c0_score, c0_end: PAnsiChar;
  move: TMove;
  mi: PMtrlInfo;
  parser, mp, c0: TParser;
  epd: TextFile;
  cmove: TMoveString;
  t1, t2: TTimeString;
  line: array [0..1023] of AnsiChar;

begin
  early_abort := early_exit;

  with engine do begin
    fillchar(time_ctrl, sizeof(TTimeCtrl), 0);
    time_ctrl.move_time := time_limit;
    time_ctrl.mode := TM_FIXED;
    info_callback := @epd_info;
  end;

  if not open_epd(file_name, epd) then begin
    printf('file "%s" not found' + LineEnding, file_name);
    exit;
  end;

  positions := 0; solved := 0;
  partial_credit := 0; max_credit := 0;
  time_total := 0; time_rated := 0;

  while not eof(epd) do begin
    {$I-} readln(epd, line); {$I+}
    if IOResult <> 0 then break;
    if line[0] = #0 then continue;

    parser.init(line, ';');
    epd_entry.fen := parser.get_next();
    epd_entry.problem_type := BEST_MOVE;
    solutions := strstr(epd_entry.fen, 'bm');
    if solutions = nil then begin
      solutions := strstr(epd_entry.fen, 'am');
      if solutions = nil then continue;
      epd_entry.problem_type := AVOID_MOVE;
    end;

    if not engine.load_fen(epd_entry.fen) then continue;
    inc(positions);

    epd_entry.movecnt := 0;
    mp.init(@solutions[3], ' ');
    while not mp.eof() do begin
      move := san_to_move(mp.get_next(), engine.board);
      if (move <> NOMOVE) and engine.board.hm_is_ok(move) then begin
        epd_entry.moves[epd_entry.movecnt] := move;
        inc(epd_entry.movecnt);
      end;
    end;
    printf('%3d)', positions);
    epd_entry.pscnt := 0;
    while true do begin
      if parser.eof() then begin
        printf(LineEnding);
        break;
      end;
      id := parser.get_next();
      printf('%s;', id);
      if strncmp(id, ' c0 "', 5) = 0 then begin
        c0_end := strchr(id, integer('"'));
        if c0_end = nil then break;
        c0_end^ := #0;
        c0.init(@id[5], ',');
        while not c0.eof() do begin
          ps := c0.get_next();
          c0_move := strtok(ps, '=');
          move := san_to_move(c0_move, engine.board);
          if (move <> NOMOVE) and engine.board.hm_is_ok(move) then begin
            c0_score := strtok(nil, '=');
            if c0_score = nil then continue;
            with epd_entry do begin
              partial[pscnt].move := move;
              partial[pscnt].score := atoi(c0_score);
              inc(pscnt);
            end;
          end;
        end;
      end;
    end;

    printf('     FEN: %s' + LineEnding, epd_entry.fen);
    if epd_entry.problem_type = AVOID_MOVE then
      printf('     Avoid move(s): ')
    else
      printf('     Searching for: ');
    for i := 0 to epd_entry.movecnt - 1 do
      printf('%s ', move_to_san(epd_entry.moves[i], engine.board, cmove));
    printf(LineEnding);

    solution_depth := 0;
    solution_time := 0;
    TBook.out_of_book := 9999;
    tt.clear();
    if gtb_available then
      tbcache_flush();

    with engine do begin
      for i := 0 to num_threads - 1 do
        threads[i].evaluator.clear_cache();
      mi := get_mtrl_info(board, evaluator^.mtrl_table^);
      hist.clear(mi^.phase);
      engine.think();
    end;
    printf(LineEnding);
    engine.calc_time();
    inc(time_total, engine.time_elapsed);
    if solution_time <> 0 then begin
      inc(time_rated, solution_time);
      inc(solved);
      printf(' Found in %5s', time_to_string(integer(solution_time), t1));
    end
    else begin
      inc(time_rated, engine.time_elapsed);
      printf(' Not found :(');
    end;
    printf(', Total time: %5s, Rated time: %5s, %d of %d matching' + LineEnding,
      time_to_string(integer(time_total), t1), time_to_string(integer(time_rated), t2),
      solved, positions);

    with epd_entry do
      if pscnt <> 0 then begin
        credit := 0;
        for i := 0 to pscnt - 1 do
          if partial[i].move = solution_move then begin
            credit := partial[i].score;
            break;
          end;
        inc(partial_credit, credit);
        inc(max_credit, 10);
        printf(' Partial credit: %2d (total %d of %d)' + LineEnding, credit,
          partial_credit, max_credit);
      end;
    printf(LineEnding);
  end;
  engine.reset();
end;

procedure epd_perft(file_name: PAnsiChar; max_depth: integer; test_proc: TTestProc);
var
  nodes, nodes_expected, nodes_total: uint64;
  time_start, time_finish, time_total: time_t;
  positions, total, passed, failed, depth: integer;
  msec, speed: cardinal;
  line: array [0..1023] of AnsiChar;
  fen, results: PAnsiChar;
  parser, p2: TParser;
  epd: TextFile;
begin
  if not open_EPD(file_name, epd) then begin
    printf('file "%s" not found' + LineEnding, file_name);
    exit;
  end;

  positions := 0; total := 0; passed := 0; failed := 0;
  nodes_total := 0; time_total := 0;

  while not eof(epd) do
  begin
    {$I-}
    readln(epd, line);
    {$I+}
    if IOResult <> 0 then break;
    inc(positions);
    parser.init(line, ';');
    fen := parser.get_next();
    printf('#%d ', positions);
    if not engine.load_fen(fen) then
    begin
      printf({$I %FILE%} + ' ' + {$I %LINE%} + ' illegal FEN' + LineEnding);
      continue;
    end;
    printf('FEN: %s' + LineEnding, fen);
    results := parser.get_next();
    while strlen(results) <> 0 do begin
      p2.init(results, ' ');
      depth := atoi(p2.get_next() + 1);
      nodes_expected := _strtoui64(p2.get_next(), nil, 10);
      if (depth < 1) or (depth > max_depth) or (nodes_expected = 0) then break;
      time_start := get_time();
      nodes := test_proc(engine.board, depth);
      time_finish := get_time();
      msec := time_finish - time_start;
      inc(time_total, msec);
      inc(nodes_total, nodes);
      inc(total);
      if nodes = nodes_expected then begin
        printf('PASSED');
        inc(passed);
      end
      else begin
        printf('FAILED');
        inc(failed);
      end;
      speed := cardinal(nodes div (uint64(msec) + 1));
      printf(' - Depth: %d Nodes: %'+fmt64+'u, Expected: %'+fmt64+'u (%d ms, %d knps )' + LineEnding,
        depth, nodes, nodes_expected, msec, speed);
      results := parser.get_next();
    end;
    printf('----------------------------------------------------------------' + LineEnding);
  end;
  {$I-} CloseFile(epd); {$I+}
  speed := cardinal(nodes_total div (time_total + 1));
  printf('**** Summary ****' + LineEnding);
  printf('Passed: %d, Failed %d/%d, Total time: %d' + LineEnding,
    passed, failed, total, cardinal(time_total));
  printf('Total nodes: %'+fmt64+'u, Avg speed: %d knps' + LineEnding + LineEnding, nodes_total, speed);
  engine.reset();
end;

end.
