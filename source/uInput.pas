
unit uInput;

interface

uses
  uUtil;

{$include platform.inc}

const
  CRITTER_VER: PAnsiChar = '1.1.38';
  INPUT_BUFFER_SIZE = 4096;

type
  TCmdProc = procedure(var p: TParser);
  TCmdEntry = record
    token: PAnsiChar;
    proc: TCmdProc;
  end;

var
  input_buffer: array [0..INPUT_BUFFER_SIZE-1] of AnsiChar;
  gtb_options_changed: boolean = false;

procedure UCI();

implementation

uses
  uBitboard, uBoard, uBook, uEngine, uEPD, uEval, uEvalDebug, uFen, uGtb,
  uHistory, uNotation, uMaterial, uMove, uMovegen, uPiece, uScore, uSearchBase,
  uSearchDef, uSee, uSmp, uSquare, uSystem, uTest, uTranstab, uZobrist;

var
  should_quit: boolean;

procedure UCI_info(depth: integer; score: TScore; bound: integer; pv: PMove);
var
  stats: PSearchStats;
  speed, i: integer;
  move_str: TMoveString;
  score_str: array [0..63] of AnsiChar;
  pv_str: array [0..1023] of AnsiChar;
begin
  stats := engine.sum_stats();
  engine.time_elapsed := get_time() - engine.time_start;
  if engine.time_elapsed <> 0 then
    speed := stats^.nodes * 1000 div engine.time_elapsed
  else
    speed := 0;

  if pv <> nil then begin

    score_to_uci(score, @score_str);
    case bound of
      LOWER_BOUND: strcat(@score_str, ' lowerbound');
      UPPER_BOUND: strcat(@score_str, ' upperbound');
    end;

    pv_str[0] := #0; i := 0;
    while (i < 128) and (pv^ <> NOMOVE) do begin
      strcat(@pv_str, move_to_string(pv^, move_str));
      strcat(@pv_str, ' ');
      inc(pv);
    end;

    printf('info multipv %d depth %d seldepth %d nodes %'+fmt64+'u'
      + ' time %d nps %d hashfull %d score %s pv %s' + LineEnding,
      engine.multicnt, depth, stats^.maxply + 1, stats^.nodes,
      integer(engine.time_elapsed), speed, tt.hash_full,
      score_str, pv_str);
    engine.last_output := engine.time_elapsed;
  end
  else
    if engine.time_elapsed > engine.last_output + 1000 then begin
      printf('info nodes %'+fmt64+'u nps %d hashfull %d tbhits %'+fmt64+'u' + LineEnding,
        stats^.nodes, speed, tt.hash_full, stats^.tbhits);
      engine.last_output := engine.time_elapsed;
    end;
end;

procedure console_info(depth: integer; score: TScore; bound: integer; pv: PMove);
var
  stats: PSearchStats;
  speed: integer;
  sign: AnsiChar;
  time: TTimeString;
  score_str: TScoreString;
  pv_str: array [0..1023] of AnsiChar;
begin
  stats := engine.sum_stats();
  if pv = nil then exit;

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
  printf('%2d/%2d%c %5s %9'+fmt64+'u %7d %6s %s' + LineEnding, depth, stats^.maxply+1, sign,
    time, stats^.nodes, speed, score_to_string(score, score_str), pv_str);
end;

function input_move(const SAN: PAnsiChar): TMove;
begin
  {$ifdef debug}
  printf({$I %FILE%} + ' ' + {$I %LINE%} + ' input_move(%s)' + LineEnding, SAN);
  {$endif}
  result := san_to_move(SAN, engine.board);
  if not engine.board.hm_is_ok(result) then
    result := NOMOVE;
end;

procedure cmd_quit(var p: TParser);
begin
  p.eof();
  should_quit := true;
end;

procedure cmd_isready(var p: TParser);
begin
  p.eof();
  if gtb_available and gtb_options_changed then begin
    gtb_unload();
    gtb_load();
    gtb_options_changed := false;
  end;
  printf('readyok' + LineEnding);
end;

procedure cmd_uci(var p: TParser);
const
  option: PAnsiChar = 'option name %s type %s ';

  procedure opt_check(name: PAnsiChar; default: PAnsiChar);
  begin
    printf(option, name, PAnsiChar('check'));
    printf('default %s' + LineEnding, default);
  end;

  procedure opt_spin(name: PAnsiChar; default, min, max: integer);
  begin
    printf(option, name, PAnsiChar('spin'));
    printf('default %d min %d max %d' + LineEnding, default, min, max);
  end;

  procedure opt_string(name: PAnsiChar; default: PAnsiChar);
  begin
    printf(option, name, PAnsiChar('string'));
    printf('default %s' + LineEnding, default);
  end;

  procedure opt_combo(name: PAnsiChar; default, values: PAnsiChar);
  begin
    printf(option, name, PAnsiChar('combo'));
    printf('default %s var %s' + LineEnding, default, values);
  end;

begin
  p.eof();
  engine.ai.UCI_mode := true;
  printf('id name OpenCritter %s' + LineEnding, CRITTER_VER);
  printf('id author Richard Vida' + LineEnding);
  opt_spin('Hash', 64, 16, MAX_HASH_SIZE);
  opt_check('Ponder', 'false');
  opt_check('OwnBook', 'true');
  opt_string('Book File', default_book);
  opt_check('UCI_Chess960', 'false');
  opt_spin('MultiPV', 1, 1, 100);
  opt_spin('Threads', get_cpu_count(), 1, MAX_THREADS);
  opt_spin('Minimum Split Depth', MIN_SPLIT_DEPTH, MSD_MIN, MSD_MAX);
  if gtb_available then
  begin
    opt_string('GaviotaTbPath', '\gtb');
    opt_spin('GaviotaTbCache', 32, 4, 256);
    opt_combo('GaviotaTbCompression', 'cp4', 'uncompressed var cp1 var cp2 var cp3 var cp4');
    opt_combo('Tablebase Usage', 'Only At Root', 'Disable var Only At Root var Everywhere');
  end;
  printf('option name Clear Hash type button' + LineEnding);
  printf('uciok' + LineEnding);
end;

procedure cmd_perft(var p: TParser);
var
  node_count: uint64;
  start_time: time_t;
  depth, msec, speed: cardinal;
begin
  depth := atoi(p.get_next());
  if (depth = 0) then exit;
  start_time := get_time();
  node_count := perft(engine.board, depth);
  msec := cardinal(get_time() - start_time);
  speed := cardinal(node_count) div (msec + 1);
  printf('%'+fmt64+'u nodes in %d ms, %d knps' + LineEnding, node_count, msec, speed);
end;

procedure cmd_divide(var p: TParser);
var
  node_count: uint64;
  start_time: time_t;
  depth, msec, speed: cardinal;
begin
  depth := atoi(p.get_next());
  if (depth = 0) then exit;
  start_time := get_time();
  node_count := divide(engine.board, depth);
  msec := cardinal(get_time() - start_time);
  speed := cardinal(node_count) div (msec + 1);
  printf('%'+fmt64+'u nodes in %d ms, %d knps' + LineEnding, node_count, msec, speed);
end;

procedure cmd_ucinewgame(var p: TParser);
begin
  p.eof();
  engine.reset();
end;

procedure cmd_setboard(var p: TParser);
var
  tokens: integer;
  fen: TFenString;
begin
  cmd_ucinewgame(p);
  tokens := 0;
  fen[0] := #0;
  while not p.eof() and (tokens < 6) do
  begin
    strncat(fen, p.get_next(), 250);
    strcat(fen, ' ');
  end;
  fen[strlen(fen) - 1] := #0;
  if not engine.load_fen(fen) then
    printf({$I %FILE%} + ' ' + {$I %LINE%} + ' illegal FEN' + LineEnding);
end;

procedure cmd_epdperft(var p: TParser);
var
  file_name: PAnsiChar;
  depth: integer;
begin
  if p.eof then exit;
  file_name := p.get_next();
  depth := atoi(p.get_next());
  if depth = 0 then depth := 4;
  epd_perft(file_name, depth, @perft);
end;

procedure cmd_test(var p: TParser);
begin
  if p.eof() then exit;
  run_test(p);
end;

procedure cmd_psq(var p: TParser);
begin
  p.eof();
  print_psq(engine.board);
end;

procedure cmd_position(var p: TParser);
var
  token: PAnsiChar;
  move: TMove;
  fen: TFenString;
begin
  if p.eof() then exit;
  fen[0] := #0;
  token := p.get_next();
  if stricmp(token, 'startpos') then
  begin
    engine.board.reset(@engine.root_state);
    engine.undocnt := 0;
    if p.eof() then exit;
    token := p.get_next();
  end else
  begin
    while not p.eof() do
    begin
      token := p.get_next();
      if stricmp(token, 'moves') then
        break;
      strncat(fen, token, 250);
      strcat(fen, ' ');
    end;
    fen[strlen(fen) - 1] := #0;
    if not engine.load_fen(fen) then
    begin
      printf({$I %FILE%} + ' ' + {$I %LINE%} + ' illegal FEN' + LineEnding);
      exit;
    end;
  end;

  if stricmp(token, 'moves') then
  begin
    while not p.eof() do
    begin
      token := p.get_next();
      move := input_move(token);
      if move = NOMOVE then
      begin
        printf({$I %FILE%} + ' ' + {$I %LINE%} + ' illegal move "%s"' + LineEnding, token);
        exit;
      end;
      engine.make_move(move);
    end;
  end;
end;

procedure cmd_go(var p: TParser);
var
  param, value: PAnsiChar;
  cmd: TEngineCmd;
  midx, ival: integer;
  move: TMove;
  tc: TTimeCtrl;
  mi: PMtrlInfo;
  ml: TMoveList;
begin
  zero_mem(@tc, sizeof(tc));
  cmd := EC_THINK;
  ml[0].move := NOMOVE;
  midx := 0;
  if p.eof() then begin
    tc.mode := TM_FIXED;
    tc.move_time := 10000;
  end
  else
    while not p.eof() do begin
      param := p.get_next();
      if stricmp(param, 'searchmoves') then
        while not p.eof() do begin
          param := p.get_next();
          move := input_move(param);
          if move = NOMOVE then break;
          ml[midx].move := move;
          inc(midx);
        end;

      if stricmp(param, 'infinite') then begin
        tc.max_depth := MAX_ITER;
        tc.mode := TM_FIXED;
        continue;
      end

      else if stricmp(param, 'ponder') then begin
        cmd := EC_PONDER;
        continue;
      end;

      if p.eof() then break;
      value := p.get_next();
      ival := atoi(value);

      if stricmp(param, 'wtime') then tc.time[WHITE] := ival else
      if stricmp(param, 'btime') then tc.time[BLACK] := ival else
      if stricmp(param, 'winc') then tc.increment[WHITE] := ival else
      if stricmp(param, 'binc') then tc.increment[BLACK] := ival else
      if stricmp(param, 'movestogo') then tc.moves_to_go := ival else
      if stricmp(param, 'movetime') then tc.move_time := ival else
      if stricmp(param, 'depth') then tc.max_depth := ival else
      if stricmp(param, 'nodes') then tc.max_nodes := ival;

      if tc.max_depth or tc.max_nodes or tc.move_time <> 0 then
        tc.mode := TM_FIXED;
    end;

  with engine do begin
    system.move(tc, time_ctrl, sizeof(tc));
    move_list := nil;
    if midx <> 0 then begin
      ml[midx].move := NOMOVE;
      move_list := @ml;
    end;

    if ai.UCI_mode then
      info_callback := @UCI_info
    else
      info_callback := @console_info;

    if new_game then begin
      mi := get_mtrl_info(board, evaluator^.mtrl_table^);
      hist.clear(mi^.phase);
      new_game := false;
    end;

    send_command(cmd);
  end;
end;

procedure cmd_ponderhit(var p: TParser);
begin
  p.eof();
  if engine.pondering then
    engine.send_command(EC_PONDERHIT);
end;

procedure cmd_setoption(var p: TParser);
var
  token: PAnsiChar;
  name: array [0..255] of AnsiChar;
  value: array [0..1023] of AnsiChar;
begin
  name[0] := #0;
  repeat
    if p.eof() then exit;
    token := p.get_next();
  until stricmp(token, 'name');

  while not p.eof() do begin
    token := p.get_next();
    if stricmp(token, 'value') then break;
    if name[0] <> #0 then strncat(name, ' ', 255);
    strncat(name, token, 255);
  end;

  if stricmp(name, 'Clear Hash') then begin
    tt.clear();
    exit;
  end;

  if p.eof() then exit;

  value[0] := #0;
  while not p.eof() do begin
    token := p.get_next();
    if value[0] <> #0 then strncat(value, ' ', 1023);
    strncat(value, token, 1023);
  end;

  if stricmp(name, 'Hash') then
    tt.set_size(atoi(value))
  else
  if stricmp(name, 'OwnBook') then
    TBook.use_book := stricmp(value, 'true')
  else
  if stricmp(name, 'Book File') then begin
    engine.init_book(value);
  end
  else
  if stricmp(name, 'MultiPV') then
    engine.ai.multi_pv := max(1, atoi(value))
  else
  if stricmp(name, 'Threads') then
    engine.set_threads(atoi(value))
  else
  if stricmp(name, 'UCI_Chess960') then
    Chess_960 := stricmp(value, 'true')
  else
  if stricmp(name, 'Minimum split depth') then
    engine.set_split_depth(atoi(value))
  else
  if stricmp(name, 'GaviotaTbPath') then begin
    strcpy(gtb_path, value);
    gtb_options_changed := true;
  end
  else
  if stricmp(name, 'GaviotaTbCache') then begin
    gtb_cache := min(max(4, atoi(value)), 256);
    gtb_options_changed := true;
  end
  else
  if stricmp(name, 'GaviotaTbCompression') then begin
    if stricmp(value, 'uncompressed') then gtb_compression := tb_UNCOMPRESSED else
    if stricmp(value, 'cp1') then gtb_compression := tb_CP1 else
    if stricmp(value, 'cp2') then gtb_compression := tb_CP2 else
    if stricmp(value, 'cp3') then gtb_compression := tb_CP3 else
    if stricmp(value, 'cp4') then gtb_compression := tb_CP4;
    gtb_options_changed := true;
  end
  else
  if stricmp(name, 'Tablebase Usage') then begin
    if stricmp(value, 'Disable') then use_gtb := TB_DISABLED else
    if stricmp(value, 'Only At Root') then use_gtb := TB_AT_ROOT else
    if stricmp(value, 'Everywhere') then use_gtb := TB_EVERYWHERE;
  end
  else
  if stricmp(name, 'Ponder') then
    engine.pondering_enabled := stricmp(value, 'true');
end;

procedure cmd_stop(var p: TParser);
begin
  p.eof();
  engine.stop_think();
end;

procedure cmd_wait(var p: TParser);
var
  ms: integer;
begin
  ms := atoi(p.get_next);
  if ms <= 0 then ms := integer(INFINITE);
  engine.wait(ms);
end;

procedure cmd_book(var p: TParser);
begin
  p.eof();
  print_book_moves(engine.board);
end;

procedure cmd_probe(var p: TParser);
begin
  p.eof();
  if not gtb_available then
    printf('Tablebase support not available' + LineEnding)
  else
    if popcnt(engine.board.bb.occupied) > gtb_num_pieces then
      printf('More than %d pieces!' + LineEnding, gtb_num_pieces)
    else
      print_probe_result(engine.board);
end;

procedure cmd_makebook(var p: TParser);
var
  arg, book_name, pgn_name: PAnsiChar;
  max_ply, min_games, min_score: integer;
  uniform: boolean;
  color_filter: integer;
begin
  book_name := nil; pgn_name := nil;
  if not p.eof() then pgn_name := p.get_next();
  if not p.eof() then book_name := p.get_next();

  if book_name = nil then begin
    printf('syntax:' + LineEnding + LineEnding);
    printf('makebook <file_name.pgn> <book_name.cbk> mingames <n> maxply <m>' + LineEnding
      + 'minscore <x> [whiteonly | blackonly] [uniform]' + LineEnding + LineEnding);
    exit;
  end;

  max_ply := 20;
  min_games := 1;
  min_score := 0;
  uniform := false;
  color_filter := -1;

  while not p.eof() do begin
    arg := p.get_next();
    if stricmp(arg, 'uniform') then uniform := true
    else if stricmp(arg, 'whiteonly') then color_filter := integer(WHITE)
    else if stricmp(arg, 'blackonly') then color_filter := integer(BLACK)
    else if not p.eof() then begin
      if stricmp(arg, 'maxply') then max_ply := atoi(p.get_next())
      else if stricmp(arg, 'mingames') then min_games := atoi(p.get_next())
      else if stricmp(arg, 'minscore') then min_score := atoi(p.get_next())
      else begin
        printf('unknown argument: %s' + LineEnding, arg);
        exit;
      end;
    end
    else begin
      printf('missing value for: %s' + LineEnding, arg);
      exit;
    end;
  end;
  book_create(pgn_name, book_name, max_ply, min_games, min_score, color_filter, uniform);
end;

procedure cmd_mergebook(var p: TParser);
var
  src1, src2, dest: PAnsiChar;
begin
  src1 := nil; src2 := nil; dest := nil;
  if not p.eof() then src1 := p.get_next();
  if not p.eof() then src2 := p.get_next();
  if not p.eof() then dest := p.get_next();

  if (dest = nil) then begin
    printf('syntax:' + LineEnding + LineEnding);
    printf('mergebook <book1.cbk> <book2.cbk> <output.cbk>' + LineEnding + LineEnding);
    exit;
  end;
  book_merge(src1, src2, dest);
end;

procedure cmd_epdtest(var p: TParser);
var
  file_name: PAnsiChar;
  time_limit: integer;
  strict_time: boolean;
begin
  if p.eof() then exit;
  file_name := p.get_next();
  time_limit := atoi(p.get_next());
  if time_limit = 0 then
    time_limit := 10000;
  strict_time := not p.eof() and (p.get_next()[0] = '!');
  epd_test(file_name, time_limit, not strict_time);
end;

procedure cmd_flip(var p: TParser);
begin
  p.eof();
  flip_board(engine.board);
end;

procedure cmd_eval(var p: TParser);
var
  b: PBoard;
  hashkey: TKey;
  s1, s2: TScore;
  c1, c2: TScoreString;
  fen: TFenString;
begin
  b := @engine.board;
  hashkey := b^.st^.hashkey;
  with PEvalDump(evaluator)^ do begin
    clear_stats();
    uncache(hashkey); evaluate(b^, 0, 0, 0); s1 := b^.st^.eval;
    flip_board(b^);
    uncache(hashkey); evaluate(b^, 0, 0, 0); s2 := b^.st^.eval;
    flip_board(b^);
    if s1 = s2 then begin
      printf('eval = %s' + LineEnding, score_to_string(s1, c1));
      if stats.eg_recog <> 0 then
        printf('evaluated by engame knowledge' + LineEnding + LineEnding)
      else
        print_eval();
    end
    else begin
      printf('eval assymetry (orig = %s, flipped = %s)' + LineEnding,
        score_to_string(s1, c1), score_to_string(s2, c2));
      printf('original: %s' + LineEnding, encode_fen(engine.board, fen));
      uncache(hashkey); print_eval(); flip_board(b^);
      printf('flipped: %s' + LineEnding, encode_fen(b^, fen));
      uncache(hashkey); print_eval(); flip_board(b^);
    end;
  end;
end;

procedure cmd_see(var p: TParser);
var
  m: TMove;
  see_result: PAnsiChar;
begin
  if p.eof() then exit;
  m := input_move(p.get_next());
  if m <> NOMOVE then begin
    if bad_see(engine.board, m) then
      see_result := 'SEE_BAD'
    else
      see_result := 'SEE_GOOD';
    printf('result = %s' + LineEnding, see_result);
  end;
end;

const
  cmdtab: array [0..24] of TCmdEntry = (
    ( token: 'quit';       proc: @cmd_quit ),
    ( token: 'uci';        proc: @cmd_uci ),
    ( token: 'go';         proc: @cmd_go ),
    ( token: 'ucinewgame'; proc: @cmd_ucinewgame ),
    ( token: 'isready';    proc: @cmd_isready ),
    ( token: 'stop';       proc: @cmd_stop ),
    ( token: 'ponderhit';  proc: @cmd_ponderhit),
    ( token: 'position';   proc: @cmd_position ),
    ( token: 'setoption';  proc: @cmd_setoption ),
    ( token: 'setboard';   proc: @cmd_setboard ),
    ( token: '?';          proc: @cmd_stop ),
    ( token: 'perft';      proc: @cmd_perft ),
    ( token: 'divide';     proc: @cmd_divide ),
    ( token: 'epdtest';    proc: @cmd_epdtest ),
    ( token: 'epdperft';   proc: @cmd_epdperft ),
    ( token: 'psq';        proc: @cmd_psq ),
    ( token: 'see';        proc: @cmd_see ),
    ( token: 'eval';       proc: @cmd_eval ),
    ( token: 'flip';       proc: @cmd_flip ),
    ( token: 'probe';      proc: @cmd_probe ),
    ( token: 'book';       proc: @cmd_book ),
    ( token: 'makebook';   proc: @cmd_makebook ),
    ( token: 'mergebook';  proc: @cmd_mergebook ),
    ( token: 'wait';       proc: @cmd_wait ),
    ( token: 'test';       proc: @cmd_test )
  );

procedure parse_cmd(line: PAnsiChar);
var
  parser: TParser;
  move: TMove;
  cmd: PAnsiChar;
  i: integer;
begin
  parser.init(line, ' ');
  cmd := parser.get_next();

  i := 0;
  while i <= high(cmdtab) do begin
    if stricmp(cmd, cmdtab[i].token) then begin
      cmdtab[i].proc(parser);
      exit;
    end;
    inc(i);
  end;
  move := input_move(cmd);
  if move <> NOMOVE then
    engine.make_move(move)
  else
    printf('invalid move or command: %s' + LineEnding, cmd);
end;

procedure parse_input(line: PAnsiChar);
var
  parser: TParser;
begin
  if line[0] = #0 then begin
    print_board(engine.board);
    exit;
  end;
  parser.init(line, ';');
  while not parser.eof() do
    parse_cmd(parser.get_next());
end;

procedure UCI();
var
  last_chr: size_t;
begin
  tt.init();
  engine.init();
  engine.info_callback := @console_info;

  printf('Eval cache: %9d entries of %2d bytes = %4d KB' + LineEnding,
    ECACHE_SIZE, integer(sizeof(TEvalCacheEntry)),
    ECACHE_SIZE * integer(sizeof(TEvalCacheEntry)) div (1 shl 10));
  printf('Pawn hash : %9d entries of %2d bytes = %4d MB' + LineEnding,
    PHASH_SIZE, integer(sizeof(TPawnHashEntry)),
    PHASH_SIZE * integer(sizeof(TPawnHashEntry)) div (1 shl 20));

  printf('%d cpu(s) detected' + LineEnding, engine.num_threads);

  TBook.use_book := engine.init_book(default_book);

  should_quit := false;
  if StrLen(input_buffer) <> 0 then begin
    parse_input(input_buffer);
  end;
  while not should_quit do begin
    if fgets(input_buffer, sizeof(input_buffer) - 1, stdin) = nil then break;
    last_chr := strlen(input_buffer);
    if (last_chr > 0) and (input_buffer[last_chr-1] = #10) then
      input_buffer[last_chr-1] := #0;
    parse_input(input_buffer);
  end;

  engine.destroy();
  tt.destroy();
end;

end.
