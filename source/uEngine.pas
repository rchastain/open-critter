
unit uEngine;

interface

uses
  uBoard, uEval, uMove, uRoot, uSmp, uSystem;

{$include platform.inc}

const
  INFINITE = cardinal($ffffffff);

type
  TEngineCmd =
    ( EC_THINK, EC_PONDER, EC_PONDERHIT, EC_NOOP, EC_QUIT );

  PEngine = ^TEngine;
  TEngine = object(TSmpManager)
    ai: TRoot;
    command: TEngineCmd;
    new_game: boolean;
    searchmoves: boolean;
    undocnt: integer;
    move_list: PMoveList;
    board: TBoard;
    evaluator: PEvaluator;
    root_state: TStateInfo;
    hReady, hCommand: TEvent;
    undostack: array [0..2*MAX_PLY-1] of TStateInfo;
    procedure allocate_time();
    procedure think();
    procedure init();
    procedure destroy();
    procedure reset(clear_tt: boolean = true);
    procedure stop_think();
    procedure wait(ms: cardinal = INFINITE);
    procedure send_command(cmd: TEngineCmd);
    procedure make_move(move: TMove);
    procedure idle_loop(threadID: integer; sp_wait: PSplitPoint);
    function init_book(name: PAnsiChar): boolean;
    function load_fen(const fen: PAnsiChar): boolean;
  end;

var
  engine: TEngine;

implementation

uses
{$ifndef unix}
  Windows,
{$endif}
  uBook, uGtb, uNotation, uSearch, uSquare, uSearchDef, uTranstab;

function main_thread(engine: pointer): TThreadResult; register;
begin
  result := 0;
  randomize();
  with PEngine(engine)^ do begin
    while true do begin
      hReady.signal();
      stop := false;
      hCommand.wait_for(INFINITE);
      case command of
        EC_NOOP: ;
        EC_PONDER: think();
        EC_THINK: think();
        EC_QUIT: exit;
      end;
    end;
  end;
end;

procedure TEngine.send_command(cmd: TEngineCmd);
begin
  if cmd = EC_PONDERHIT then begin
    assert(pondering);
    command := cmd;
    pondering := false;
    exit;
  end;
  if not idle then stop_think();
  hReady.wait_for(INFINITE);
  assert(idle);
  idle := false;
  command := cmd;
  pondering := (cmd = EC_PONDER);
  hCommand.signal();
end;

procedure TEngine.stop_think();
begin
  if not idle then begin
    stop_threads();
    wait();
  end;
end;

procedure TEngine.wait(ms: cardinal = INFINITE);
begin
  hReady.wait_for(ms);
  hReady.signal();
end;

procedure TEngine.reset(clear_tt: boolean = true);
var
  i: integer;
begin
  board.reset(@root_state);
  undocnt := 0;
  TBook.out_of_book := 0;
  if clear_tt then begin
    tt.clear();
    for i := 0 to num_threads-1 do
      threads[i].evaluator.clear_cache();
  end;
  new_game := true;
end;

function helper_main(tp: pointer): TThreadResult; register;
begin
  with PThreadParam(tp)^ do
    TEngine(smp_mgr^).idle_loop(threadID, nil);
  result := 0;
end;

procedure TEngine.init();
var
  tid: TThreadID;
begin
  fillchar(threads, sizeof(threads), 0);
  hReady.init();
  hCommand.init();
  num_threads := 0;
  pondering_enabled := false;
  if gtb_available then
    use_gtb := TB_AT_ROOT
  else
    use_gtb := TB_DISABLED;
  smp_init(@helper_main);
  set_threads(get_cpu_count());
  set_split_depth(MIN_SPLIT_DEPTH);
  self.evaluator := @threads[0].evaluator;
  uEval.evaluator := self.evaluator;
  reset();
  ai.thread := @threads[0];
  ai.threadID := 0;
  ai.uci_mode := false;
  searchmoves := false;
  ai.multi_pv := 1;
  move_list := nil;
  idle := true;
  start_thread(@main_thread, @self, tid);
end;

procedure TEngine.destroy();
begin
  if not idle then stop_think();
  send_command(EC_QUIT);
  free_evaluators;
  smp_destroy();
  hCommand.done();
  hReady.done();
end;

procedure TEngine.allocate_time();
var
  abs_time_left, time_target, time_limit: integer;
  incr, moves_left: integer;
begin
  with time_ctrl do begin
    if (move_time <> 0) or (max_depth <> 0) or (max_nodes <> 0) then
      mode := TM_FIXED;

    time_target := move_time;
    time_limit := move_time;

    if mode = TM_NORMAL then begin
      abs_time_left := time[board.us];
      incr := increment[board.us];

      if moves_to_go <> 0 then
        moves_left := min(moves_to_go, 32)
      else
        moves_left := 32;

      time_target := (abs_time_left div moves_left) + incr;

      if pondering_enabled then
        time_target := min((time_target * 5) div 4, abs_time_left - 100);

      if (incr <> 0) and (abs_time_left < incr) then
        dec(time_target, time_target div 2);

      time_limit := min(time_target * 5, abs_time_left div 5);

      if (time_limit < time_target) or (abs_time_left - time_limit < 2000) then begin
        if time_target > 200 then
          dec(time_target, 100);
        time_limit := time_target;
      end;
    end;
  end;

  time_max := time_limit;
  time_normal := (time_target * 100 ) div 100;
  time_failhi := min((time_target * 133) div 100, time_max);
  if pondering_enabled then
    time_easy := (time_target * 33) div 100
  else
    time_easy := (time_target * 25) div 100;
end;

function TEngine.load_fen(const fen: PAnsiChar): boolean;
begin
  {$ifdef debug}
  printf({$I %FILE%} + ' ' + {$I %LINE%} + ' TEngine.load_fen(%s)' + LineEnding, fen);
  {$endif}
  undocnt := 0;
  result := board.load_fen(fen);
end;

procedure TEngine.make_move(move: TMove);
begin
  board.make_move(move, undostack[undocnt]);
  inc(undocnt);
  if (board.st^.flags and FLAG_EXACT) <> 0 then
    board.update_state()
  else
    threads[0].evaluator.evaluate(board, 0, 0, 0);
  if board.st^.rule50 = 0 then begin
    board.trim_stack();
    undocnt := 0;
  end;
end;

procedure TEngine.think();
var
  i, speed: integer;
  lazy, ph, ec: double;
  evals: uint64;
  bm, pm: TMoveString;
  es: TEvalStats;
  st: TStateInfo;
begin
  idle := false;
  allocate_time();

  if not searchmoves then
    ai.root_list.init(board, move_list);

  fillchar(stats, sizeof(stats), 0);
  for i := 0 to num_threads - 1 do begin
    fillchar(threads[i].stats, sizeof(TSearchStats), 0);
    threads[i].evaluator.clear_stats();
  end;
  splits := 0;

  free_cpu_mask := 0;
  ai.think(board);

  idle := true;
  if ai.UCI_mode then begin
    if ai.ponder_move <> NOMOVE then
      printf('bestmove %s ponder %s' + LineEnding, move_to_string(ai.root_best, bm),
        move_to_string(ai.ponder_move, pm))
    else
      printf('bestmove %s' + LineEnding, move_to_string(ai.root_best, bm));
  end
  else begin
    sum_stats();
    zero_mem(pointer(@es), sizeof(TEvalStats));
    for i := 0 to num_threads - 1 do
      with threads[i].evaluator.stats do begin
        inc(es.ecache_probes, ecache_probes);
        inc(es.ecache_hits, ecache_hits);
        inc(es.phash_probes, phash_probes);
        inc(es.phash_hits, phash_hits);
        inc(es.lazy_evals, lazy_evals);
        inc(es.eg_recog, eg_recog);
      end;
    ec := 0; ph := 0; lazy := 0; speed := 0;

    if es.ecache_probes <> 0 then
      ec := es.ecache_hits / es.ecache_probes * 100;

    if es.phash_probes <> 0 then
      ph := es.phash_hits / es.phash_probes * 100;

    evals := es.ecache_probes - es.ecache_hits;
    if evals <> 0 then
      lazy := es.lazy_evals / evals * 100;

    if engine.time_elapsed <> 0 then
      speed := stats.nodes div engine.time_elapsed;

    move_to_san(ai.root_best, board, bm);
    if ai.ponder_move <> NOMOVE then begin
      board.make_move_fast(ai.root_best, st);
      move_to_san(ai.ponder_move, board, pm);
      board.undo_move(ai.root_best);
      printf('bestmove %s ponder %s' + LineEnding, bm, pm);
    end
    else
      printf('bestmove %s' + LineEnding, move_to_san(ai.root_best, board, bm));

    printf(LineEnding + 'time: %d nodes: %' + fmt64 + 'u evals: %' + fmt64 + 'u knps: %d' + LineEnding,
      integer(time_elapsed), stats.nodes, evals, speed);
    printf('phash: %2.2f%% evalcache: %2.2f%% lazy: %2.2f%% eg_recog: %' + fmt64 + 'u' + LineEnding,
      ph, ec, lazy, es.eg_recog);
    if splits <> 0 then
      printf('splits: %' + fmt64 + 'u' + LineEnding, splits);
  end;
end;

procedure TEngine.idle_loop(threadID: integer; sp_wait: PSplitPoint);
var
  thread: PThread;
  helper: TSearch;
  nt: TNodeType;
begin
  thread := @threads[threadID];

  helper.threadID := threadID;
  helper.thread := thread;

  while true do begin
    if threadID <> 0 then begin
      if exit_flag then begin
        thread^.state := TS_TERMINATED;
        exit;
      end;
      while idle or (threadID >= num_threads) do begin
        thread^.state := TS_SLEEPING;
        thread^.hIdle.wait_for(INFINITE);
        thread^.state := TS_IDLE;
        if threadID < num_threads then begin
          EnterCriticalSection(smp_lock);
          free_cpu_mask := free_cpu_mask or (1 shl threadID);
          LeaveCriticalSection(smp_lock);
        end;
      end;
    end;
    if thread^.state = TS_WORK_PENDING then begin
      assert(not idle and not exit_flag);
      EnterCriticalSection(smp_lock);
      free_cpu_mask := free_cpu_mask and not (1 shl threadID);
      LeaveCriticalSection(smp_lock);
      thread^.state := TS_WORKING;
      helper.board.clone_from(thread^.split_point^.board^, @helper.root_state);
      nt := thread^.split_point^.nt;
      if nt <> NODE_PV then
        helper.smp_scout(nt, thread^.split_point^)
      else
        helper.smp_pv(thread^.split_point^);

      EnterCriticalSection(smp_lock);
      free_cpu_mask := free_cpu_mask or (1 shl threadID);
      LeaveCriticalSection(smp_lock);

      thread^.state := TS_IDLE;
      thread^.stop := stop;
    end;
    if (sp_wait <> nil) and (sp_wait^.cpus = 0) then begin
      assert(thread^.state = TS_IDLE);
      thread^.state := TS_WORKING;
      exit;
    end;
  end;
end;

function TEngine.init_book(name: PAnsiChar): boolean;
var
  b: TBoard;
  root_st: TStateInfo;
begin
  b.reset(@root_st);
  result := book_init(name, b);
end;

end.
