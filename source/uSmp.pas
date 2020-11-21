
unit uSmp;

interface

uses
{$ifdef unix}
  CThreads,
{$else}
  Windows,
{$endif}
  uBoard, uEval, uMove, uMovePick, uSearchDef, uScore, uSquare, uSystem;

{$include platform.inc}

const
  MAX_THREADS_PER_SP = 8;
  MAX_SPLIT_POINTS = 8;    // per thread
  MIN_SPLIT_DEPTH = 6;
  MSD_MIN = 4;
  MSD_MAX = 10;

type
  TLock = TRtlCriticalSection;

  TThreadState =
    ( TS_SLEEPING, TS_IDLE, TS_WORK_PENDING, TS_WORKING, TS_TERMINATED );

  PSplitPoint = ^TSplitPoint;
  TSplitPoint = record
    parent: PSplitPoint;
    board: PBoard;
    ply, depth: integer;
    nt: TNodeType;
    beta: TScore;
    master: integer;
    cpus: integer;
    slaves: array [0..MAX_THREADS - 1] of byte;
    lock: TLock;
    best_move, threat_move: TMove;
    alpha, best_score: TScore;
    movecnt: integer;
    mate_threat: boolean;
    mp: PMovePick;
    finished: boolean;
    search_stack: array [0..MAX_THREADS-1, 0..MAX_PLY] of TSearchStack;
  end;

  PThread = ^TThread;
  TThread = record
    split_point: PSplitPoint;
    sp_count: integer;
    state: TThreadState;
    stats: TSearchStats;
    hIdle: TEvent;
    evaluator: TEvaluator;
    stop: boolean;
  end;

  PThreadParam = ^TThreadParam;
  TThreadParam = record
    smp_mgr: pointer;
    threadID: integer;
  end;

  PSmpManager = ^TSmpManager;

  TInfoCallback =  procedure(iter: integer; score: TScore; bound: integer; pv: PMove);

  TSmpManager = object
    num_threads: integer;
    idle, stop, exit_flag: boolean;
    split_depth, split_depth_pv: integer;
    free_cpu_mask: integer;
    smp_lock: TLock;

    time_start: time_t;
    time_elapsed: time_t;
    last_output: time_t;

    time_normal: integer;
    time_failhi: integer;
    time_easy: integer;
    time_max: integer;

    iteration, multicnt: integer;
    info_callback: TInfoCallback;
    pondering, pondering_enabled: boolean;
    time_ctrl: TTimeCtrl;
    splits: int64;
    stats: TSearchStats;
    threads: array [0..MAX_THREADS-1] of TThread;
    split_points: array [0..MAX_THREADS-1, 0..MAX_SPLIT_POINTS-1] of TSplitPoint;

    procedure set_threads(n: integer);
    procedure free_evaluators; // RC
    procedure smp_init(const entry_point: TThreadFunc);
    procedure smp_destroy();
    procedure set_split_depth(sd: integer);
    function thread_should_stop(thread: PThread): boolean;
    function is_thread_available(master, slave: integer): boolean;
    function has_free_threads(master: integer): boolean;
    procedure wake_threads();
    procedure stop_threads();
    function sum_stats(): PSearchStats;
    function calc_time(): time_t;
  end;

implementation

function TSmpManager.calc_time(): time_t;
begin
  result := get_time() - time_start;
  time_elapsed := result;
end;

procedure TSmpManager.set_split_depth(sd: Integer);
begin
  sd := min(max(MSD_MIN, sd), MSD_MAX);
  split_depth := sd * PLYUNIT;
  split_depth_pv := (sd - 1) * PLYUNIT;
end;

procedure TSmpManager.set_threads(n: integer);
var
  i: integer;
begin
  n := max(min(n, MAX_THREADS), 1);
  if n <> num_threads then begin
    for i := 0 to MAX_THREADS - 1 do
      if i < n then
        threads[i].evaluator.cache_init()
      else
        threads[i].evaluator.cache_destroy();
    num_threads := n;
  end;
end;

procedure TSmpManager.free_evaluators; // RC
var
  i: integer;
begin
  for i := 0 to MAX_THREADS - 1 do
  begin
    threads[i].evaluator.cache_destroy();
  end;
end;

procedure TSmpManager.smp_init(const entry_point: TThreadFunc);
var
  i, j: integer;
  tp: TThreadParam;
  tid: TThreadID;
begin
  InitCriticalSection(smp_lock);
  exit_flag := false;
  for i := 0 to MAX_THREADS - 1 do begin
    for j := 0 to MAX_SPLIT_POINTS - 1 do begin
      split_points[i, j].parent := nil;
      InitCriticalSection(split_points[i, j].lock);
    end;
    with threads[i] do begin
      sp_count := 0;
      split_point := nil;
      hIdle.init();
    end;
    if i <> 0 then begin
      threads[i].state := TS_IDLE;
      tp.smp_mgr := @self;
      tp.threadID := i;
      start_thread(entry_point, @tp, tid);
      while threads[i].state <> TS_SLEEPING do ;
    end
    else
      threads[i].state := TS_WORKING;
  end;
end;

procedure TSmpManager.wake_threads();
var
  i: integer;
begin
  threads[0].stop := false;
  for i := 0 to MAX_THREADS - 1 do begin
    threads[i].stop := false;
    threads[i].hIdle.signal();
  end;
end;

procedure TSmpManager.stop_threads();
var
  i: integer;
begin
  stop := true;
  for i := 0 to MAX_THREADS - 1 do
    threads[i].stop := true;
end;

procedure TSmpManager.smp_destroy();
var
  i, j: integer;
begin
  exit_flag := true;
  idle := false;
  num_threads := MAX_THREADS;
  wake_threads();

  for i := 1 to MAX_THREADS - 1 do
    while threads[i].state <> TS_TERMINATED do ;

  for i := 0 to MAX_THREADS - 1 do
  begin
    for j := 0 to MAX_SPLIT_POINTS - 1 do
      DoneCriticalSection(split_points[i, j].lock);
    threads[i].hIdle.done();
  end;
  
  DoneCriticalSection(smp_lock);
end;

function TSmpManager.sum_stats(): PSearchStats;
var
  s: PSearchStats;
  i: integer;
begin
  move(threads[0].stats, stats, sizeof(stats));
  if num_threads > 1 then begin
    s := @stats;
    i := 1;
    repeat
      with threads[i].stats do begin
        inc(s^.nodes, nodes);
        inc(s^.tbhits, tbhits);
        s^.maxply := max(s^.maxply, maxply);
      end;
      inc(i);
    until i = num_threads;
  end;
  result := @stats;
end;

function TSmpManager.has_free_threads(master: integer): boolean;
var
  i: integer;
begin
  for i := 0 to num_threads - 1 do
    if is_thread_available(master, i) then begin
      result := true;
      exit;
    end;
  result := false;
end;

function TSmpManager.is_thread_available(master, slave: integer): boolean;
var
  sp_count: integer;
begin
  if (slave <> master) and (threads[slave].state = TS_IDLE) then begin
    result := true;
    if num_threads = 2 then exit;
    sp_count := threads[slave].sp_count;
    if (sp_count = 0) then exit;
    if (split_points[slave, sp_count-1].slaves[master] <> 0) then exit;
  end;
  result := false;
end;

function TSmpManager.thread_should_stop(thread: PThread): boolean;
var
  sp: PSplitPoint;
begin
  result := true;
  if stop then exit;

  sp := thread^.split_point;
  while sp <> nil do begin
    if sp^.finished then exit;
    sp := sp^.parent;
  end;

  result := false;
end;

end.
