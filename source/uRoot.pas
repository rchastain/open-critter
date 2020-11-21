
unit uRoot;

interface

uses
  uBoard, uMove, uSearch, uSearchBase, uSearchDef, uSquare, uScore;

{$include platform.inc}

type
  PRootMove = ^TRootMove;
  TRootMove = record
    move: TMove;
    score: TScore;
    node_count: uint64;
    total_nodes: uint64;
    tbhit: boolean;
    refutation: TMove;
    pv: array [0..MAX_PLY-1] of TMove;
    {$ifndef FPC}
      class operator LessThan(m1, m2: TRootMove): boolean;
    {$endif}
  end;

  TRootMoveList = object
    num_moves: integer;
    best_idx, idx: integer;
    best_score: TScore;
    moves: array of TRootMove;
    procedure init(var board: TBoard; move_list: PMoveList);
    procedure sort(count: integer);
    procedure bring_to_front(move: TMove);
  end;

  TRoot = object(TSearch)
    UCI_mode: boolean;
    iteration, multi_pv, multi_cnt: integer;
    prevply_move: TMove;
    prevply_score: TScore;
    root_changed, score_drop: boolean;
    move_on_ponderhit: boolean;
    allow_instant_reply: boolean;
    root_best, ponder_move: TMove;
    prev_iterations: integer;
    fh_move: PRootMove;
    fail_hi_cnt: integer;
    root_list: TRootMoveList;
    function think(const b: TBoard): TMove;
    function search_root(ss: PSS; depth: integer; alpha, beta: TScore): TScore;
    function probe_dtm(out score: TScore): boolean;

{$ifdef LOG_MOVES}
    procedure log();
    procedure log_clear();
{$endif}

  end;

{$ifdef FPC}
  operator < (m1, m2: TRootMove) LessThan : boolean;
{$endif}

implementation

uses
  uBitboard, uBook, uEngine, uEval, uGtb, uMovegen, uTranstab, uPiece,
  uSystem;

function TRoot.think(const b: TBoard): TMove;
var
  alpha, beta, score: TScore;
  hm_score, offset, awh, awl: TScore;
  time_ctrl: TTimeCtrl;
  use_tb, tb_extend: boolean;
  hm_depth, depth, new_depth: integer;
  move, hash_move: TMove;
  rm: PRootMove;
  h: PHashEntry;
  key32: cardinal;
  complete_iterations: integer;
  tbhits, i: integer;
  u: TStateInfo;
  search_stack: array [0..MAX_PLY-1] of TSearchStack;

begin

{$ifdef LOG_MOVES}
  log_clear();
{$endif}

  engine.time_start := get_time();
  engine.time_elapsed := 0;

  root_best := NOMOVE;
  ponder_move := NOMOVE;

  if (root_list.num_moves = 0) then begin
    while engine.pondering and not engine.stop do ;
    result := NOMOVE;
    exit;
  end;

  with TBook do begin
    inc(out_of_book);
    if book_available and use_book and (out_of_book < 6) then begin
      move := book.probe(b);
      if move <> NOMOVE then begin
        out_of_book := 0;
        root_best := move;
        ponder_move := NOMOVE;
        while engine.pondering and not engine.stop do ;
        result := root_best;
        exit;
      end
    end;
  end;

  time_ctrl := engine.time_ctrl;
  zero_mem(@search_stack[0], sizeof(search_stack));
  board.clone_from(b, @root_state);
  board.ply := 0;
  search_stack[0].curr_move := b.st^.lastmove;
  move_on_ponderhit := false;

  if (book.out_of_book < 2) and (time_ctrl.mode <> TM_FIXED) then
    with engine do
      if time_max > 2 * time_failhi then begin
        allow_instant_reply := false;
        time_normal := time_normal * 2;
        time_failhi := time_failhi * 2;
      end;

  if (time_ctrl.max_nodes <> 0) and (time_ctrl.max_nodes < 65536) then
    time_check_mask := nodes_to_mask(time_ctrl.max_nodes)
  else
    with engine do
      if (time_max <> 0) and (time_max < 2048) then
        time_check_mask := time_to_mask(time_max)
      else
        time_check_mask := 65535;

  draw_score[WHITE] := 0;
  draw_score[BLACK] := 0;
  root_stm := board.us;
  root_list.best_idx := 0;
  root_best := root_list.moves[0].move;

  use_tb := gtb_available and (use_gtb and TB_AT_ROOT <> 0);
  tbhits := 0;
  for i := 0 to root_list.num_moves - 1 do
    with root_list.moves[i] do begin
      board.make_move(move, u);
      thread^.evaluator.evaluate(board, 0, 0, 0);
      if use_tb and probe_dtm(score) then begin
        inc(tbhits);
        tbhit := true;
      end
      else
        score := -board.st^.eval;
      board.undo_move(move);
    end;
  root_list.sort(root_list.num_moves);
  inc(engine.stats.tbhits, tbhits);

  with root_list.moves[0] do
    if tbhit and ((score >= RESIGN) or (tbhits = root_list.num_moves)) then begin
      if score >= RESIGN then
        tb_extend := (engine.time_normal > 500) or (engine.time_normal = 0)
      else
        tb_extend := false;
      tt.extract_pv(board, @pv, move, tb_extend);
      engine.calc_time();
      engine.info_callback(1, score, BOUND_EXACT, @pv);
      root_best := move;
      result := move;
      while engine.pondering and not engine.stop do ;
      exit;
    end;

  tt.new_search();
  engine.wake_threads();

  depth := 0;
  hm_depth := 0;
  move := NOMOVE;
  hash_move := NOMOVE;
  h := @tt.table^[board.st^.hashkey and tt.hash_mask];
  key32 := cardinal(board.st^.hashkey shr 32);
  for i := 0 to HASH_SLOTS - 1 do begin
    if h^.key32 = key32 then begin
      if (h^.best_move <> NOMOVE) and (h^.depth_lower > hm_depth) then begin
        hash_move := h^.best_move;
        hm_depth := h^.depth_lower;
      end;
      if (h^.flags and BOUND_EXACT <> 0) and (h^.depth_lower > depth) then begin
        depth := h^.depth_lower;
        move := h^.best_move;
        hm_score := score_from_tt(h^.score_lower, board.ply);
      end;
    end;
    inc(h);
  end;

  if not board.hm_is_ok(hash_move) then
    hash_move := NOMOVE;

  if hash_move <> NOMOVE then
    root_list.bring_to_front(hash_move);

  if allow_instant_reply and (time_ctrl.mode <> TM_FIXED) then
    if (prev_iterations >= 12) and (depth >= (prev_iterations - 3) * ONE_PLY) then
      if (move <> NOMOVE) and (move = hash_move) then
      begin
        new_depth := (prev_iterations - 6) * ONE_PLY;
        offset := hm_score - $80;
        score := scout_search(@search_stack[1], NODE_ALL, new_depth, offset, move);
        if not engine.stop and (score < offset) then begin
          rm := @root_list.moves[0];
          allow_instant_reply := false;
          tt.extract_pv(board, @rm^.pv[0], move, false);
          thread^.stats.maxply := depth div ONE_PLY;
          engine.calc_time();
          engine.info_callback(depth div ONE_PLY, hm_score, BOUND_EXACT, @rm^.pv[0]);
          ponder_move := rm^.pv[1];
          root_best := rm^.move;
          if not engine.pondering then begin
            result := root_best;
            exit;
          end;
          move_on_ponderhit := true;
        end;
      end;

  complete_iterations := 0;
  iteration := 1;
  alpha := -SCORE_INF;
  beta  := +SCORE_INF;
  awh := 0;
  awl := 0;

  while true do begin

{$ifdef LOG_MOVES}
    log();
{$endif}

    for i := 0 to root_list.num_moves-1 do
      root_list.moves[i].node_count := 0;

    root_list.best_score := alpha;
    root_list.idx := 0;
    fail_hi_cnt := 0;
    engine.multicnt := 1;
    root_changed := false;
    fh_move := nil;

    while true do begin

      if alpha < -$2000 then alpha := -SCORE_INF;
      if beta > $2000 then beta := SCORE_INF;

      score := search_root(@search_stack[1], iteration * PLYUNIT, alpha, beta);
      if engine.stop then break;
      engine.calc_time();

      if score <= alpha then begin
        engine.info_callback(iteration, alpha, UPPER_BOUND, @root_list.moves[0].pv);
        if (awl < $500) then begin
          alpha := score - awl;
          inc(awl, awl div 2);
        end
        else
          alpha := -SCORE_INF;
        score_drop := true;
        root_list.idx := 0;
        root_list.best_score := alpha;
        continue;
      end;

      if score >= beta then begin
        engine.info_callback(iteration, beta, LOWER_BOUND,
          @root_list.moves[root_list.idx].pv);
        if (awh < $500) then begin
          beta := score + awh;
          inc(awh, awh div 2);
        end
        else
          beta := SCORE_INF;
        if root_list.idx <> 0 then
          score_drop := true;
        continue;
      end;

      fail_hi_cnt := 0;
      fh_move := nil;
      break;
    end;

    if engine.stop then break;

    if (time_ctrl.max_depth <> 0) and (iteration >= time_ctrl.max_depth) then
      break;

    root_list.sort(root_list.num_moves);
    root_list.bring_to_front(root_best);
    root_list.best_idx := 0;

    if iteration >= 12 then begin

      if iteration >= MAX_ITER - 8 then break;

      if (iteration >= prev_iterations - 1) and (time_ctrl.mode <> TM_FIXED) then
        if engine.time_elapsed > engine.time_easy then begin
          depth := (iteration - 6) * ONE_PLY;
          offset := root_list.best_score - $180;
          score := scout_search(@search_stack[0], depth, NODE_ALL, offset, root_best);
          if engine.stop then break;
          if score < offset then
          begin
            if engine.pondering then
              move_on_ponderhit := true
            else begin
              allow_instant_reply := false;
              ponder_move := root_list.moves[0].refutation;
              result := root_best;
              exit;
            end
          end;
        end;
    end;

    if not engine.pondering then
      if (time_ctrl.mode <> TM_FIXED) then begin
        if score_drop or root_changed then i := 85 else i := 65;
        if engine.time_elapsed > (engine.time_normal * i) div 100 then break;
      end;

    complete_iterations := iteration;
    inc(iteration);

    if UCI_mode and (engine.time_elapsed > 1000) then
      printf('info depth %d' + LineEnding, iteration);

    score := root_list.best_score;
    prevply_score := score;
    prevply_move := root_best;

    if (multi_pv = 1) and (iteration > 7) then begin
      awl := max(AW_WIDTH, abs(score) shr 4);
      awh := awl;
      alpha := score - awl;
      beta := score + awh;
    end
    else begin
      beta := SCORE_INF;
      alpha := -SCORE_INF;
    end;

  end;

  with engine do begin

    while pondering and not stop do ;

    stop := false;
    calc_time();

    if pondering_enabled and (command <> EC_PONDER) then
      allow_instant_reply := false
    else
      if not move_on_ponderhit then begin
        allow_instant_reply := true;
        prev_iterations := complete_iterations;
      end;
  end;

  if (fh_move <> nil) and (fail_hi_cnt >= 2) then begin
    root_best := fh_move^.move;
    ponder_move := fh_move^.refutation;
  end;

  result := root_best;
end;

procedure TRootMoveList.init(var board: TBoard; move_list: PMoveList);
var
  i: integer;
  ml: TMoveList;
begin
  num_moves := 0;
  if move_list <> nil then begin
    while (move_list^[num_moves].move <> NOMOVE) do begin
      ml[num_moves].move := move_list^[num_moves].move;
      inc(num_moves);
    end;
    ml[num_moves].move := NOMOVE;
  end
    else
      num_moves := gen_legal_moves(board, ml);

  SetLength(moves, num_moves);
  for i := 0 to num_moves-1 do
    with moves[i] do begin
      move := ml[i].move;
      refutation := NOMOVE;
      score := 0;
      node_count := 0;
      tbhit := false;
      pv[0] := NOMOVE;
    end;
end;

procedure TRootMoveList.bring_to_front(move: TMove);
var
  i, j: integer;
  tmp: TRootMove;
begin
  for i := 0 to num_moves - 1 do
    if moves[i].move = move then begin
      tmp := moves[i];
      for j := i - 1 downto 0 do moves[j+1] := moves[j];
      moves[0] := tmp;
      exit;
    end;
end;

{$ifndef FPC}
class operator TRootMove.LessThan(m1, m2: TRootMove): boolean;
{$else}
operator < (m1, m2: TRootMove) LessThan : boolean;
{$endif}
begin
  if m1.score <> m2.score then
    result := m1.score < m2.score
  else
    result := m1.node_count < m2.node_count;
end;

procedure TRootMoveList.sort(count: integer);
var
  i, j: integer;
  tmp: TRootMove;
begin
  for i := 0 to count - 1 do begin
    tmp := moves[i];
    j := i - 1;
    while (j >= 0) and (moves[j] < tmp) do begin
      moves[j+1] := moves[j];
      dec(j);
    end;
    moves[j+1] := tmp;
  end;
end;

function TRoot.probe_dtm(out score: TScore): boolean;
var
  wdl, dtm: integer;
begin
  result := false;
  if (popcnt(board.bb.occupied) > gtb_num_pieces) then
    exit;
  if gtb_probe_dtm(board, PROBE_HARD, wdl, dtm) then begin
    case wdl of
      ord(tb_DRAW): score := SCORE_ZERO;
      ord(tb_WMATE): score := (mated_in(dtm) + 1) * sign[board.us];
      ord(tb_BMATE): score := (mate_in(dtm) - 1) * sign[board.us];
    else
      exit;
    end;
    result := true;
  end;
end;

function TRoot.search_root(ss: PSS; depth: integer; alpha, beta: TScore): TScore;
var
  start_idx, i, iter: integer;
  node_count, nodes: uint64;
  old_alpha: TScore;
  new_depth: integer;
  enough_time: boolean;
  tb_extend: boolean;
  speed: cardinal;
  move: TMove;
  stats: PSearchStats;
  rm: PRootMove;
  c: TMoveString;
  u: TStateInfo;

begin
  stats := @engine.stats;
  start_idx := root_list.idx;
  init_node(ss);
  ss^.in_chk := board.st^.checkersBB <> 0;
  old_alpha := alpha;
  while root_list.idx < root_list.num_moves do begin
    rm := @root_list.moves[root_list.idx];
    move := rm^.move;
    ss^.curr_move := move;
    ss[1].curr_move := NOMOVE;
    if UCI_mode and (engine.time_elapsed > 1000) then begin
      speed := stats^.nodes * 1000 div (engine.calc_time() + 1);
      printf('info currmove %s currmovenumber %d nodes %' + fmt64 + 'u' +
        ' nps %d hashfull %d tbhits %' + fmt64 + 'u' + LineEnding,
        move_to_string(move, c), root_list.idx + 1, stats^.nodes,
        speed, tt.hash_full(), stats^.tbhits);
    end;

    node_count := stats^.nodes;

    {if (iteration = 8) and (move = TMove($3765)) then
      printf('.');}

    if rm^.tbhit then
      result := rm^.score
    else begin
      board.make_move(move, u);
      thread^.evaluator.evaluate(board, 0, 0, 0);
      if u.flags and FLAG_EXACT = 0 then begin
        new_depth := depth - PLYUNIT + extend(ss, NODE_PV, NOMOVE);
        if (root_list.idx < multi_pv) or (iteration = 1) or (root_list.idx = start_idx) then
        begin
          if multi_pv > 1 then
            alpha := -SCORE_INF;
          if new_depth >= ONE_PLY then
            result := -pv_search(@ss[1], new_depth, -beta, -alpha)
          else
            if u.checkersBB <> 0 then
              result := -qevasions_pv(@ss[1], 1, -beta, -alpha)
            else
              result := -quiesce_pv(@ss[1], 1, -beta, -alpha);
        end
        else begin
          result := -scout_search(@ss[1], NODE_CUT, new_depth, -alpha);
          if (result > alpha) and not engine.stop then begin
            root_changed := true;
            if iteration >= prev_iterations - 3 then
              move_on_ponderhit := false;
            result := -pv_search(@ss[1], new_depth, -alpha-1, -alpha);
            if (result > alpha) and not engine.stop then
              result := -pv_search(@ss[1], new_depth, -beta, -alpha);
          end;
        end;
      end
      else
        result := -u.eval;
      board.undo_move(move);

      if engine.stop then exit;

      engine.sum_stats();
      nodes := stats^.nodes - node_count;

      if result <= alpha then
        rm^.score := old_alpha
      else
        rm^.score := result;

      inc(rm^.node_count, nodes);
      inc(rm^.total_nodes, nodes);
    end;

    if result <= alpha then begin
      if ss[1].curr_move <> NOMOVE then
        rm^.refutation := ss[1].curr_move;
      if root_list.idx = 0 then begin
        score_drop := true;
        fh_move := nil;
        fail_hi_cnt := 0;
        if iteration >= prev_iterations - 3 then
          move_on_ponderhit := false;
      end;
    end;

    if result > alpha then begin
      alpha := result;
      with engine do
        enough_time := (time_ctrl.time[board.us] > 1000) or (time_normal = 0);
      tb_extend := rm^.tbhit and (abs(result) >= RESIGN) and enough_time;
      tt.extract_pv(board, @rm^.pv, move, tb_extend);
      if rm^.pv[1] <> NOMOVE then
        rm^.refutation := rm^.pv[1];

      if result >= beta then begin
        if fh_move <> rm then fail_hi_cnt := 0;
        inc(fail_hi_cnt);
        fh_move := rm;
        if fail_hi_cnt < 2 then begin
          if move <> root_best then
            root_changed := true;
        end
        else
          score_drop := result < prevply_score - 50;
        exit;
      end;

      if result < prevply_score - 50 then begin
        score_drop := true;
        if iteration >= prev_iterations - 3 then
          move_on_ponderhit := false;
      end;

      if result > root_list.best_score then begin
        root_list.best_idx := root_list.idx;
        root_list.best_score := result;
        root_best := move;
        ponder_move := rm^.refutation;
      end;

      fh_move := nil;
      fail_hi_cnt := 0;

      if iteration > 1 then begin
        engine.calc_time();
        if multi_pv = 1 then begin
          multi_cnt := 1;
          engine.info_callback(iteration, result, BOUND_EXACT, @rm^.pv);
        end
        else begin
          root_list.sort(root_list.idx + 1);
          for i := 0 to min(root_list.num_moves, multi_pv) do begin
            multi_cnt := i + 1;
            iter := iteration;
            if i > root_list.idx then dec(iter);
            with root_list.moves[i] do
              engine.info_callback(iter, score, BOUND_EXACT, @pv);
          end;
          alpha := root_list.moves[min(root_list.idx, multi_pv - 1)].score;
        end;
      end;
    end;
    inc(root_list.idx);
  end;

  result := root_list.best_score;
end;

{$ifdef LOG_MOVES}

procedure TRoot.log_clear();
var f: TextFile;
begin
  AssignFile(f, 'makemove.log');
  {$I-}
  Rewrite(f);
  {$I+}
  CloseFile(f);
end;

procedure TRoot.log();
var
  i: integer;
  rm: PRootMove;
  f: TextFile;
  c: TMoveString;
begin
  AssignFile(f, 'makemove.log');
  {$I-}
  Append(f);
  {$I+}
  if IOResult <> 0 then Rewrite(f);
  writeln(f, LineEnding + 'iteration ', iteration);
  for i := 0 to root_list.num_moves-1 do begin
    rm := @root_list.moves[i];
    writeln(f, 'move: ', move_to_string(rm^.move, c), #9'score: ',
      rm^.score, #9'nodes: ', rm^.node_count);
  end;
  writeln(f);
  CloseFile(f);
end;

{$endif}

end.
