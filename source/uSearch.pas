
unit uSearch;

interface

uses
  uBitboard, uBoard, uHistory, uMove, uMovePick, uQuiesce, uScore, uSearchBase,
  uSearchDef, uSmp, uSquare;

{$include platform.inc}

type
  TSearch = object(TQSearch)
    function selective_search(ss: PSS; depth: integer; scout: TScore): TScore;

    function scout_search(ss: PSS; nt: TNodeType; depth: integer; scout: TScore;
      ex_move: TMove = NOMOVE): TScore;

    function pv_search(ss: PSS; depth: integer; alpha, beta: TScore): TScore;

    procedure smp_scout(nt: TNodeType; var sp: TSplitPoint);
    procedure smp_pv(var sp: TSplitPoint);

    function split(
      ss: PSS;
      depth: integer;
      alpha, beta: TScore;
      var best_score: TScore;
      var best_move: TMove;
      var movecnt: integer;
      var mp: TMovePick;
      node_type: TNodeType): boolean;
    end;

implementation

uses
{$ifndef unix}
  Windows,
{$endif}
  uEngine, uPiece, uMovegen, uSee, uTranstab, uZobrist;

function TSearch.selective_search(ss: PSS; depth: integer; scout: TScore): TScore;
var
  move, hash_move: TMove;
  eval, diff, best_score: TScore;
  move_depth, movecnt, skipped, i: integer;
  new_depth, threat, flags: integer;
  us: TSide;
  mask: TBitboard;
  exact: boolean;
  use_history: boolean;
  h: PHashEntry;
  key32: cardinal;
  u: TStateInfo;
  mp: TMovePick;

begin
  us := board.us;
  if (depth < ONE_PLY) then with board.st^ do begin
    if (checkersBB = 0) then begin
      if ((threat[us] <> 0)
        and (eval >= scout + tempo)
        and (qs_delay = 0)
        and (not is_quiet(lastmove) or (threat[us] > ss[-1].threat[us]))) then
      begin
        qs_delay := 1;
        depth := ONE_PLY;
      end
      else begin
        result := quiesce(ss, 0, scout);
        exit;
      end;
    end;
  end;

  init_node(ss);

  if scout > mate_in(board.ply) then begin
    result := scout-1;
    exit;
  end;
  if scout <= mated_in(board.ply) then begin
    result := scout;
    exit;
  end;

  result := SCORE_ZERO;
  if should_stop() then exit;

  with thread^.stats do begin
    if board.ply > maxply then maxply := board.ply;
    inc(nodes);
    if (threadID = 0) and (nodes and time_check_mask = 0) then
      if time_check() then exit;
  end;

  ss^.in_chk := board.st^.checkersBB <> 0;

  move_depth := 0;
  hash_move := NOMOVE;
  h := @tt.table^[board.st^.hashkey and tt.hash_mask];
  key32 := cardinal(board.st^.hashkey shr 32);
  for i := 0 to HASH_SLOTS - 1 do begin
    if h^.key32 = key32 then begin

      if (h^.flags and HASH_NOMOVES <> 0) then begin
        if ss^.in_chk then
          result := mated_in(board.ply)
        else
          result := -draw_score[us];
        exit;
      end;

      if (h^.best_move <> NOMOVE) and (h^.depth_lower > move_depth) then begin
        hash_move := h^.best_move;
        move_depth := h^.depth_lower;
      end;

      if (h^.depth_lower <> 0) and (h^.depth_lower >= depth) then begin
        result := score_from_tt(h^.score_lower, board.ply);
        if result >= scout then begin
          ss^.curr_move := h^.best_move;
          exit;
        end;
      end;

      if h^.depth_upper <> 0 then begin
        result := score_from_tt(h^.score_upper, board.ply);
        if result < scout then begin
          if h^.depth_upper >= depth then exit;
          board.st^.flags := board.st^.flags and not NULL_MASK;
        end;
      end;
    end;
    inc(h);
  end;

  eval := board.st^.eval;

  if not ss^.in_chk then begin

    if (eval + EG_VQUEEN < scout) then begin
      result := scout-1;
      exit;
    end;

    if (eval >= scout + 26 * (depth + 7)) and (board.st^.threat[us] = 0) then
    begin
      result := eval;
      exit;
    end;

    if (eval >= scout) and (board.st^.flags and (cardinal(us)+1) <> 0) then begin
      board.make_null(u);
      exact := u.flags and FLAG_EXACT <> 0;
      if not exact then
        result := -quiesce(@ss[1], 0, -scout+1)
      else
        result := -board.st^.eval;
      board.undo_null();
      if thread^.stop then exit;
      if result >= scout then begin
        if not exact then
          tt.store_lower(board.st^.hashkey, depth, result, board.ply, hash_move);
        exit;
      end
      else
        if ss[-1].reduction <> 0 then
          if (result < scout - $80) or (result < -RESIGN) then begin
            result := scout-1;
            exit;
          end;
    end;

    best_score := min(eval, scout-1);
    mp.init(board, ss, S1_HASH, hash_move);
    use_history := (eval + $80 > scout);
    diff := scout - eval - 21*depth;
    if diff > $80 then with board do begin
      mp.phase := S2_HASH;
      mp.target := bb.pieces[they];
      with delta_margins[st^.phase.total] do begin
        if diff > pawn_margin then begin
          mask := bb.pawns[they] and NonRank7[they];
          mp.target := mp.target xor mask;
          if mask and st^.attacks[us] <> 0 then
            best_score := eval + pawn_inc;
          if diff > minor_margin then begin
            mask := bb.knights[they] or bb.bishops[they];
            mp.target := mp.target xor mask;
            if mask and st^.attacks[us] <> 0 then
              best_score := eval + minor_inc;
            if diff > rook_margin then begin
              mp.target := mp.target xor bb.rooks[they];
              if bb.rooks[they] and st^.attacks[us] <> 0 then
               best_score := eval + rook_inc;
            end;
          end;
        end;
      end;
    end;
  end
  else begin
    best_score := mated_in(board.ply);
    mp.init(board, ss, S3_EVASIONS_HASH, hash_move);
    use_history := false;
  end;

  movecnt := 0;
  skipped := 0;

  while true do begin
    move := mp.get_next();
    if move = NOMOVE then break;
    ss^.curr_move := move;
    inc(movecnt);

    if (scout > draw_score[board.they]) and (board.st^.rule50 >= 2) then
      if is_quiet(move) and (ss[-2].curr_move = reversed(move)) then begin
        if ss^.in_chk then
          best_score := max(draw_score[board.they], best_score);
        continue;
      end;

    if not ss^.in_chk then begin
      if (movecnt > depth) and (mp.phase = S1_QUIET) then
        if (board.count.np[us] <> 0) and not board.is_check(move) then
          if (eval + hist.gain(move) + 5*depth < scout + $61 + 6*movecnt) then
            continue;

      if is_quiet(move) or (mp.phase = S2_CHECKS) or (depth <= 5) then
        if (move <> hash_move) and non_king_move(move) then
          if (not board.is_dc(move)) and bad_see(board, move) then
            continue;
    end
    else
      if is_quiet(move) and (move <> hash_move) and (scout > -RESIGN) then
        if non_king_move(move) and bad_see(board, move) then
        begin
          inc(skipped);
          continue;
        end;

    board.make_move(move, u);
    thread^.evaluator.evaluate(board, scout, scout, LAZY_QS);
    flags := u.flags;
    if flags and FLAG_EXACT = 0 then begin
      if u.checkersBB <> 0 then begin
        new_depth := depth - HALF_PLY + integer(ss^.in_chk and (u.phase.total >= 18));
        result := -selective_search(@ss[1], new_depth, -scout+1);
      end
      else begin
        if not ss^.in_chk and (movecnt > depth) then
          if (-u.eval + 5 * depth < scout + 2*movecnt) then begin
            threat := u.threat[board.us];
            if (threat = 0) or ((threat <= ss^.threat[board.us]) and is_quiet(move)) then
            begin
              board.undo_move(move);
              continue;
            end;
          end;
        new_depth := depth - ONE_PLY + integer(ss^.in_chk and (u.phase.total >= 18));
        result := -selective_search(@ss[1], new_depth, -scout+1);
      end;
    end
    else
      result := -u.eval;
    board.undo_move(move);

    if thread^.stop then exit;

    if result > best_score then begin
      best_score := result;
      if result >= scout then begin
        if is_quiet(move) and not ss^.in_chk then
          if (flags and FLAG_REPETITION = 0) then begin
            hist.record_good_move(move, depth);
            update_killers(ss, move);
          end;
        tt.store_lower(board.st^.hashkey, max(depth, 1), result, board.ply, move);
        exit;
      end;
    end;

    if use_history and is_quiet(move) and (flags and FLAG_REPETITION = 0) then
      hist.record_bad_move(move, depth);
  end;

  ss^.curr_move := NOMOVE;
  if movecnt = 0 then begin
    if ss^.in_chk then begin
      result := mated_in(board.ply);
      exit;
    end
    else
      if mp.phase < S2_HASH then begin
        result := SCORE_ZERO;
        exit;
      end;
  end
  else
    if (skipped <> 0) and (best_score < -RESIGN) then
      best_score := scout-1;

  tt.store_upper(board.st^.hashkey, max(depth, 1), best_score, board.ply);
  result := best_score;
end;

function TSearch.scout_search(ss: PSS; nt: TNodeType; depth: integer;
  scout: TScore; ex_move: TMove = NOMOVE): TScore;
var
  eval, best_score: TScore;
  fmargin, emargin, offset: TScore;
  move, hash_move, best_move: TMove;
  move_depth, movecnt, flags, i: integer;
  new_depth, ext, singular: integer;
  use_futility, use_history: boolean;
  mask: TBitboard;
  hash_key: TKey;
  h: PHashEntry;
  key32: cardinal;
  u: TStateInfo;
  mp: TMovePick;

begin
  if (ex_move = NOMOVE) then begin
    if depth < SELECTIVE_DEPTH then begin
      result := selective_search(ss, depth, scout);
      exit;
    end;
    if board.ply >= 200 then begin
      result := selective_search(ss, 6, scout);
      exit;
    end;
  end;

  init_node(ss);

  if scout > mate_in(board.ply) then begin
    result := scout-1;
    exit;
  end;
  if scout <= mated_in(board.ply) then begin
    result := scout;
    exit;
  end;

  result := SCORE_ZERO;
  if should_stop() then exit;

  with thread^.stats do begin
    if board.ply > maxply then maxply := board.ply;
    inc(nodes);
    if (threadID = 0) and (nodes and time_check_mask = 0) then
      if time_check() then exit;
  end;

  with board do begin
    ss^.in_chk := st^.checkersBB <> 0;
    if (ex_move = NOMOVE) then
      hash_key := st^.hashkey
    else
      hash_key := st^.hashkey xor exclusion_key(us, ex_move);
  end;

  move_depth := 0;
  hash_move := NOMOVE;
  h := @tt.table^[hash_key and tt.hash_mask];
  key32 := cardinal(hash_key shr 32);
  for i := 0 to HASH_SLOTS - 1 do begin
    if h^.key32 = key32 then begin

      if h^.flags and HASH_NOMOVES <> 0 then begin
        if ss^.in_chk then
          result := mated_in(board.ply)
        else
          result := -draw_score[board.us];
      end;

      if (h^.best_move <> NOMOVE) and (h^.depth_lower >= move_depth) then begin
        hash_move := h^.best_move;
        move_depth := h^.depth_lower;
      end;

      if h^.depth_lower >= depth then begin
        result := score_from_tt(h^.score_lower, board.ply);
        if result >= scout then
          if (nt <> NODE_CUT) or (h^.flags and HASH_NODE_ALL = 0) then
            if (h^.best_move <> NOMOVE) or board.allow_null() then begin
              h^.age := tt.generation;
              ss^.curr_move := h^.best_move;
              exit;
            end;
      end;

      if h^.depth_upper <> 0 then begin
        result := score_from_tt(h^.score_upper, board.ply);
        if result < scout then begin
          board.st^.flags := board.st^.flags and not NULL_MASK;
          if h^.depth_upper >= depth then
            if (nt <> NODE_ALL) or (h^.flags and HASH_NODE_CUT = 0) then begin
              h^.age := tt.generation;
              exit;
            end;
        end;
      end;

    end;
    inc(h);
  end;

  if (depth >= EGTB_DEPTH) and (board.st^.flags and FLAG_PROBE_TB <> 0) then
    if egtb_probe(result, scout, scout) then exit;

  eval := board.st^.eval;
  movecnt := 0;

  if not ss^.in_chk then begin
    if (eval >= scout) and (board.st^.flags and (cardinal(board.us)+1) <> 0) then begin
      if not zugzwang_danger(board) then begin
        board.make_null(u);
        flags := u.flags;
        if flags and FLAG_EXACT = 0 then begin
          result := -quiesce(@ss[1], 0, -scout+1);
          if (result >= scout) and not thread^.stop then begin
            new_depth := get_null_depth(result - scout, depth);
            if new_depth >= ONE_PLY then
              result := -scout_search(@ss[1], nt xor 1, new_depth, -scout+1);
          end;
        end;
        board.undo_null();
        if thread^.stop then exit;
        if (result >= scout) then begin
          if ((ex_move = NOMOVE) or (depth >= 9*ONE_PLY)) then begin
            new_depth := depth - 5*ONE_PLY;
            board.st^.flags := board.st^.flags and not NULL_MASK;
            result := scout_search(ss, nt, new_depth, scout, ex_move);
            board.st^.flags := board.st^.flags or NULL_MASK;
            if thread^.stop then exit;
          end;
          if result >= scout then begin
            if (hash_move = NOMOVE) and (flags and FLAG_REPETITION = 0) then
              if nt = NODE_CUT then
                tt.store_lower(hash_key, depth, result, board.ply, NOMOVE)
              else
                tt.store_lower_all(hash_key, depth, result, board.ply, NOMOVE);
            exit;
          end;
        end
        else begin
          if (result < scout-$80) or (result < -RESIGN) then
            if (ss[-1].reduction <> 0) and (ex_move = NOMOVE) then begin
              result := scout-1;
              exit;
            end;
        end;
      end
      else
        board.st^.flags := board.st^.flags and not NULL_MASK;
    end;

    if (nt = NODE_CUT) and (hash_move = NOMOVE) and (depth >= 3*PLYUNIT) then begin
      result := scout_search(ss, NODE_CUT, depth-2*PLYUNIT, scout);
      if thread^.stop then exit;
      if result >= scout then
        hash_move := ss^.curr_move;
    end;

    use_futility := depth <= FUTILITY_DEPTH;
    use_history := eval + $80 > scout;
    mp.init(board, ss, S1_HASH, hash_move);
    if (depth < 10*ONE_PLY) and (eval + $80 * (depth - 5 + nt) < scout) then begin
      inc(movecnt);
      mp.phase := S2_HASH;
      mp.target := board.bb.pieces[board.they];
      if eval + $80 * (depth - 2) < scout then begin
        mask := board.bb.pawns[board.they] and NonRank7[board.they];
        mp.target := mp.target xor mask;
      end;
    end;
  end
  else begin
    use_futility := false;
    use_history := false;
    mp.init(board, ss, S3_EVASIONS_HASH, hash_move);
  end;

  singular := 0;
  best_score := mated_in(board.ply);
  best_move := NOMOVE;

  while true do begin
    move := mp.get_next();
    if move = NOMOVE then break;
    if move = ex_move then continue;

    inc(movecnt);

    if (scout > draw_score[board.they]) and is_quiet(move) then
      if (board.st^.rule50 >= 2) and (ss[-2].curr_move = reversed(move)) then begin
        if ss^.in_chk then
          best_score := max(draw_score[board.they], best_score);
        continue;
      end;

    if (nt = NODE_CUT) and (move = hash_move) and (depth >= 8*ONE_PLY) then
      if (abs(scout) < RESIGN) and (board.st^.phase.total <> 0) then begin
        offset := scout - (depth*2 + depth shr 1);
        new_depth := depth - min(6*PLYUNIT, depth shr 1);
        result := scout_search(ss, NODE_ALL, new_depth, offset, move);
        if result < offset then begin
          singular := 1;
          if (board.ply * 4 <= depth) then singular := 2;
          offset := scout - depth*5;
          result := scout_search(ss, NODE_ALL, new_depth, offset, move);
          if result < offset then begin
            inc(singular);
            if (board.ply * 8 <= depth) then inc(singular);
          end;
        end;
      end;

    ss^.curr_move := move;

    if (mp.phase = S1_QUIET) and use_futility then
      if not board.is_check(move) then begin
        if (movecnt > 6) and is_quiet(move) then begin
          fmargin := (3 + 10*nt) shl (depth - 6);
          if (eval + hist.gain(move) + fmargin < scout + $64 + 5*movecnt) then
            continue;
        end;
        if nt = NODE_ALL then
          if (5 shl (depth - 6) + eval - $140 < scout) then
            if non_king_move(move) and bad_see(board, move) then
              continue;
      end;

    if (mp.phase = S2_CHECKS) and non_king_move(move) then
      if not board.is_dc(move) and bad_see(board, move) then
        continue;

    board.make_move(move, u);
    thread^.evaluator.evaluate(board, scout, scout, LAZY_NONPV);
    flags := u.flags;
    if flags and FLAG_EXACT = 0 then begin

      if use_futility and is_quiet(move) and (movecnt > 3 * (nt + 1)) then
        if (u.checkersBB = 0) then
          if (u.threat[board.us] <= ss^.threat[board.us]) then begin
            emargin := 5 shl (depth - 6);
            if -u.eval + emargin < scout + 3*movecnt - 42 then begin
              board.undo_move(move);
              continue;
            end;
          end;

      ext := extend(ss, nt, hash_move);
      if ext < singular then ext := singular;
      singular := 0;
      result := scout;

      if u.checkersBB = 0 then begin
        ss^.reduction := reduce(nt, ss^.in_chk, ext, depth, movecnt, mp.phase);
        if ss^.reduction <> 0 then begin
          new_depth := depth - ONE_PLY + ext - ss^.reduction;
          result := -scout_search(@ss[1], nt xor 1, new_depth, -scout+1);
          ss^.reduction := 0;
        end;
      end;
      if (result >= scout) and not thread^.stop then begin
        new_depth := depth - ONE_PLY + ext;
        result := -scout_search(@ss[1], nt xor 1, new_depth, -scout+1);
      end;
    end
    else
      result := -u.eval;

    board.undo_move(move);

    if thread^.stop then exit;

    if result > best_score then begin
      best_score := result;
      if result >= scout then begin
        best_move := move;
        if is_quiet(move) and not ss^.in_chk then
          if (flags and FLAG_REPETITION = 0) then begin
            hist.record_good_move(move, depth);
            update_killers(ss, move);
          end;
        break;
      end;
    end;

    if use_history and is_quiet(move) and (flags and FLAG_REPETITION = 0) then
      hist.record_bad_move(move, depth);

    if (engine.free_cpu_mask <> 0) and
       (depth >= engine.split_depth) and
       (not thread^.stop) and
       (engine.has_free_threads(threadID)) and
       split(ss, depth, 0, scout, best_score, best_move, movecnt, mp, nt) then break;
  end;

  ss^.curr_move := best_move;

  if (movecnt = 0) and (ex_move = NOMOVE) then begin
    if ss^.in_chk then
      result := mated_in(board.ply)
    else
      result := -draw_score[board.us];
    exit;
  end;

  result := best_score;

  if best_score >= scout then begin
    if nt = NODE_CUT then
      tt.store_lower(hash_key, depth, best_score, board.ply, best_move)
    else
      tt.store_lower_all(hash_key, depth, best_score, board.ply, best_move);
    exit;
  end;

  if not ss^.in_chk then
    result := scout-1;

  if nt = NODE_ALL then
    tt.store_upper(hash_key, depth, result, board.ply)
  else
    tt.store_upper_cut(hash_key, depth, result, board.ply);
  exit;
end;

function TSearch.pv_search(ss: PSS; depth: integer; alpha, beta: TScore): TScore;
var
  score, best_score, hm_score: TScore;
  offset, iid_alpha, iid_beta: TScore;
  move, best_move, hash_move: TMove;
  hm_depth, movecnt, flags, i: integer;
  new_depth, lmr_depth, ext, singular: integer;
  use_history: boolean;
  h: PHashEntry;
  key32: cardinal;
  u: TStateInfo;
  mp: TMovePick;

begin
  init_node(ss);

  if alpha > mate_in(board.ply) then begin
    result := alpha;
    exit;
  end;
  best_score := mated_in(board.ply);
  if alpha < best_score then begin
    if beta <= best_score then begin
      result := beta;
      exit;
    end;
    alpha := best_score;
  end;

  result := SCORE_ZERO;
  if should_stop() then exit;

  with thread^.stats do begin
    if board.ply > maxply then maxply := board.ply;
    inc(nodes);
    if (threadID = 0) and (nodes and time_check_mask = 0) then
      if time_check() then exit;
  end;

  ss^.in_chk := board.st^.checkersBB <> 0;
  hash_move := NOMOVE;
  hm_score := -SCORE_INF;
  hm_depth := 0;

  h := @tt.table^[board.st^.hashkey and tt.hash_mask];
  key32 := cardinal(board.st^.hashkey shr 32);
  for i := 0 to HASH_SLOTS - 1 do begin
    if h^.key32 = key32 then begin

      if (h^.best_move <> NOMOVE) and (h^.depth_lower > hm_depth) then begin
        hash_move := h^.best_move;
        hm_depth := h^.depth_lower;
        hm_score := score_from_tt(h^.score_lower, board.ply);
      end;

      if h^.flags and BOUND_EXACT <> 0 then
        if h^.flags and HASH_NOMOVES <> 0 then begin
          if ss^.in_chk then
            result := mated_in(board.ply)
          else
            result := -draw_score[board.us];
          exit;
        end;

      if h^.depth_lower >= depth then begin
        h^.age := tt.generation;
        result := score_from_tt(h^.score_lower, board.ply);
        if h^.depth_lower = EGTB_DRAFT then begin
          if (result >= beta) or (result < alpha) then exit;
          if (result = SCORE_ZERO) or (abs(result) > RESIGN) then exit;
        end;
      end;
    end;
    inc(h);
  end;

  if (depth >= EGTB_DEPTH) and (board.st^.flags and FLAG_PROBE_TB <> 0) then
    if egtb_probe(result, alpha, beta) then exit;

  if ((hash_move = NOMOVE) and (depth >= 3*ONE_PLY)) or
     ((depth >= 5*ONE_PLY) and (depth > hm_depth + 4*ONE_PLY)) then
  begin
    iid_alpha := max(alpha - 2*depth, -SCORE_INF);
    iid_beta := min(beta + 2*depth, SCORE_INF);
    score := alpha + 1;
    if (depth >= 5*PLYUNIT) then begin
      score := pv_search(ss, depth - 4*ONE_PLY, iid_alpha, iid_beta);
      if thread^.stop then exit;
      if score > iid_alpha then
        hash_move := ss^.curr_move;
    end;
    if score > iid_alpha then begin
      score := pv_search(ss, depth - 2*ONE_PLY, iid_alpha, iid_beta);
      if thread^.stop then exit;
      if score > iid_alpha then
        hash_move := ss^.curr_move;
    end;
  end;

  singular := 0;
  if not ss^.in_chk then
    mp.init(board, ss, S1_HASH, hash_move)
  else begin
    mp.init_evasions(board, ss, hash_move);
    if mp.num_moves <= 2 then
      singular := 3 - mp.num_moves;
  end;

  use_history := board.st^.eval + $80 > alpha;
  best_move := NOMOVE;
  movecnt := 0;

  while true do begin
    move := mp.get_next();
    if move = NOMOVE then break;
    ss^.curr_move := move;
    inc(movecnt);

    if (alpha > draw_score[board.they]) and is_quiet(move) then
      if (board.st^.rule50 >= 2) and (ss[-2].curr_move = reversed(move)) then begin
        best_score := max(draw_score[board.they], best_score);
        continue;
      end;

    board.make_move(move, u);
    if move = hash_move then
      thread^.evaluator.evaluate(board, alpha, beta, 0)
    else
      thread^.evaluator.evaluate(board, alpha, beta, LAZY_PV);
    flags := u.flags;
    if flags and FLAG_EXACT = 0 then begin

      ext := extend(ss, NODE_PV, hash_move);
      if ext < singular then
        ext := singular;

      if (move = HASH_MOVE) and (depth >= 8*ONE_PLY) and (ext < 2) then
        if u.phase.total <> 0 then begin
          if hm_score < alpha then hm_score := alpha;
          if hm_score > beta  then hm_score := beta;
          if abs(hm_score) < RESIGN then begin
            board.undo_move(move);
            new_depth := depth - min(6*ONE_PLY, depth shr 1);
            if ext = 0 then begin
              offset := hm_score - (depth + depth shr 2);
              score := scout_search(ss, NODE_ALL, new_depth, offset, move);
              if thread^.stop then exit;
              if score < offset then ext := 1;
            end;
            if ext = 1 then begin
              offset := hm_score - (depth*2 + depth shr 1);
              score := scout_search(ss, NODE_ALL, new_depth, offset, move);
              if thread^.stop then exit;
              if score < offset then ext := 2;
            end;
            board.make_move(move, u);
            thread^.evaluator.evaluate(board, 0, 0, 0);
            ss^.curr_move := move;
          end;
        end;

      singular := 0;
      new_depth := depth - ONE_PLY + ext;

      if new_depth < ONE_PLY then begin
        if (  (u.threat[board.us] <> 0)
          and (not is_quiet(move) or (u.threat[board.us] > ss^.threat[board.us]))
          and (-u.eval <= alpha + u.tempo)
          and (u.qs_delay = 0)) then
        begin
          u.qs_delay := 1;
          new_depth := ONE_PLY;
        end
        else begin
          if u.checkersBB <> 0 then
            score := -qevasions_pv(@ss[1], 1, -beta, -alpha)
          else
            score := -quiesce_pv(@ss[1], 1, -beta, -alpha);
        end;
      end;

      if new_depth >= ONE_PLY then begin
        if (move = hash_move) then
          score := -pv_search(@ss[1], new_depth, -beta, -alpha)
        else begin
          score := alpha + 1;
          if (new_depth > LMR_DEPTH_PV) and (movecnt > LMR_COUNT_PV) then
            if (mp.phase = S1_QUIET) and (ext = 0) then begin
              lmr_depth := new_depth - msb_8bit[movecnt - 5];
              if lmr_depth < ONE_PLY then
                lmr_depth := ONE_PLY;
              ss^.reduction := new_depth - lmr_depth;
              score := -scout_search(@ss[1], NODE_CUT, lmr_depth, -alpha);
              ss^.reduction := 0;
            end;

          if (score > alpha) and not thread^.stop then
            score := -scout_search(@ss[1], NODE_CUT, new_depth, -alpha);

          if (score > alpha) and not thread^.stop then
            score := -pv_search(@ss[1], new_depth, -beta, -alpha);
        end
      end;
    end
    else
      score := -u.eval;

    board.undo_move(move);
    if thread^.stop then exit;

    if score <= alpha then
      if use_history and is_quiet(move) and (flags and FLAG_REPETITION = 0) then
        hist.record_bad_move(move, depth);

    if score > best_score then
    begin
      best_score := score;
      if score > alpha then
      begin
        best_move := move;
        if is_quiet(move) and (flags and FLAG_REPETITION = 0) then
          hist.record_good_move(move, depth);
        tt.store_lower(board.st^.hashkey, depth, score, board.ply, move);
        alpha := score;
        if score >= beta then begin
          if is_quiet(move) and (flags and FLAG_REPETITION = 0) then
            update_killers(ss, move);
          break;
        end;
      end;
    end;

    if (engine.free_cpu_mask <> 0) and
       (depth >= engine.split_depth_pv) and
       (not thread^.stop) and
       (engine.has_free_threads(threadID)) and
       split(ss, depth, alpha, beta, best_score, best_move, movecnt, mp, NODE_PV) then break;
  end;

  ss^.curr_move := best_move;

  if movecnt = 0 then begin
    if ss^.in_chk then
      result := mated_in(board.ply)
    else
      result := -draw_score[board.us];
    tt.store_exact(board.st^.hashkey, depth, result, board.ply, NOMOVE,
      BOUND_EXACT or HASH_NOMOVES);
    exit;
  end;

  if best_score < beta then
    if best_move <> NOMOVE then
      tt.store_exact(board.st^.hashkey, depth, best_score, board.ply, best_move)
    else
      tt.store_upper(board.st^.hashkey, depth, best_score, board.ply);

  result := best_score;
end;

procedure TSearch.smp_pv(var sp: TSplitPoint);
var
  ss: PSS;
  move: TMove;
  phase: TGenPhase;
  depth, new_depth, lmr_depth: integer;
  ext, flags, movecnt: integer;
  use_history: boolean;
  score: TScore;
  u: TStateInfo;

begin
  ss := @sp.search_stack[ThreadID, 2];
  depth := sp.depth;

  use_history := board.st^.eval + $80 > sp.alpha;

  while not thread^.stop do begin
    EnterCriticalSection(sp.lock);
    move := sp.mp^.smp_get_next();
    if move = NOMOVE then begin
      LeaveCriticalSection(sp.lock);
      break;
    end;
    phase := sp.mp^.phase;
    inc(sp.movecnt);
    movecnt := sp.movecnt;
    LeaveCriticalSection(sp.lock);
    ss^.curr_move := move;

    if (sp.alpha > draw_score[board.they]) and (board.st^.rule50 >= 2) then
      if is_quiet(move) and (ss[-2].curr_move = reversed(move)) then begin
        if sp.best_score < draw_score[board.they] then begin
          EnterCriticalSection(sp.lock);
          if sp.best_score < draw_score[board.they] then
            sp.best_score := draw_score[board.they];
          LeaveCriticalSection(sp.lock);
        end;
        continue;
      end;

    board.make_move(move, u);
    thread^.evaluator.evaluate(board, sp.alpha, sp.beta, LAZY_PV);
    flags := u.flags;
    if flags and FLAG_EXACT = 0 then begin

      ext := extend(ss, NODE_PV, NOMOVE);
      new_depth := depth - PLYUNIT - ext;

      score := sp.alpha + 1;
      if (new_depth > LMR_DEPTH_PV) and (movecnt > LMR_COUNT_PV) then
        if (phase = S1_QUIET) and (ext = 0) then begin
          lmr_depth := new_depth - msb_8bit[movecnt-5];
          if lmr_depth < ONE_PLY then lmr_depth := ONE_PLY;
          ss^.reduction := new_depth - lmr_depth;
          score := -scout_search(@ss[1], NODE_CUT, lmr_depth, -sp.alpha);
          ss^.reduction := 0;
        end;

      if (score > sp.alpha) and not thread^.stop then
        score := -scout_search(@ss[1], NODE_CUT, new_depth, -sp.alpha);

      if (score > sp.alpha) and not thread^.stop then
        score := -pv_search(@ss[1], new_depth, -sp.beta, -sp.alpha);
    end
    else
      score := -u.eval;

    board.undo_move(move);
    if thread^.stop then break;

    if score <= sp.alpha then
      if use_history and is_quiet(move) and (flags and FLAG_REPETITION = 0) then
        hist.record_bad_move(move, depth);

    if score > sp.best_score then begin
      EnterCriticalSection(sp.lock);
      if (score > sp.best_score) and not thread^.stop then begin
        sp.best_score := score;
        if score > sp.alpha then begin
          sp.best_move := move;
          if score >= sp.beta then begin
            sp.finished := true;
            dec(sp.cpus);
            sp.slaves[threadID] := 0;
            LeaveCriticalSection(sp.lock);
            if is_quiet(move) and (flags and FLAG_REPETITION = 0) then
              hist.record_good_move(move, depth);
            exit;
          end;
          sp.alpha := score;
          LeaveCriticalSection(sp.lock);
          tt.store_lower(board.st^.hashkey, depth, score, board.ply, move);
        end
        else
          LeaveCriticalSection(sp.lock);
      end
      else
        LeaveCriticalSection(sp.lock);
    end;
  end;

  EnterCriticalSection(sp.lock);
  dec(sp.cpus);
  sp.slaves[threadID] := 0;
  LeaveCriticalSection(sp.lock);
end;

procedure TSearch.smp_scout(nt: TNodeType; var sp: TSplitPoint);
var
  ss: PSS;
  move: TMove;
  scout, eval, score, fmargin, emargin: TScore;
  depth, new_depth: integer;
  movecnt, flags, ext: integer;
  phase: TGenPhase;
  use_futility, use_history: boolean;
  u: TStateInfo;
begin
  ss := @sp.search_stack[threadID, 2];
  ss^.in_chk := board.st^.checkersBB <> 0;
  eval := board.st^.eval;
  depth := sp.depth;
  scout := sp.beta;

  if (ss^.in_chk) then begin
    use_futility := (depth < FUTILITY_DEPTH);
    use_history := eval + $80 > scout;
  end
  else begin
    use_futility := false;
    use_history := false;
  end;

  while not thread^.stop do begin
    EnterCriticalSection(sp.lock);
    move := sp.mp^.smp_get_next();
    if move = NOMOVE then begin
      LeaveCriticalSection(sp.lock);
      break;
    end;
    phase := sp.mp^.phase;
    movecnt := sp.movecnt;
    inc(sp.movecnt);
    LeaveCriticalSection(sp.lock);
    ss^.curr_move := move;

    if (scout > draw_score[board.they]) and (board.st^.rule50 >= 2) then
      if is_quiet(move) and (ss[-2].curr_move = reversed(move)) then begin
        if ss^.in_chk and (sp.best_score < draw_score[board.they]) then begin
          EnterCriticalSection(sp.lock);
          if (sp.best_score < draw_score[board.they]) then
            sp.best_score := draw_score[board.they];
          LeaveCriticalSection(sp.lock);
        end;
        continue;
      end;

    if use_futility and (phase = S1_QUIET) and not board.is_check(move) then begin
      if (movecnt > 6) and is_quiet(move) then begin
        fmargin := (3 + 10*nt) shl (depth - 6);
        if (eval + hist.gain(move) + fmargin < scout + $64 + 5*movecnt) then
          continue;
      end;
      if nt = NODE_ALL then
        if (5 shl (depth - 6) + eval - $140 < scout) then
          if non_king_move(move) and bad_see(board, move) then
            continue;
    end;

    if (phase = S2_CHECKS) and non_king_move(move) then
      if not board.is_dc(move) and bad_see(board, move) then
        continue;

    board.make_move(move, u);
    thread^.evaluator.evaluate(board, scout, scout, LAZY_NONPV);
    flags := u.flags;
    if flags and FLAG_EXACT = 0 then begin

      if use_futility and is_quiet(move) and (movecnt > 3 * (nt + 1)) then
        if u.checkersBB = 0 then begin
          emargin := 5 shl (depth - 6);
          if -u.eval + emargin < scout + 3*movecnt - 42 then
            if u.threat[board.us] <= ss^.threat[board.us] then begin
              board.undo_move(move);
              continue;
            end;
        end;

      ext := extend(ss, nt, NOMOVE);
      score := scout;

      if board.st^.checkersBB = 0 then begin
        ss^.reduction := reduce(nt, ss^.in_chk, ext, depth, movecnt, sp.mp^.phase);
        if ss^.reduction <> 0 then begin
          new_depth := depth - ONE_PLY + ext - ss^.reduction;
          score := -scout_search(@ss[1], nt xor 1, new_depth, -scout+1);
          ss^.reduction := 0;
        end;
      end;

      if (score >= scout) and not thread^.stop then begin
        new_depth := depth - PLYUNIT + ext;
        score := -scout_search(@ss[1], nt xor 1, new_depth, -scout+1);
      end;
    end
    else
      score := -u.eval;

    board.undo_move(move);
    if thread^.stop then break;

    if score > sp.best_score then begin
      EnterCriticalSection(sp.lock);
      if score > sp.best_score then begin
        sp.best_score := score;
        if score >= scout then begin
          sp.finished := true;
          sp.best_move := move;
          dec(sp.cpus);
          sp.slaves[threadID] := 0;
          LeaveCriticalSection(sp.lock);
          if is_quiet(move) and not ss^.in_chk and (flags and FLAG_REPETITION = 0) then
            hist.record_good_move(move, depth);
          exit;
        end;
      end;
      LeaveCriticalSection(sp.lock);
    end;

    if use_history and is_quiet(move) and (flags and FLAG_REPETITION = 0) then
      hist.record_bad_move(move, depth);
  end;

  EnterCriticalSection(sp.lock);
  dec(sp.cpus);
  sp.slaves[threadID] := 0;
  LeaveCriticalSection(sp.lock);
end;

function TSearch.split(
  ss: PSS;
  depth: integer;
  alpha, beta: TScore;
  var best_score: TScore;
  var best_move: TMove;
  var movecnt: integer;
  var mp: TMovePick;
  node_type: TNodeType): boolean;
var
  sp: PSplitPoint;
  t: PThread;
  i: integer;
begin
  t := thread;

  EnterCriticalSection(engine.smp_lock);

  if not engine.has_free_threads(threadID) or (t^.sp_count >= MAX_SPLIT_POINTS) then
  begin
    LeaveCriticalSection(engine.smp_lock);
    result := false;
    exit;
  end;

  inc(engine.splits);
  sp := @engine.split_points[threadID, t^.sp_count];
  inc(t^.sp_count);

  EnterCriticalSection(sp^.lock);

  sp^.parent := t^.split_point;
  sp^.finished := false;
  sp^.depth := depth;
  sp^.alpha := alpha;
  sp^.beta := beta;
  sp^.nt := node_type;
  sp^.best_score := best_score;
  sp^.best_move := best_move;
  sp^.master := threadID;
  sp^.mp := @mp;
  sp^.cpus := 1;
  sp^.board := @board;
  sp^.movecnt := movecnt;

  move(ss[-2], sp^.search_stack[threadID, 0], 4*sizeof(TSearchStack));
  t^.split_point := sp;
  t^.state := TS_WORK_PENDING;

  for i := 0 to engine.num_threads - 1 do
    if engine.is_thread_available(threadID, i) then begin
      move(ss[-2], sp^.search_stack[i, 0], 4 * sizeof(TSearchStack));
      sp^.slaves[i] := 1;
      engine.threads[i].split_point := sp;
      engine.threads[i].state := TS_WORK_PENDING;
      inc(sp^.cpus);
      if sp^.cpus >= MAX_THREADS_PER_SP then break;
  end;
  assert(sp^.cpus > 1);

  LeaveCriticalSection(sp^.lock);
  LeaveCriticalSection(engine.smp_lock);

  engine.idle_loop(threadID, sp);

  EnterCriticalSection(engine.smp_lock);
  best_score := sp^.best_score;
  best_move := sp^.best_move;
  movecnt := sp^.movecnt;
  dec(t^.sp_count);
  t^.split_point := sp^.parent;
  LeaveCriticalSection(engine.smp_lock);
  result := true;
end;

end.
