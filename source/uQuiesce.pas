
unit uQuiesce;

interface

uses
  uHistory, uMove, uMoveGen, uScore, uSearchBase, uSearchDef, uSquare;

{$include platform.inc}

const
  QS_FMARGIN = $80;

type
  TQSearch = object (TSearchBase)
    function quiesce(ss: PSS; depth: integer; scout: TScore): TScore;

    function quiesce_pv(ss: PSS; depth: integer; alpha, beta: TScore): TScore;

    function qevasions(ss: PSS; depth: integer; scout: TScore): TScore;

    function qevasions_pv(ss: PSS; depth: integer; alpha, beta: TScore): TScore;
  end;

implementation

uses
  uBitboard, uBoard, uEval, uMovePick, uPiece, uSee, uTranstab;

function TQSearch.qevasions(ss: PSS; depth: integer; scout: TScore): TScore;
var
  eval, best_score: TScore;
  h: PHashEntry;
  move, hash_move: TMove;
  move_depth, num_moves: integer;
  has_piece: boolean;
  skipped, i: integer;
  key32: cardinal;
  target, mask: TBitboard;
  ml: TMoveList;
  mptr: PMoveStack;
  u: TStateInfo;

begin
  assert(board.st^.checkersBB <> 0);
  assert(board.st^.flags and FLAG_EXACT = 0);

  ss^.curr_move := NOMOVE;

  best_score := mated_in(board.ply);
  if best_score >= scout then begin
    result := scout;
    exit;
  end;

  if scout > mate_in(board.ply) then begin
    result := scout-1;
    exit;
  end;

  result := SCORE_ZERO;
  if (should_stop()) then exit;

  inc(thread^.stats.nodes);
  if (threadID = 0) and (thread^.stats.nodes and time_check_mask = 0) then
    if time_check() then exit;

  hash_move := NOMOVE;
  move_depth := 0;

  h := @tt.table^[board.st^.hashkey and tt.hash_mask];
  key32 := cardinal(board.st^.hashkey shr 32);
  for i := 0 to HASH_SLOTS - 1 do begin
    if h^.key32 = key32 then begin
      if h^.depth_lower <> 0 then begin
        if (h^.best_move <> NOMOVE) and (h^.depth_lower > move_depth) then begin
          hash_move := h^.best_move;
          move_depth := h^.depth_lower;
          ss^.curr_move := hash_move;
        end;
        result := score_from_tt(h^.score_lower, board.ply);
        if result >= scout then exit;
      end;
      if h^.depth_upper <> 0 then begin
        result := score_from_tt(h^.score_upper, board.ply);
        if result < scout then exit;
      end;
    end;
    inc(h);
  end;

  target := TBB(BB_FULL);
  with board do begin
    eval := st^.eval;
    has_piece := count.np[us] >= 1;
    if eval + 26 < scout then begin
      best_score := eval + 26;
      target := bb.pieces[they];
      with delta_margins[st^.phase.total] do begin
        if eval + pawn_margin < scout then begin
          mask := bb.pawns[they] and NonRank7[they];
          target := target xor mask;
          if mask and st^.attacks[us] <> 0 then
            best_score := eval + pawn_inc;
          if eval + minor_margin < scout then begin
            mask := bb.knights[they] or bb.bishops[they];
            target := target xor mask;
            if mask and st^.attacks[us] <> 0 then
              best_score := eval + minor_inc;
            if eval + rook_margin < scout then begin
              target := target xor bb.rooks[they];
              if bb.rooks[they] and st^.attacks[us] <> 0 then
                best_score := eval + rook_inc;
            end;
          end;
        end;
      end;
    end;
  end;

  num_moves := gen_evasions(board, @ml, target);
  if num_moves = 1 then
    inc(depth);

  for i := 0 to num_moves-1 do begin
    with ml[i] do begin
      if move = hash_move then
        score := 10 * MAX_HIST
      else if is_capture(move) then
        score := 9 * MAX_HIST + MVV_LVA_M[(move shr SHIFT_PIECE) and $ff]
      else
        score := EV_SORT[piece_moved(move)] * MAX_HIST;
    end;
  end;

  skipped := 0;
  mptr := @ml;

  while true do begin
    move := pick_move(mptr); inc(mptr);
    if (move = NOMOVE) then break;
    ss^.curr_move := move;

    if is_quiet(move) and (move <> hash_move) and (scout > -RESIGN) then begin

      if (scout > draw_score[board.they]) and (board.st^.rule50 >= 2) then
        if (ss[-2].curr_move = reversed(move)) then begin
          best_score := max(draw_score[board.they], best_score);
          continue;
        end;

      if not is_king_move(move) and bad_see(board, move) then begin
        inc(skipped);
        continue;
      end;

      if has_piece then
        if eval + hist.gain(move) < scout + $40 then begin
          inc(skipped);
          continue;
        end;
    end;

    board.make_move(move, u);
    thread^.evaluator.evaluate(board, scout, scout, LAZY_QS);

    if u.flags and FLAG_EXACT = 0 then begin
      if u.checkersBB <> 0 then
        result := -qevasions(@ss[1], depth-1, -scout+1)
      else
        result := -quiesce(@ss[1], depth-1, -scout+1);
    end
    else
      result := -u.eval;

    board.undo_move(move);

    if thread^.stop then exit;

    if result > best_score then begin
      best_score := result;
      if result >= scout then begin
        tt.store_lower(board.st^.hashkey, 1, result, board.ply, move);
        exit;
      end;
    end;
  end;

  if (skipped <> 0) then begin
    if eval + 26 > best_score then best_score := eval + 26;
    if best_score >= scout then best_score := scout - 1;
  end;

  tt.store_upper(board.st^.hashkey, 1, best_score, board.ply);
  ss^.curr_move := NOMOVE;
  result := best_score;
end;

function TQSearch.qevasions_pv(ss: PSS; depth: integer; alpha, beta: TScore): TScore;
var
  eval, best_score: TScore;
  move, hash_move, best_move: TMove;
  move_depth, num_moves: integer;
  h: PHashEntry;
  i: integer;
  key32: cardinal;
  target, mask: TBitboard;
  ml: TMoveList;
  mptr: PMoveStack;
  u: TStateInfo;

begin
  assert(board.st^.checkersBB <> 0);
  assert(board.st^.flags and FLAG_EXACT = 0);

  ss^.curr_move := NOMOVE;

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
  if (should_stop()) then exit;

  inc(thread^.stats.nodes);
  if (threadID = 0) and (thread^.stats.nodes and time_check_mask = 0) then
    if time_check() then exit;

  hash_move := NOMOVE;
  move_depth := 0;

  h := @tt.table^[board.st^.hashkey and tt.hash_mask];
  key32 := cardinal(board.st^.hashkey shr 32);
  for i := 0 to HASH_SLOTS - 1 do begin
    if h^.key32 = key32 then begin
      if h^.depth_lower <> 0 then begin
        if (h^.best_move <> NOMOVE) and (h^.depth_lower > move_depth) then begin
          move_depth := h^.depth_lower;
          hash_move := h^.best_move;
          ss^.curr_move := h^.best_move;
        end;
        result := score_from_tt(h^.score_lower, board.ply);
        if (result >= beta) or (h^.flags and BOUND_EXACT <> 0) then exit;
      end;
      if h^.depth_upper <> 0 then begin
        result := score_from_tt(h^.score_upper, board.ply);
        if result < alpha then exit;
      end;
    end;
    inc(h);
  end;

  target := TBB(BB_FULL);
  with board do begin
    eval := st^.eval;
    if eval + 25 < alpha then begin
      best_score := eval + 25;
      target := bb.pieces[they];
      with delta_margins[st^.phase.total] do begin
        if eval + pawn_margin < alpha then begin
          mask := bb.pawns[they] and NonRank7[they];
          target := target xor mask;
          if mask and st^.attacks[us] <> 0 then
            best_score := eval + pawn_inc;
          if eval + minor_margin < alpha then begin
            mask := bb.knights[they] or bb.bishops[they];
            target := target xor mask;
            if mask and st^.attacks[us] <> 0 then
              best_score := eval + minor_inc;
            if eval + rook_margin < alpha then begin
              target := target xor bb.rooks[they];
              if bb.rooks[they] and st^.attacks[us] <> 0 then
                best_score := eval + rook_inc;
            end;
          end;
        end;
      end;
    end;
  end;

  num_moves := gen_evasions(board, @ml, target);
  if num_moves = 1 then inc(depth);

  for i := 0 to num_moves-1 do begin
    with ml[i] do begin
      if move = hash_move then
        score := 10 * MAX_HIST
      else if is_capture(move) then
        score := 9 * MAX_HIST + MVV_LVA_M[(move shr SHIFT_PIECE) and $ff]
      else
        score := EV_SORT[piece_moved(move)] * MAX_HIST;
    end;
  end;

  mptr := @ml;
  best_move := NOMOVE;

  while true do begin
    move := pick_move(mptr); inc(mptr);
    if move = NOMOVE then break;
    ss^.curr_move := move;

    board.make_move(move, u);
    if move = hash_move then
      thread^.evaluator.evaluate(board, alpha, beta, 0)
    else
      thread^.evaluator.evaluate(board, alpha, beta, LAZY_PV_QS);

    if u.flags and FLAG_EXACT = 0 then begin
      if u.checkersBB <> 0 then
        result := -qevasions_pv(@ss[1], depth-1, -beta, -alpha)
      else
        result := -quiesce_pv(@ss[1], depth-1, -beta, -alpha);
    end
    else
      result := -u.eval;

    board.undo_move(move);

    if thread^.stop then exit;

    if result > best_score then begin
      best_score := result;
      if result > alpha then begin
        if result >= beta then begin
          tt.store_lower(board.st^.hashkey, 1, result, board.ply, move);
          exit;
        end;
        alpha := result;
        best_move := move;
      end;
    end;
  end;

  ss^.curr_move := best_move;
  if best_move <> NOMOVE then
    tt.store_exact(board.st^.hashkey, 1, best_score, board.ply, best_move)
  else
    tt.store_upper(board.st^.hashkey, 1, best_score, board.ply);
  result := best_score;
end;

function TQSearch.quiesce(ss: PSS; depth: integer; scout: TScore): TScore;
var
  eval, best_score: TScore;
  move, hash_move: TMove;
  move_depth: integer;
  h: PHashEntry;
  i: integer;
  key32: cardinal;
  target, mask: TBitboard;
  ml: TMoveList;
  mptr: PMoveStack;
  u: TStateInfo;

begin
  assert(board.st^.checkersBB = 0);
  assert(board.st^.flags and FLAG_EXACT = 0);

  ss^.curr_move := NOMOVE;
  if scout <= mated_in(board.ply) then begin
    result := scout;
    exit;
  end;
  if scout > mate_in(board.ply) then begin
    result := scout-1;
    exit;
  end;

  result := SCORE_ZERO;
  if (should_stop()) then exit;

  inc(thread^.stats.nodes);
  if (threadID = 0) and (thread^.stats.nodes and time_check_mask = 0) then
    if time_check() then exit;

  hash_move := NOMOVE;
  move_depth := 0;

  h := @tt.table^[board.st^.hashkey and tt.hash_mask];
  key32 := cardinal(board.st^.hashkey shr 32);
  for i := 0 to HASH_SLOTS - 1 do begin
    if h^.key32 = key32 then begin
      if h^.depth_lower <> 0 then begin
        if (h^.best_move <> NOMOVE) and (h^.depth_lower > move_depth) then begin
          move_depth := h^.depth_lower;
          hash_move := h^.best_move;
          ss^.curr_move := h^.best_move;
        end;
        result := score_from_tt(h^.score_lower, board.ply);
        if result >= scout then exit;
      end;
      if h^.depth_upper <> 0 then begin
        result := score_from_tt(h^.score_upper, board.ply);
        if result < scout then exit;
      end;
    end;
    inc(h);
  end;

  eval := board.st^.eval;
  best_score := board.st^.eval + board.st^.tempo;
  if best_score >= scout then begin
    result := best_score;
    exit;
  end;

  if not is_quiet(hash_move) and board.hm_is_legal(hash_move) then begin
    ss^.curr_move := hash_move;
    board.make_move(hash_move, u);
    thread^.evaluator.evaluate(board, scout, scout, LAZY_QS);
    if u.flags and FLAG_EXACT = 0 then begin
      if u.checkersBB <> 0 then
        result := -qevasions(@ss[1], depth-1, -scout+1)
      else
        result := -quiesce(@ss[1], depth-1, -scout+1);
    end
    else
      result := -u.eval;
    board.undo_move(hash_move);
    if result > best_score then begin
      best_score := result;
      if result >= scout then begin
        tt.store_lower(board.st^.hashkey, 1, result, board.ply, hash_move);
        exit;
      end;
    end;
  end
  else
    hash_move := NOMOVE;

  with board do begin
    target := bb.pieces[they];
    if count.np[us] <> 0 then
      with delta_margins[st^.phase.total] do begin
        if eval + pawn_margin < scout then begin
          mask := bb.pawns[they] and NonRank7[they];
          target := target xor mask;
          if mask and st^.attacks[us] <> 0 then
            best_score := eval + pawn_inc;
          if eval + minor_margin < scout then begin
            mask := bb.knights[they] or bb.bishops[they];
            target := target xor mask;
            if mask and st^.attacks[us] <> 0 then
              best_score := eval + minor_inc;
            if eval + rook_margin < scout then begin
              target := target xor bb.rooks[they];
              if bb.rooks[they] and st^.attacks[us] <> 0 then
                best_score := eval + rook_inc;
            end;
          end;
        end;
      end;
  end;

  gen_captures(board, @ml, target);
  mptr := @ml;

  while true do begin
    move := pick_move(mptr); inc(mptr);
    if move = NOMOVE then break;

    if (move = hash_move) then continue;

    if not is_king_move(move) then begin
      if not board.is_legal(move) then continue;
      if (board.st^.pins[board.us] and SquareBB[source_sq(move)] = 0) then
        if bad_see(board, move) then continue;
    end;

    ss^.curr_move := move;

    board.make_move(move, u);
    thread^.evaluator.evaluate(board, scout, scout, LAZY_QS);
    if u.flags and FLAG_EXACT = 0 then begin
      if u.checkersBB <> 0 then
        result := -qevasions(@ss[1], depth-1, -scout+1)
      else
        result := -quiesce(@ss[1], depth-1, -scout+1);
    end
    else
      result := -u.eval;
    board.undo_move(move);

    if thread^.stop then exit;

    if result > best_score then begin
      best_score := result;
      if result >= scout then begin
        tt.store_lower(board.st^.hashkey, 1, result, board.ply, move);
        exit;
      end;
    end;
  end;

  if (depth >= -1) and (eval + $100 + (30 shl (depth+4)) >= scout) then begin
    gen_checks(board, @ml, target);
    mptr := @ml;
    while true do begin
      move := mptr^.move; inc(mptr);
      if move = NOMOVE then break;

      if (move = hash_move) or not board.is_legal(move) then continue;

      if (scout > draw_score[board.they]) and (board.st^.rule50 >= 2) and
         (ss[-2].curr_move = reversed(move)) then continue;

      if not board.is_dc(move) and bad_see(board, move) then continue;

      ss^.curr_move := move;

      board.make_move(move, u);
      thread^.evaluator.evaluate(board, scout, scout, LAZY_QS);
      if u.flags and FLAG_EXACT = 0 then
        result := -qevasions(@ss[1], depth-1, -scout+1)
      else
        result := -u.eval;
      board.undo_move(move);

      if thread^.stop then exit;

      if result > best_score then begin
        best_score := result;
        if result >= scout then begin
          if is_quiet(move) then
            update_killers(ss, move);
          tt.store_lower(board.st^.hashkey, 1, result, board.ply, move);
          exit;
        end;
      end;
    end;
  end;

  ss^.curr_move := NOMOVE;
  tt.store_upper(board.st^.hashkey, 1, best_score, board.ply);
  result := best_score;
end;

function TQSearch.quiesce_pv(ss: PSS; depth: integer; alpha, beta: TScore): TScore;
var
  eval, best_score: TScore;
  move, hash_move, best_move: TMove;
  move_depth: integer;
  target, mask: TBitboard;
  h: PHashEntry;
  i: integer;
  key32: cardinal;
  mptr, bptr: PMoveStack;
  u: TStateInfo;
  ml: TMoveList;

begin
  assert(board.st^.checkersBB = 0);
  assert(board.st^.flags and FLAG_EXACT = 0);

  ss^.curr_move := NOMOVE;

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
  if (should_stop()) then exit;

  inc(thread^.stats.nodes);
  if (threadID = 0) and (thread^.stats.nodes and time_check_mask = 0) then
    if time_check() then exit;

  hash_move := NOMOVE;
  move_depth := 0;

  h := @tt.table^[board.st^.hashkey and tt.hash_mask];
  key32 := cardinal(board.st^.hashkey shr 32);
  for i := 0 to HASH_SLOTS - 1 do begin
    if h^.key32 = key32 then begin
      if h^.depth_lower <> 0 then begin
        if (h^.best_move <> NOMOVE) and (h^.depth_lower > move_depth) then begin
          move_depth := h^.depth_lower;
          hash_move := h^.best_move;
          ss^.curr_move := h^.best_move;
        end;
        result := score_from_tt(h^.score_lower, board.ply);
        if (result >= beta) or (h^.flags and BOUND_EXACT <> 0) then exit;
      end;
      if h^.depth_upper <> 0 then begin
        result := score_from_tt(h^.score_upper, board.ply);
        if result < alpha then exit;
      end;
    end;
    inc(h);
  end;

  eval := board.st^.eval;
  best_score := eval + board.st^.tempo;

  if best_score >= beta then begin
    result := best_score;
    exit;
  end;

  target := board.bb.pieces[board.they];
  if (best_score <= alpha) then begin
    if (board.count.np[board.us] >= 1) then
      with board, delta_margins[st^.phase.total] do
        if eval + pawn_margin < alpha then begin
          mask := bb.pawns[they] and NonRank7[they];
          target := target xor mask;
          if mask and st^.attacks[us] <> 0 then
            best_score := eval + pawn_inc;
          if eval + minor_margin < alpha then begin
            mask := bb.knights[they] or bb.bishops[they];
            target := target xor mask;
            if mask and st^.attacks[us] <> 0 then
              best_score := eval + minor_inc;
            if eval + rook_margin < alpha then begin
              target := target xor bb.rooks[they];
              if bb.rooks[they] and st^.attacks[us] <> 0 then
                best_score := eval + rook_inc;
            end;
          end;
        end;
  end
  else
    alpha := best_score;

  gen_captures(board, @ml, target);
  mptr := @ml;
  if hash_move <> NOMOVE then
    while (mptr^.move <> NOMOVE) do begin
      if mptr^.move = hash_move then begin
        inc(mptr^.score, $40000000);
        break;
      end;
      inc(mptr);
    end;

  mptr := @ml;
  bptr := @ml[MAX_MOVES-1];
  best_move := NOMOVE;

  while true do begin
    move := pick_move(mptr); inc(mptr);
    if move = NOMOVE then break;

    ss^.curr_move := move;

    if not is_king_move(move) then begin
      if not board.is_legal(move) then continue;
      if (move <> hash_move) then
        if board.st^.pins[board.us] and SquareBB[source_sq(move)] = 0 then
          if bad_see(board, move) then begin
            bptr^.move := move;
            dec(bptr);
            continue;
          end;
    end;

    board.make_move(move, u);
    if move = hash_move then
      thread^.evaluator.evaluate(board, alpha, beta, 0)
    else
      thread^.evaluator.evaluate(board, alpha, beta, LAZY_PV_QS);
    if u.flags and FLAG_EXACT = 0 then begin
      if u.checkersBB <> 0 then
        result := -qevasions_pv(@ss[1], depth-1, -beta, -alpha)
      else
        result := -quiesce_pv(@ss[1], depth-1, -beta, -alpha);
    end
    else
      result := -u.eval;
    board.undo_move(move);

    if thread^.stop then exit;

    if result > best_score then begin
      best_score := result;
      if result > alpha then begin
        if result >= beta then begin
          tt.store_lower(board.st^.hashkey, 1, result, board.ply, move);
          result := result;
          exit;
        end;
        alpha := result;
        best_move := move;
      end;
    end;
  end;

  if depth > 0 then begin
    bptr^.move := NOMOVE;
    mptr := @ml[MAX_MOVES-1];
    while mptr^.move <> NOMOVE do begin
      move := mptr^.move; dec(mptr);
      ss^.curr_move := move;

      board.make_move(move, u);
      if move = hash_move then
        thread^.evaluator.evaluate(board, alpha, beta, 0)
      else
        thread^.evaluator.evaluate(board, alpha, beta, LAZY_PV_QS);
      if u.flags and FLAG_EXACT = 0 then begin
        if u.checkersBB <> 0 then
          result := -qevasions_pv(@ss[1], depth-1, -beta, -alpha)
        else
          result := -quiesce_pv(@ss[1], depth-1, -beta, -alpha);
      end
      else
        result := -u.eval;
      board.undo_move(move);

      if thread^.stop then exit;

      if result > best_score then begin
        best_score := result;
        if result > alpha then begin
          if result >= beta then begin
            tt.store_lower(board.st^.hashkey, 1, result, board.ply, move);
            exit;
          end;
          alpha := result;
          best_move := move;
        end;
      end;
    end;
  end;

  if (depth >= -2) and (eval + $100 + (30 shl (depth + 5)) >= alpha) then begin
    mptr := @ml;
    gen_checks(board, @ml, target);

    while mptr^.move <> NOMOVE do begin
      move := mptr^.move; inc(mptr);
      if not board.is_legal(move) then continue;
      if not board.is_dc(move) and bad_see(board, move) then continue;
      ss^.curr_move := move;

      board.make_move(move, u);
      if move = hash_move then
        thread^.evaluator.evaluate(board, alpha, beta, 0)
      else
        thread^.evaluator.evaluate(board, alpha, beta, LAZY_PV_QS);
      if u.flags and FLAG_EXACT = 0 then
        result := -qevasions_pv(@ss[1], depth-1, -beta, -alpha)
      else
        result := -u.eval;
      board.undo_move(move);

      if thread^.stop then exit;

      if result > best_score then begin
        best_score := result;
        if result > alpha then begin
          if result >= beta then begin
            if is_quiet(move) then
              update_killers(ss, move);
            tt.store_lower(board.st^.hashkey, 1, result, board.ply, move);
            result := result;
            exit;
          end;
          alpha := result;
          best_move := move;
        end;
      end;
    end;
  end;

  if (depth >= 0) and (eval + $180 >= alpha) then begin
    gen_positional(board, @ml, alpha - eval);
    sort_moves(ml);
    mptr := @ml;
    while mptr^.move <> NOMOVE do begin
      move := mptr^.move; inc(mptr);
      if not board.is_legal(move) or board.is_check(move) then continue;

      ss^.curr_move := move;

      board.make_move(move, u);
      if move = hash_move then
        thread^.evaluator.evaluate(board)
      else
        thread^.evaluator.evaluate(board, alpha, beta, LAZY_PV_QS);
      result := -u.eval;
      if (result < alpha) then begin
        board.undo_move(move);
        continue;
      end;
      if (u.flags and FLAG_EXACT = 0) then
        result := -quiesce_pv(@ss[1], depth-1, -beta, -alpha);
      board.undo_move(move);

      if thread^.stop then exit;

      if result > best_score then begin
        best_score := result;
        if result > alpha then begin
          if result >= beta then begin
            update_killers(ss, move);
            tt.store_lower(board.st^.hashkey, 1, result, board.ply, move);
            result := result;
            exit;
          end;
          alpha := result;
          best_move := move;
        end;
      end;
    end;
  end;

  ss^.curr_move := best_move;
  if best_move <> NOMOVE then
    tt.store_exact(board.st^.hashkey, 1, best_score, board.ply, best_move)
  else
    tt.store_upper(board.st^.hashkey, 1, best_score, board.ply);
  result := best_score;
end;

end.
