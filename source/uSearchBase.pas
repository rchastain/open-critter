
unit uSearchBase;

interface

uses
  uBitboard, uBoard, uMove, uMovepick, uPiece, uScore, uSearchDef, uSmp, uSquare;

{$include platform.inc}

procedure init_margins();

procedure update_killers(ss: PSS; move: TMove); inline;

type
  PSearchBase = ^TSearchBase;
  TSearchBase = object
    thread: PThread;
    threadID: integer;
    board: TBoard;
    root_state: TStateInfo;
    function get_null_depth(value_diff: TScore; depth: integer): integer; inline;
    function extend(ss: PSS; nt: TNodeType; hash_move: TMove): integer;
    function reduce(nt: TNodeType; in_chk: boolean; ext, depth, movecnt: integer;
      phase: TGenPhase): integer;
    procedure init_node(ss: PSS); inline;
    function egtb_probe(var score: TScore; alpha, beta: TScore): boolean;
    function time_check(): boolean;
    function should_stop(): boolean;
  class var
    null_r: array [0..575] of integer;
  end;

function nodes_to_mask(nodes: cardinal): cardinal;
function time_to_mask(time: cardinal): cardinal;

function perft(var b: TBoard; depth: integer): uint64;
function divide(var b: TBoard; depth: integer): uint64;

type
  TDeltaMargins = record
    pawn_margin: TScore;
    pawn_inc: TScore;
    minor_margin: TScore;
    minor_inc: TScore;
    rook_margin: TScore;
    rook_inc: TScore;
  end;

var
  time_check_mask: cardinal;
  delta_margins: array [TMtrlStage] of TDeltaMargins;

implementation

uses
  uEngine, uGtb, uMaterial, uMovegen, uNotation, uRoot,
  uSystem, uTranstab;

procedure TSearchBase.init_node(ss: PSS);
begin
  with ss^ do begin
    curr_move := NOMOVE;
    reduction := 0;
    threat[WHITE] := board.st^.threat[WHITE];
    threat[BLACK] := board.st^.threat[BLACK];
  end;
end;

procedure update_killers(ss: PSS; move: TMove); inline;
begin
  with ss^ do
    if killer_a <> move then begin
      killer_b := killer_a;
      killer_a := move;
    end;
end;

function TSearchBase.should_stop(): boolean;
var
  sp: PSplitPoint;
begin
  if not thread^.stop then begin
    sp := thread^.split_point;
    while sp <> nil do begin
      if not sp^.finished then
        sp := sp^.parent
      else begin
        result := true;
        exit;
      end;
    end;
    result := false;
  end
  else
    result := true;
end;

function TSearchBase.get_null_depth(value_diff: TScore; depth: integer): integer;
var
  depth_r, value_r: integer;
begin
  assert(value_diff >= 0);
  depth_r := max(46 * depth + 794, 2048);
  value_r := null_r[min(value_diff, 575)];
  result := depth - (depth_r + value_r) shr 8;
end;

function TSearchBase.reduce(nt: integer; in_chk: boolean; ext, depth, movecnt: integer;
  phase: TGenPhase): integer;
begin
  result := 0;
  if not in_chk then begin
    if phase = S1_QUIET then
      if nt = NODE_CUT then begin
        if ext = 0 then
          result := 2*ONE_PLY + msb_8bit[movecnt + 3];
      end
      else
        if (movecnt > 3) and (board.st^.checkersBB = 0) then
          result := msb_8bit[movecnt];
  end
  else begin
    if movecnt >= 2 then begin
      result := min(movecnt-1, ONE_PLY);
      if nt = NODE_CUT then begin
        inc(result);
        if depth > 8 then
          inc(result, msb_8bit[depth - 7]);
      end;
    end;
  end;
end;

function TSearchBase.extend(ss: PSS; nt: TNodeType; hash_move: TMove): integer;
var
  move: TMove;
  to_sq: TSquare;
  rr: TRank;
begin
  move := ss^.curr_move;
  with board do

  case nt of
    NODE_CUT:
      begin
        result := HALF_PLY;
        if st^.checkersBB <> 0 then exit;
        if is_np_capture(move) and (st^.phase.raw = 0) then exit;
        if ss^.in_chk then begin
          if (st^.phase.raw < 18) then result := 0;
          exit;
        end;
        if is_pawn_push(move) then begin
          to_sq := target_sq(move);
          if pawn_is_passed(to_sq, they) then begin
            rr := rank_of(relative(to_sq, they));
            if (move = HASH_MOVE) and (rr >= RANK_4) then exit;
            if rr >= RANK_6 then exit;
          end;
        end;
        if (move = HASH_MOVE) and is_capture(move) and is_capture(ss[-1].curr_move) then
          if target_sq(move) = target_sq(ss[-1].curr_move) then exit;
      end;

    NODE_ALL:
      begin
        result := HALF_PLY;
        if st^.checkersBB <> 0 then exit;
        if is_np_capture(move) and (st^.phase.raw = 0) then exit;
        if ss^.in_chk then begin
          if (st^.phase.raw < 18) then result := 0;
          exit;
        end;
        if is_pawn_push(move) then begin
          to_sq := target_sq(move);
          if pawn_is_passed(to_sq, they) then
            if rank_of(relative(to_sq, they)) >= RANK_6 then exit;
        end;
      end;

    NODE_PV:
      begin
        if is_np_capture(move) and (st^.phase.raw = 0) then begin
          result := ONE_PLY;
          exit;
        end;
        result := HALF_PLY;
        if is_pawn_push(move) then begin
          to_sq := target_sq(move);
          if pawn_is_passed(to_sq, they) then begin
            rr := rank_of(relative(to_sq, they));
            if rr >= RANK_6 then begin
              result := ONE_PLY;
              exit;
            end;
            if rr >= RANK_4 then exit;
          end;
        end;
        if is_capture(move) or (st^.checkersBB <> 0) then exit;
        if st^.threat[us] > ss^.threat[us] then exit;
        if ss^.in_chk and (st^.phase.raw >= 18) then exit;
      end;
  end;

  result := 0;
end;

function nodes_to_mask(nodes: cardinal): cardinal;
begin
  result := (1 shl (integer(BSR(nodes))) - 1);
end;

function time_to_mask(time: cardinal): cardinal;
begin
  result := (1 shl (integer(BSR(time)) + 5) - 1);
end;

function TSearchBase.time_check(): boolean;
var
  time_left: time_t;
  rt: integer;
  remaining_nodes: int64;
  root: ^TRoot;
begin
  root := @engine.ai;
  with engine do begin
    calc_time();
    info_callback(root^.iteration, 0, 0, nil);
    if time_ctrl.max_nodes = 0 then begin
      rt := 2048;
      if not pondering and (time_ctrl.max_depth = 0) then begin
        time_left := time_max - time_elapsed;
        if time_left < 2048 then begin
          if time_left < LATENCY then begin
            stop_threads();
            result := true;
            exit;
          end;
          rt := integer(time_left);
        end;
        if (time_ctrl.mode <> TM_FIXED) and not root^.score_drop then
          if not root^.root_changed and (time_elapsed > time_normal) then begin
            stop_threads();
            result := true;
            exit;
          end;
      end;
      time_check_mask := time_to_mask(rt);
    end
    else begin
      remaining_nodes := time_ctrl.max_nodes - stats.nodes;
      if remaining_nodes <= 0 then begin
        stop_threads();
        result := true;
        exit;
      end;
      if remaining_nodes < 65536 then
        time_check_mask := nodes_to_mask(remaining_nodes)
      else
        time_check_mask := 65535;
    end;
    result := stop;
  end;
end;

function TSearchBase.egtb_probe(var score: TScore; alpha, beta: TScore): boolean;
var
  wdl, dtm: integer;
begin
  if gtb_probe_wdl(board, PROBE_HARD, wdl) then begin
    inc(thread^.stats.tbhits);
    result := true;
    if wdl = ord(tb_DRAW) then begin
      score := SCORE_ZERO;
      tt.store_exact(board.st^.hashkey, EGTB_DRAFT, score, board.ply, NOMOVE);
      exit;
    end;
    if wdl = ord(tb_WMATE) then score := RESIGN else score := -RESIGN;
    score := score * sign[board.us];
    if score > SCORE_ZERO then begin
      if beta < SCORE_WIN then begin
        tt.store_lower(board.st^.hashkey, EGTB_DRAFT, score, board.ply, NOMOVE);
        exit;
      end
      else begin
        gtb_probe_dtm(board, PROBE_HARD, wdl, dtm);
        score := mate_in(board.ply + dtm);
        tt.store_exact(board.st^.hashkey, EGTB_DRAFT, score, board.ply, NOMOVE);
        exit;
      end;
    end
    else begin
      if alpha >= -SCORE_WIN then begin
        tt.store_upper(board.st^.hashkey, EGTB_DRAFT, score, board.ply);
        exit;
      end
      else begin
        gtb_probe_dtm(board, PROBE_HARD, wdl, dtm);
        score := mated_in(board.ply + dtm);
        tt.store_exact(board.st^.hashkey, EGTB_DRAFT, score, board.ply, NOMOVE);
        exit;
      end;
    end;
  end;
  result := false;
end;

procedure init_margins();
const
  DELTA_MARGIN_P = $080;
  DELTA_MARGIN_M = $100;
  DELTA_MARGIN_R = $0cc;
var
  ph, i: integer;
begin
  for ph := STAGE_ENDGAME to STAGE_OPENING do
    with mtrl_values[ph], delta_margins[ph] do begin

      pawn_margin  := value_p + DELTA_MARGIN_P;
      pawn_inc     := value_p + DELTA_MARGIN_P div 2;

      minor_margin := value_b + DELTA_MARGIN_M;
      minor_inc    := value_b + DELTA_MARGIN_M div 2;

      rook_margin  := value_r + DELTA_MARGIN_R;
      rook_inc     := value_r + DELTA_MARGIN_R div 2;
    end;

  for i := 0 to 575 do
    TSearchBase.null_r[i] := trunc(sqrt(i) * 20);
end;


function perft(var b: TBoard; depth: integer): uint64;
var
  num_moves, i: integer;
  move: TMove;
  ml: TMoveList;
  u: TStateInfo;
begin
  if depth = 1 then begin
    result := gen_legal_moves(b, ml);
    exit;
  end;

  result := 0;
  num_moves := gen_all(b, ml);

  for i := 0 to num_moves-1 do begin
    move := ml[i].move;
    if (b.st^.checkersBB <> 0) or b.is_legal(move) then begin
      b.make_move(move, u);
      b.update_state();
      inc(result, perft(b, depth - 1));
      b.undo_move(move);
    end;
  end
end;

function divide(var b: TBoard; depth: integer): uint64;
var
  move: TMove;
  num_moves, i: integer;
  nodes: uint64;
  ml: TMoveList;
  u: TStateInfo;
  c: TMoveString;
begin
  result := 0;
  num_moves := gen_legal_moves(b, ml);
  nodes := 1;
  for i := 0 to num_moves-1 do begin
    move := ml[i].move;
    b.make_move(move, u);
    b.update_state();
    if depth > 1 then
      nodes := perft(b, depth - 1);
    b.undo_move(move);
    inc(result, nodes);
    printf('0x%08x: %s = %'+fmt64+'u' + LineEnding, move, move_to_san(move, b, c), nodes);
  end;
end;

end.

