
unit uMovepick;

interface

uses
  uBitboard, uBoard, uhistory, uMove, uSearchDef;

{$include platform.inc}

type
  TGenPhase = (
    S1_HASH, S1_GEN_CAP, S1_GOOD_CAP,
    S1_KILL_A, S1_KILL_B, S1_GEN_QUIET, S1_QUIET, S1_LOSING_CAP,
    S2_HASH, S2_GEN_CAP, S2_CAPTURES, S2_GEN_CHECKS, S2_CHECKS,
    S3_EVASIONS_HASH, S3_GEN_EVASIONS, S3_EVASIONS,
    S4_EVASIONS_NO_HASH, S4_EVASIONS
  );

  PMovePick = ^TMovePick;
  TMovePick = object
    board: PBoard;
    phase: TGenPhase;
    hash_move: TMove;
    target: TBitboard;
    num_moves: integer;
    procedure init(const b: TBoard; sstack: PSS; seq: TGenPhase; hashmove: TMove);
    procedure init_evasions(const b: TBoard; sstack: PSS; hashmove: TMove);
    function get_next(): TMove;
    function smp_get_next(): TMove;
  private
    move_ptr: PMoveStack;
    bad_ptr: PMoveStack;
    ss: PSS;
    ml: TMoveList;
  end;

function pick_move(mptr: PMoveStack): TMove;

procedure sort_moves(var ml: TMoveList);

implementation

uses
  uMovegen, uPiece, uSee;

procedure TMovePick.init(const b: TBoard; sstack: PSS; seq: TGenPhase; hashmove: TMove);
begin
  board := @b;
  ss := sstack;
  phase := seq;
  hash_move := hashmove;
end;

function TMovePick.smp_get_next(): TMove;
begin
  result := NOMOVE;
  if phase <> TGenPhase(0) then begin
    result := get_next();
    if result = NOMOVE then
      phase := TGenPhase(0);
  end;
end;

function TMovePick.get_next(): TMove;
var
  move: TMove;
begin
  while true do begin

    case phase of
      S1_HASH:
        begin
          inc(phase);
          if hash_move <> NOMOVE then
            if board^.hm_is_legal(hash_move) then begin
              result := hash_move;
              exit;
            end;
        end;

      S1_GEN_CAP:
        begin
          inc(phase);
          gen_captures(board^, @ml, board^.bb.pieces[board^.they]);
          sort_moves(ml);
          move_ptr := @ml;
          bad_ptr := @ml[MAX_MOVES-1];
        end;

      S1_GOOD_CAP:
        while true do begin
          move := move_ptr^.move;
          if move = NOMOVE then begin
            bad_ptr^.move64 := 0;
            inc(phase);
            break;
          end;
          inc(move_ptr);

          if move = hash_move then
            continue;

          if is_king_move(move) then begin
            result := move;
            exit;
          end;

          if not board^.is_legal(move) then
            continue;

          if (board^.st^.pins[board^.us] and SquareBB[source_sq(move)] = 0) then
            if bad_see(board^, move) then begin
              bad_ptr^.move := move;
              dec(bad_ptr);
              continue;
            end;

          result := move;
          exit;
        end;

      S1_KILL_A:
        begin
          inc(phase);
          move := ss^.killer_a;
          if (move <> NOMOVE) and (move <> hash_move) then
            if board^.hm_is_legal(move) then begin
              result := move;
              exit;
            end;
          inc(phase);
          move := ss^.killer_b;
          if (move <> NOMOVE) and (move <> hash_move) then
            if board^.hm_is_legal(move) then begin
              result := move;
              exit;
            end;
        end;

      S1_KILL_B:
        begin
          inc(phase);
          move := ss^.killer_b;
          if (move <> NOMOVE) and (move <> hash_move) then
            if (move <> ss^.killer_a) and board^.hm_is_legal(move) then begin
              result := move;
              exit;
            end;
        end;

      S1_GEN_QUIET:
        begin
          inc(phase);
          move_ptr := @ml;
          gen_quiet(board^, @ml);
          sort_moves(ml);
        end;

      S1_QUIET:
        while true do begin
          move := move_ptr^.move;
          if move = NOMOVE then begin
            move_ptr := @ml[MAX_MOVES-1];
            inc(phase);
            break;
          end;
          inc(move_ptr);
          with ss^ do
            if (move <> hash_move) and (move <> killer_a) and (move <> killer_b) then
              if board^.is_legal(move) then begin
                result := move;
                exit;
              end;
        end;

      S1_LOSING_CAP:
        while true do begin
          result := move_ptr^.move;
          if result <> NOMOVE then begin
            dec(move_ptr);
            if (result <> hash_move) and board^.is_legal(result) then exit;
          end
          else
            exit;
        end;

      S2_HASH:
        begin
          inc(phase);
          if hash_move <> NOMOVE then
            if board^.hm_is_legal(hash_move) then begin
              result := hash_move;
              exit;
            end;
        end;

      S2_GEN_CAP:
        begin
          inc(phase);
          move_ptr := @ml;
          gen_captures(board^, @ml, target);
        end;

      S2_CAPTURES:
        while true do begin
          move := pick_move(move_ptr);
          inc(move_ptr);
          if move <> NOMOVE then begin
            if (move <> hash_move) and board^.is_legal(move) then begin
              result := move;
              exit;
            end;
          end
          else begin
            inc(phase);
            break;
          end;
        end;

      S2_GEN_CHECKS:
        begin
          inc(phase);
          move_ptr := @ml;
          gen_checks(board^, @ml, target);
        end;

      S2_CHECKS:
        while true do begin
          result := move_ptr^.move;
          inc(move_ptr);
          if result = NOMOVE then exit;
          if (result <> hash_move) and board^.is_legal(result) then exit;
        end;

      S3_EVASIONS_HASH:
        begin
          inc(phase);
          move := hash_move;
          if (move <> NOMOVE) and board^.hm_is_legal_evasion(move) then begin
            result := move;
            exit;
          end;
        end;

      S3_GEN_EVASIONS:
        begin
          inc(phase);
          move_ptr := @ml;
          num_moves := gen_evasions(board^, @ml, TBB(BB_Full));
          while move_ptr^.move <> NOMOVE do begin
            with move_ptr^ do begin
              if is_capture(move) then
                score := 9*MAX_HIST + MVV_LVA[piece_moved(move), capture_victim(move)]
              else if move = ss^.killer_a then
                score := 8*MAX_HIST + 1
              else if move = ss^.killer_b then
                score := 8*MAX_HIST
              else
                score := EV_SORT[piece_moved(move)]*MAX_HIST + hist.get_score(move);
            end;
            inc(move_ptr);
          end;
          move_ptr := @ml;
          sort_moves(ml);
        end;

      S3_EVASIONS:
        begin
          repeat
            result := move_ptr^.move;
            inc(move_ptr);
            if result <> hash_move then exit;
          until result = NOMOVE;
          exit;
        end;

      S4_EVASIONS:
        begin
          result := move_ptr^.move;
          inc(move_ptr);
          exit;
        end;
    end;
  end;
end;

procedure TMovePick.init_evasions(const b: TBoard; sstack: PSS; hashmove: TMove);
begin
  board := @b;
  ss := sstack;
  move_ptr := @ml;
  hash_move := hashmove;
  num_moves := gen_evasions(board^, @ml, TBB(BB_Full));
  while move_ptr^.move <> NOMOVE do begin
    with move_ptr^ do begin
      if move = hash_move then
        score := 10*MAX_HIST
      else if is_capture(move) then
        score := 9*MAX_HIST + MVV_LVA[piece_moved(move), capture_victim(move)]
      else if move = ss^.killer_a then
        score := 8*MAX_HIST + 1
      else if move = ss^.killer_b then
        score := 8*MAX_HIST
      else
        score := EV_SORT[piece_moved(move)]*MAX_HIST + hist.get_score(move);
    end;
    inc(move_ptr);
  end;
  move_ptr := @ml;
  sort_moves(ml);
  phase := S4_EVASIONS;
end;

procedure sort_moves(var ml: TMoveList);
var
  tmp: uint64;
  i, j: integer;
begin
  i := 0;
  while ml[i].move64 <> 0 do begin
    tmp := ml[i].move64;
    j := i - 1;
    while (j >= 0) and (ml[j].move64 < tmp) do begin
      ml[j + 1] := ml[j];
      dec(j);
    end;
    ml[j + 1].move64 := tmp;
    inc(i);
  end;
end;

function pick_move(mptr: PMoveStack): TMove;
var
  best, tmp: uint64;
begin
  best := mptr^.move64;
  while mptr^.move64 <> 0 do begin
    if mptr^.move64 > best then begin
      tmp := mptr^.move64;
      mptr^.move64 := best;
      best := tmp;
    end;
    inc(mptr);
  end;
  result := TMove(integer(best) and $ffffffff);
end;

end.
