
unit uTranstab;

interface

uses
  uBoard, uMove, uScore, uSquare, uZobrist;

{$include platform.inc}

const
  HASH_SLOTS = 4;
  MEGABYTE   = 1024 * 1024;
  MAX_SIZE   = 1024;

  LOWER_BOUND   = $01;
  UPPER_BOUND   = $02;
  BOUND_EXACT   = $04;
  HASH_NODE_ALL = $08;
  HASH_NODE_CUT = $10;
  HASH_NOMOVES  = $20;

type
  PHashEntry = ^THashEntry;

  THashEntry = packed record
    key32: TKey32;
    best_move: TMove;
    score_upper, score_lower: smallint;
    depth_upper, depth_lower: byte;
    age, flags: byte;
  end;

const
  BUCKET_SIZE = HASH_SLOTS * sizeof(THashEntry);
  MAX_ENTRIES = MAX_SIZE * MEGABYTE div sizeof(THashEntry);
  MAX_HASH_SIZE = {$ifdef cpu64}8192{$else}1024{$endif};

{$ifdef unix}
type
  size_t = ptrint;
{$endif}

type
  THashTable = array [0 .. MAX_ENTRIES - 1] of THashEntry;
  PHashTable = ^THashTable;

  TPvHashEntry = packed record
    key: TKey;
    best_move: TMove;
    score: smallint;
    depth, age: byte;
  end;

  TTransTable = object
    hash_mask: cardinal;
    generation: integer;
    table: PHashTable;
    num_buckets: cardinal;
    num_writes: cardinal;
    raw_mem: pointer;
    procedure init(size: integer = 64);
    procedure destroy();
    procedure set_size(size: integer);
    procedure clear();
    procedure new_search();
{$ifdef hasprefetch}
    procedure prefetch(key: TKey); inline;
{$endif}
    function hash_full(): integer;

    procedure store_upper(key: TKey; depth: integer; score: TScore;
      ply: integer; f: integer = 0);

    procedure store_upper_cut(key: TKey; depth: integer; score: TScore;
      ply: integer);

    procedure store_lower(key: TKey; depth: integer; score: TScore;
      ply: integer; move: TMove; f: integer = 0);

    procedure store_lower_all(key: TKey; depth: integer; score: TScore;
      ply: integer; move: TMove);

    procedure store_exact(key: TKey; depth: integer; score: TScore;
      ply: integer; move: TMove; f: integer = BOUND_EXACT);

    procedure store_pv(key: TKey; depth: integer; score: TScore; move: TMove);

    procedure extract_pv(var board: TBoard; pv: PMove; move: TMove;
      tb_extend: boolean);
  end;

var
  tt: TTransTable;

implementation

uses
  uBitboard, uGtb, uSystem;

var
  pv_hash: array [0 .. 65535] of TPvHashEntry;

function age_delta(entry_age, curr_gen: integer): integer; inline;
begin
  result := ((entry_age - curr_gen) and $FF) + 1;
end;

procedure break_on_write(key: TKey); inline;
begin
   {if key = $872e818b374975e6 then
    write('.');}
end;

procedure TTransTable.init(size: integer = 64);
begin
  table := nil;
  raw_mem := nil;
  num_buckets := 0;
  set_size(size);
end;

procedure TTransTable.destroy();
begin
  FreeMem(raw_mem);
  table := nil;
  raw_mem := nil;
end;

procedure TTransTable.clear();
var
  size: size_t;
begin
  size := num_buckets * BUCKET_SIZE;
  fillchar(table^[0], size, 0);
  fillchar(pv_hash[0], sizeof(pv_hash), 0);
  generation := 0;
end;

procedure TTransTable.set_size(size: integer);
var
  pow2, num_entries: cardinal;
  raw_size: size_t;
begin
  size := max(8, min(1024, size));
  pow2 := (size * MEGABYTE) div BUCKET_SIZE;
  pow2 := 1 shl integer(BSR(pow2));
  if pow2 <> num_buckets then
  begin
    num_buckets := pow2;
    raw_size := num_buckets * BUCKET_SIZE;
    if raw_mem <> nil then
      FreeMem(raw_mem);
    table := aligned_malloc(raw_size, raw_mem);
    clear();
    num_entries := num_buckets * HASH_SLOTS;
    hash_mask := num_entries - HASH_SLOTS;
    printf('Hash table: %9d entries of %d bytes = %4d MB' + LineEnding, num_entries,
      sizeof(THashEntry), num_entries * sizeof(THashEntry) div MEGABYTE);
  end;
end;

{$ifdef hasprefetch}
procedure TTransTable.prefetch(key: TKey); inline;
var
  h: PHashEntry;
begin
  h := @(table^[key and hash_mask]);
  system.prefetch(h^);
end;
{$endif}

procedure TTransTable.store_pv(key: TKey; depth: integer; score: TScore;
  move: TMove);
var
  h, replace: ^TPvHashEntry;
  i, curr, best: integer;
begin
  best := 0;
  h := @pv_hash[key and $FFF8];
  replace := h;
  for i := 0 to 2 * HASH_SLOTS - 1 do
  begin
    if h^.key = key then
    begin
      h^.best_move := move;
      h^.score := score;
      h^.depth := depth;
      h^.age := generation;
    end;
    curr := age_delta(h^.age, generation) shl 8 - h^.depth;
    if curr > best then
    begin
      best := curr;
      replace := h;
    end;
    inc(h);
  end;
  replace^.key := key;
  replace^.best_move := move;
  replace^.score := score;
  replace^.depth := depth;
  replace^.age := generation;
end;

procedure TTransTable.store_exact(key: TKey; depth: integer; score: TScore;
  ply: integer; move: TMove; f: integer = BOUND_EXACT);
var
  key_hi: TKey32;
  h, replace: PHashEntry;
  i, j, curr, best: integer;
begin
  score := score_to_tt(score, ply);
  break_on_write(key);
  store_pv(key, depth, score, move);
  assert(valid_score(score));
  best := 0;
  inc(num_writes);
  key_hi := key shr 32;
  h := @(table^[key and hash_mask]);
  replace := h;
  for i := 0 to HASH_SLOTS - 1 do begin
    if (h^.key32 = key_hi) and (h^.depth_upper <= depth) and
      (h^.depth_lower <= depth) then
    begin
      with h^ do begin
        best_move := move;
        score_upper := score;
        score_lower := score;
        depth_upper := depth;
        depth_lower := depth;
        flags := f;
        age := generation;
      end;
      for j := i + 1 to HASH_SLOTS - 1 do begin
        inc(h);
        if (h^.key32 = key_hi) and (h^.depth_upper <= depth) and
          (h^.depth_lower <= depth) then
        begin
          fillchar(h^, sizeof(THashEntry), 0);
          h^.age := h^.age xor $80;
        end;
      end;
      exit;
    end;
    curr := age_delta(h^.age, generation) shl 8 - max(h^.depth_upper, h^.depth_lower);
    if (curr > best) then begin
      best := curr;
      replace := h;
    end;
    inc(h);
  end;
  with replace^ do begin
    key32 := key_hi;
    best_move := move;
    score_upper := score;
    score_lower := score;
    depth_upper := depth;
    depth_lower := depth;
    flags := f;
    age := generation;
  end;
end;

procedure TTransTable.store_lower(key: TKey; depth: integer; score: TScore;
  ply: integer; move: TMove; f: integer = 0);
var
  key_hi: TKey32;
  h, replace: PHashEntry;
  i, curr, best: integer;
begin
  score := score_to_tt(score, ply);
  break_on_write(key);
  assert(valid_score(score));
  best := 0;
  inc(num_writes);
  key_hi := key shr 32;
  h := @(table^[key and hash_mask]);
  replace := h;
  for i := 0 to HASH_SLOTS - 1 do begin
    if (h^.key32 = key_hi) and (h^.flags and BOUND_EXACT = 0) and
      (h^.depth_lower <= depth) then
    begin
      with h^ do begin
        best_move := move;
        score_lower := score;
        depth_lower := depth;
        flags := (flags or f or LOWER_BOUND) and not HASH_NODE_ALL;
        age := generation;
      end;
      exit;
    end;
    curr := age_delta(h^.age, generation) shl 8 - max(h^.depth_upper, h^.depth_lower);
    if (curr > best) then begin
      best := curr;
      replace := h;
    end;
    inc(h);
  end;
  with replace^ do begin
    key32 := key_hi;
    best_move := move;
    score_upper := 0;
    score_lower := score;
    depth_upper := 0;
    depth_lower := depth;
    flags := LOWER_BOUND or f;
    age := generation;
  end;
end;

procedure TTransTable.store_lower_all(key: TKey; depth: integer; score: TScore;
  ply: integer; move: TMove);
var
  key_hi: TKey32;
  h, replace: PHashEntry;
  i, curr, best: integer;
begin
  score := score_to_tt(score, ply);
  break_on_write(key);
  assert(valid_score(score));
  best := 0;
  inc(num_writes);
  key_hi := key shr 32;
  h := @(table^[key and hash_mask]);
  replace := h;
  for i := 0 to HASH_SLOTS - 1 do begin
    if (h^.key32 = key_hi) and (h^.depth_lower <= depth) and
      ((h^.depth_lower = 0) or (h^.flags and HASH_NODE_ALL <> 0)) then
    begin
      with h^ do begin
        best_move := move;
        score_lower := score;
        depth_lower := depth;
        flags := flags or (LOWER_BOUND or HASH_NODE_ALL);
        age := generation;
      end;
      exit;
    end;
    curr := age_delta(h^.age, generation) shl 8 - max(h^.depth_upper,
      h^.depth_lower);
    if (curr > best) then begin
      best := curr;
      replace := h;
    end;
    inc(h);
  end;
  with replace^ do begin
    key32 := key_hi;
    best_move := move;
    score_upper := 0;
    score_lower := score;
    depth_upper := 0;
    depth_lower := depth;
    flags := LOWER_BOUND or HASH_NODE_ALL;
    age := generation;
  end;
end;

procedure TTransTable.store_upper(key: TKey; depth: integer; score: TScore;
  ply: integer; f: integer = 0);
var
  key_hi: TKey32;
  h, replace: PHashEntry;
  i, curr, best: integer;
begin
  score := score_to_tt(score, ply);
  break_on_write(key);
  assert(valid_score(score));
  best := 0;
  inc(num_writes);
  key_hi := key shr 32;
  h := @(table^[key and hash_mask]);
  replace := h;
  for i := 0 to HASH_SLOTS - 1 do begin
    if (h^.key32 = key_hi) and (h^.flags and BOUND_EXACT = 0) and
      (h^.depth_upper <= depth) then
    begin
      with h^ do begin
        score_upper := score;
        depth_upper := depth;
        flags := (flags or f or UPPER_BOUND) and not HASH_NODE_CUT;
        age := generation;
      end;
      exit;
    end;
    curr := age_delta(h^.age, generation) shl 8 - max(h^.depth_upper,
      h^.depth_lower);
    if (curr > best) then begin
      best := curr;
      replace := h;
    end;
    inc(h);
  end;
  with replace^ do begin
    key32 := key_hi;
    best_move := NOMOVE;
    score_lower := 0;
    score_upper := score;
    depth_lower := 0;
    depth_upper := depth;
    flags := UPPER_BOUND or f;
    age := generation;
  end;
end;

procedure TTransTable.store_upper_cut(key: TKey; depth: integer; score: TScore;
  ply: integer);
var
  key_hi: TKey32;
  h, replace: PHashEntry;
  i, curr, best: integer;
begin
  score := score_to_tt(score, ply);
  break_on_write(key);
  assert(valid_score(score));
  best := 0;
  inc(num_writes);
  key_hi := key shr 32;
  h := @(table^[key and hash_mask]);
  replace := h;
  for i := 0 to HASH_SLOTS - 1 do begin
    if (h^.key32 = key_hi) and (h^.depth_upper <= depth) and
      ((h^.depth_upper = 0) or (h^.flags and HASH_NODE_CUT <> 0)) then
    begin
      with h^ do begin
        score_upper := score;
        depth_upper := depth;
        flags := flags or (UPPER_BOUND or HASH_NODE_CUT);
        age := generation;
      end;
      exit;
    end;
    curr := age_delta(h^.age, generation) shl 8 - max(h^.depth_upper, h^.depth_lower);
    if (curr > best) then begin
      best := curr;
      replace := h;
    end;
    inc(h);
  end;
  with replace^ do
  begin
    key32 := key_hi;
    best_move := NOMOVE;
    score_lower := 0;
    score_upper := score;
    depth_lower := 0;
    depth_upper := depth;
    flags := UPPER_BOUND or HASH_NODE_CUT;
    age := generation;
  end;
end;

procedure TTransTable.new_search();
begin
  generation := (generation + 1) and $ff;
  num_writes := 0;
end;

function TTransTable.hash_full(): integer;
var
  samples: ^TKey;
  h: PHashEntry;
  i: integer;
begin
  result := 0;
  samples := @KeyPieces;
  for i := 0 to 999 do begin
    h := @(table^[samples^ and hash_mask]);
    if h^.age = generation then
      inc(result);
    inc(samples);
  end;
end;

procedure TTransTable.extract_pv(var board: TBoard; pv: PMove; move: TMove;
  tb_extend: boolean);
var
  pvlen, i, move_depth: integer;
  key32: TKey32;
  h: ^THashEntry;
  p: ^TPvHashEntry;
  old_state: PStateInfo;
  st: TStateInfo;
begin
  old_state := board.st;
  pvlen := 0;
  pv[0] := move;
  while move <> NOMOVE do begin
    board.make_move_fast(move, st);
    inc(pvlen);
    if st.flags and FLAG_REPETITION <> 0 then
      break;

    move := NOMOVE;
    p := @pv_hash[st.hashkey and $FFF8];
    for i := 0 to 2 * HASH_SLOTS - 1 do begin
      if p^.key = st.hashkey then begin
        move := p^.best_move;
        break;
      end;
      inc(p);
    end;

    if (move = NOMOVE) and tb_extend and (popcnt(board.bb.occupied) <= 5) then
      move := gtb_best_move(board);

    if (move = NOMOVE) then begin
      h := @table^[st.hashkey and hash_mask];
      key32 := st.hashkey shr 32;
      move_depth := 0;
      for i := 0 to HASH_SLOTS-1 do begin
        if (h^.key32 = key32) and (h^.best_move <> NOMOVE) then
          if h^.depth_lower > move_depth then begin
            move_depth := h^.depth_lower;
            move := h^.best_move;
          end;
      end;
    end;

    if (move = NOMOVE) or (pvlen >= 120) or not board.hm_is_ok(move) then
      break;

    pv[pvlen] := move;
  end;

  pv[pvlen] := NOMOVE;
  while (pvlen <> 0) do begin
    dec(pvlen);
    board.undo_move(pv[pvlen]);
  end;
  board.st := old_state;
end;

end.
