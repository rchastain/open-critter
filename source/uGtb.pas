
unit uGtb;

interface

uses
{$ifndef unix}
  Windows,
{$endif}
  uBitboard, uBoard, uMove, uPiece, uScore, uSquare;

{$include platform.inc}

type
  int = integer;

  TB_return_values = (
    tb_DRAW = 0,
    tb_WMATE = 1,
    tb_BMATE = 2,
    tb_FORBID = 3,
    tb_UNKNOWN = 7
  );

  TB_pieces = ( tb_NOPIECE, tb_PAWN, tb_KNIGHT, tb_BISHOP, tb_ROOK, tb_QUEEN, tb_KING );
  TB_compression_scheme = ( tb_UNCOMPRESSED, tb_CP1, tb_CP2, tb_CP3, tb_CP4 );
  TBProbeType = ( PROBE_SOFT, PROBE_HARD );

  TBProcInit = procedure(verbosity, compression_scheme: integer; paths: PAnsiChar); cdecl;
  TBProcVoid = procedure(); cdecl;

  TBProcDtm = function(
    stm, epsq, castles: cardinal;
    inp_wSQ, inp_bSQ: PDWORD;
    inp_wPC, inp_bPC: PBYTE;
    out tbinfo: integer;
    out plies: integer): integer; cdecl;

  TBProcWdl = function(
    stm, epsq, castles: cardinal;
    inp_wSQ, inp_bSQ: PDWORD;
    inp_wPC, inp_bPC: PBYTE;
    out tbinfo: integer): integer; cdecl;

  TBProcIsInitialized = function (): integer; cdecl;
  TBProcCacheInit = function (cache_mem, wdl_fraction: integer): integer; cdecl;
  TBProcPathsInit = function (): PAnsiChar; cdecl;
  TBProcPathsAdd = function (ps, newpath: PAnsiChar): PAnsiChar; cdecl;
  TBProcPathsDone = function (ps: PAnsiChar): PAnsiChar; cdecl;
  TBProcPathsGetMain = function (): PAnsiChar; cdecl;

  TB_STATS = record
    wdl_easy_hits: uint64;      { hits that were found in own wdl cache }
    wdl_hard_prob: uint64;      { hard probes to the wdl cache: if fail, they will go to HD }
    wdl_soft_prob: uint64;      { soft probes to the wdl cache: if fail, they won't go to HD }
    wdl_cachesize: cardinal;    { size allocated for wdl cache }
    wdl_occupancy: double;      { % of slots filled in wdl cache }
    dtm_easy_hits: uint64;      { hits that were found in own dtm cache }
    dtm_hard_prob: uint64;      { hard probes to the dtm cache: if fail, they will go to HD }
    dtm_soft_prob: uint64;      { soft probes to the dtm cache: if fail, they won't go to HD }
    dtm_cachesize: cardinal;    { size allocated for dtm cache }
    dtm_occupancy: double;      { % of slots filled in dtm cache }

    total_hits: uint64;         { succesful probes }
    memory_hits: uint64;        { succesful probes to memory }
    drive_hits: uint64;         { succesful probes to the Hard drive }
    drive_miss: uint64;         { failing   probes to the Hard drive }
    bytes_read: uint64;         { bytes read from Hard drive }
    files_opened: cardinal;     { number of files newly opened }
    memory_efficiency: double;  { % hits from memory over total hits }
  end;

  TBProcStatsGet = procedure (out stats: TB_STATS); stdcall;

const
  tb_NOCASTLE = 0;
  tb_BOOO = 1;
  tb_BOO  = 2;
  tb_WOOO = 4;
  tb_WOO  = 8;
  tb_NOSQUARE = 64;

{$ifdef cpu64}
  GTB_DLL_NAME: PAnsiChar = 'gtb64.dll';
{$else}
  GTB_DLL_NAME: PAnsiChar = 'gtb32.dll';
{$endif}

function gtb_probe_wdl(var board: TBoard; pt: TBProbeType; out wdl: integer): boolean;
function gtb_probe_dtm(var board: TBoard; pt: TBProbeType; out wdl, dtm: integer): boolean;
function gtb_best_move(var board: TBoard): TMove;

procedure gtb_load();
procedure gtb_unload();
procedure gtb_print_stats();

procedure init_gtb();
procedure destroy_gtb();

var
  gtb_available: boolean;
  gtb_num_pieces: integer;
  gtb_path: array [0..1023] of AnsiChar;
  gtb_cache: integer = 32;
  gtb_compression: TB_compression_scheme = tb_CP4;
  paths: PAnsiChar;
  tbcache_flush: TBProcVoid;

implementation

uses
  uMovegen, uUtil, uSystem;

const
  piece_tab: array [TPiece] of TB_pieces = (
    tb_NOPIECE, tb_NOPIECE,
    tb_PAWN, tb_PAWN,
    tb_KING, tb_KING,
    tb_KNIGHT, tb_KNIGHT,
    tb_BISHOP, tb_BISHOP,
    tb_ROOK, tb_ROOK,
    tb_QUEEN, tb_QUEEN,
    tb_NOPIECE, tb_NOPIECE
  );

var
{$ifndef unix}
  hGtbDll: HMODULE;
{$endif}
  tb_init, tb_restart: TBProcInit;
  tb_done: TBProcVoid;
  tb_probe_hard, tb_probe_soft: TBProcDtm;
  tb_probe_wdl_hard, tb_probe_wdl_soft: TBProcWdl;
  tb_is_initialized: TBProcIsInitialized;
  tbcache_init: TBProcCacheInit;
  tbcache_done: TBProcVoid;
  tbcache_is_on: TBProcIsInitialized;
  tbstats_get: TBProcStatsGet;
  tbstats_reset: TBProcVoid;
  tbpaths_init: TBProcPathsInit;
  tbpaths_add: TBProcPathsAdd;
  tbpaths_done: TBProcPathsDone;
  tbpaths_getmain: TBProcPathsGetMain;

  castle_tab: array [0..16] of integer;

{$ifndef unix}
function dll_entry(entry_name: PAnsiChar; out p: pointer): boolean;
var
  proc: pointer;
begin
  proc := GetProcAddress(hGtbDll, entry_name);
  if proc = nil then begin
    printf('Entry point %s not found!' + LineEnding + LineEnding, entry_name);
    result := false;
    exit;
  end;
  p := proc;
  result := true;
end;
{$endif}

function gtb_dll_init(): boolean;
{$ifndef unix}
var
  dll_path: array [0..255] of AnsiChar;
{$endif}
begin
  tb_init := nil;
  tb_done := nil;
  tb_probe_hard := nil;
  tb_probe_soft := nil;
  tb_probe_wdl_hard := nil;
  tb_probe_wdl_soft := nil;
  tb_is_initialized := nil;
  tbcache_init := nil;
  tbcache_done := nil;
  tbstats_get := nil;
  tbpaths_init := nil;
  tbpaths_add := nil;
  tbpaths_done := nil;
{$ifdef unix}
  result := false;
{$else}
  hGtbDll := LoadLibraryA(GTB_DLL_NAME);
  if hGtbDll = 0 then begin
    printf('GTB dll not found (%d)' + LineEnding + LineEnding, GetLastError());
    result := false;
    exit;
  end;
  GetModuleFileNameA(hGtbDll, dll_path, sizeof(dll_path));
  printf('GTB DLL' + LineEnding'  %s' + LineEnding, dll_path);
  result :=
    dll_entry('tb_init', pointer(tb_init)) and
    dll_entry('tb_restart', pointer(tb_restart)) and
    dll_entry('tb_done', pointer(tb_done)) and
    dll_entry('tb_probe_soft', pointer(tb_probe_soft)) and
    dll_entry('tb_probe_hard', pointer(tb_probe_hard)) and
    dll_entry('tb_probe_WDL_soft', pointer(tb_probe_wdl_soft)) and
    dll_entry('tb_probe_WDL_hard', pointer(tb_probe_wdl_hard)) and
    dll_entry('tb_is_initialized', pointer(tb_is_initialized)) and
    dll_entry('tbcache_init', pointer(tbcache_init)) and
    dll_entry('tbcache_done', pointer(tbcache_done)) and
    dll_entry('tbcache_is_on', pointer(tbcache_is_on)) and
    dll_entry('tbcache_flush', pointer(tbcache_flush)) and
    dll_entry('tbstats_get', pointer(tbstats_get)) and
    dll_entry('tbstats_reset', pointer(tbstats_reset)) and
    dll_entry('tbpaths_init', pointer(tbpaths_init)) and
    dll_entry('tbpaths_add', pointer(tbpaths_add)) and
    dll_entry('tbpaths_done', pointer(tbpaths_done)) and
    dll_entry('tbpaths_getmain', pointer(tbpaths_getmain));
{$endif}
end;

procedure init_gtb();
var
  i, tbc: integer;
begin
  for i := 0 to 15 do begin
    tbc := tb_NOCASTLE;
    if i and CR_W_SHORT <> 0 then tbc := tbc or tb_WOO;
    if i and CR_W_LONG <> 0 then tbc := tbc or tb_WOOO;
    if i and CR_B_SHORT <> 0 then tbc := tbc or tb_BOO;
    if i and CR_B_LONG <> 0 then tbc := tbc or tb_BOOO;
    castle_tab[i] := tbc;
  end;
  strcpy(gtb_path, '\gtb');
  gtb_available := gtb_dll_init();
  if not gtb_available then
    exit;
  gtb_load();
end;

procedure destroy_gtb();
begin
  if gtb_available then
  begin
    gtb_unload();
    {$ifndef unix}
    FreeLibrary(hGtbDll);
    {$endif}
  end;
end;

function gtb_get_num_pieces(): integer;
var
  wdl, probe: integer;
  ws, bs: array [0..3] of integer;
  wp, bp: array [0..3] of byte;
begin
  wp[0] := ord(tb_KING); wp[1] := ord(tb_ROOK); wp[2] := ord(tb_PAWN); wp[3] := 0;
  bp[0] := ord(tb_KING); bp[1] := ord(tb_ROOK); bp[2] := 0;
  ws[0] := ord(A1); ws[1] := ord(B1); ws[2] := ord(A2); ws[3] := tb_NOSQUARE;
  bs[0] := ord(H1); bs[1] := ord(G1); bs[2] := tb_NOSQUARE;
  probe := tb_probe_wdl_hard(0, tb_NOSQUARE, 0, @ws, @bs, @wp, @bp, wdl);
  if probe <> 0 then begin
    result := 5;
    exit;
  end;
  ws[2] := tb_NOSQUARE; wp[2] := 0;
  probe := tb_probe_wdl_hard(0, tb_NOSQUARE, 0, @ws, @bs, @wp, @bp, wdl);
  if probe <> 0 then begin
    result := 4;
    exit;
  end;
  ws[1] := tb_NOSQUARE; wp[1] := 0;
  probe := tb_probe_wdl_hard(0, tb_NOSQUARE, 0, @ws, @bs, @wp, @bp, wdl);
  if probe <> 0 then begin
    result := 3;
    exit;
  end;
  result := 0;
end;

function gtb_probe_wdl(var board: TBoard; pt: TBProbeType; out wdl: integer): boolean;
var
  b: TBitboard;
  i: integer;
  sq: TSquare;
  stm, epsquare, castling: integer;
  ws, bs: array [0..16] of integer;
  wp, bp: array [0..16] of byte;
begin
  if not gtb_available then begin
    result := false;
    exit;
  end;

  stm := integer(board.us);
  castling := castle_tab[board.st^.castle_rights];
  epsquare := integer(board.st^.epsq);
  if epsquare = 0 then
    epsquare := tb_NOSQUARE;

  i := 0; b := board.bb.white;
  while b <> 0 do begin
    sq := find_lsb(b);
    ws[i] := integer(sq);
    wp[i] := byte(piece_tab[board.squares[sq]]);
    inc(i); clear_lsb(b);
  end;
  ws[i] := integer(tb_NOSQUARE);
  wp[i] := byte(tb_NOPIECE);

  i := 0; b := board.bb.black;
  while b <> 0 do begin
    sq := find_lsb(b);
    bs[i] := integer(sq);
    bp[i] := byte(piece_tab[board.squares[sq]]);
    inc(i); clear_lsb(b);
  end;
  bs[i] := integer(tb_NOSQUARE);
  bp[i] := byte(tb_NOPIECE);

  if pt = PROBE_HARD then
    result := boolean(tb_probe_wdl_hard(stm, epsquare, castling,
      @ws, @bs, @wp, @bp, wdl))
  else
    result := boolean(tb_probe_wdl_soft(stm, epsquare, castling,
      @ws, @bs, @wp, @bp, wdl))
end;

function gtb_probe_dtm(var board: TBoard; pt: TBProbeType; out wdl, dtm: integer): boolean;
var
  b: TBitboard;
  i: integer;
  sq: TSquare;
  stm, epsquare, castling: integer;
  ws, bs: array [0..16] of integer;
  wp, bp: array [0..16] of byte;
begin
  if not gtb_available then begin
    result := false;
    exit;
  end;

  stm := integer(board.us);
  epsquare := integer(board.st^.epsq);
  castling := castle_tab[board.st^.castle_rights];

  i := 0; b := board.bb.white;
  while b <> 0 do begin
    sq := find_lsb(b);
    ws[i] := integer(sq);
    wp[i] := byte(piece_tab[board.squares[sq]]);
    inc(i); clear_lsb(b);
  end;
  ws[i] := integer(tb_NOSQUARE);
  wp[i] := byte(tb_NOPIECE);

  i := 0; b := board.bb.black;
  while b <> 0 do begin
    sq := find_lsb(b);
    bs[i] := integer(sq);
    bp[i] := byte(piece_tab[board.squares[sq]]);
    inc(i); clear_lsb(b);
  end;
  bs[i] := integer(tb_NOSQUARE);
  bp[i] := byte(tb_NOPIECE);

  if pt = PROBE_HARD then
    result := boolean(tb_probe_hard(stm, epsquare, castling,
      @ws, @bs, @wp, @bp, wdl, dtm))
  else
    result := boolean(tb_probe_soft(stm, epsquare, castling,
      @ws, @bs, @wp, @bp, wdl, dtm))
end;

function gtb_best_move(var board: TBoard): TMove;
var
  num_moves, i: integer;
  wdl, dtm: integer;
  score, best_score: TScore;
  move, best_move: TMove;
  u: TStateInfo;
  ml: TMoveList;
begin
  if not gtb_available then begin
    result := NOMOVE;
    exit;
  end;

  num_moves := gen_legal_moves(board, ml);
  best_score := -SCORE_INF;
  best_move := NOMOVE;

  for i := 0 to num_moves - 1 do begin
    move := ml[i].move;
    board.make_move_fast(move, u);
    score := -SCORE_INF;
    if gtb_probe_dtm(board, PROBE_HARD, wdl, dtm) then begin
      case wdl of
        integer(tb_DRAW): score := SCORE_ZERO;
        integer(tb_WMATE): score := (-SCORE_MATE + int(dtm) + 1) * sign[board.us];
        integer(tb_BMATE): score := ( SCORE_MATE - int(dtm) - 1) * sign[board.us];
      end;
    end;
    board.undo_move(move);
    if score > best_score then begin
      best_move := move;
      best_score := score;
    end;
  end;

  result := best_move;
end;

procedure gtb_load();
var
  cache_size: cardinal;
  parser: TParser;
  p: PAnsiChar;
begin
  cache_size := gtb_cache * 1024 * 1024;
  paths := tbpaths_init();
  parser.init(gtb_path, ';');
  while not parser.eof() do begin
    p := parser.get_next();
    paths := tbpaths_add(paths, p);
  end;
  tb_init(1, integer(gtb_compression), paths);
  tbcache_init(cache_size, 96);
  if tb_is_initialized() <> 0 then
    printf('GTB Init OK' + LineEnding + LineEnding)
  else
    printf('GTB Init FAILED' + LineEnding + LineEnding);
  gtb_num_pieces := gtb_get_num_pieces();
end;

procedure gtb_unload();
begin
  tbcache_done();
  tb_done();
  paths := tbpaths_done(paths);
end;

procedure gtb_print_stats();
var
  stats: TB_STATS;
begin
  tbstats_get(stats);
  printf(' dtm hard probes: %d' + LineEnding + ' dtm soft probes: %d' + LineEnding + ' dtm easy hits %d' + LineEnding,
    int(stats.dtm_hard_prob), int(stats.dtm_soft_prob), int(stats.dtm_easy_hits));
  printf(' wdl hard probes: %d' + LineEnding + ' wdl soft probes: %d' + LineEnding + ' wdl easy hits %d' + LineEnding,
    int(stats.wdl_hard_prob), int(stats.wdl_soft_prob), int(stats.dtm_easy_hits));
  printf(' total hits: %d' + LineEnding + ' memory hits: %d' + LineEnding + ' memory eff: %3.2f%%' + LineEnding,
    int(stats.total_hits), int(stats.memory_hits), stats.memory_efficiency);
  printf(' files opened: %d' + LineEnding + ' bytes read: %d' + LineEnding,
    int(stats.files_opened), int(stats.bytes_read));
end;

end.
