
unit uBook;

interface

uses
  uBitboard, uBoard, uMove, uPiece, uSquare, uZobrist;

{$include platform.inc}

const
  default_book: PAnsiChar = 'book.cbk';

  MAX_WEIGHT = 32768;

type
  PBookEntry = ^TBookEntry;
  TBookEntry = record
    key: TKey;
    move: TMove;
    weight, value: word;
  end;
  TBookFile = file of TBookEntry;

  TBook = class
    name: array [0..255] of AnsiChar;
    function init(book_name: PAnsiChar): boolean;
    function probe(const b: TBoard): TMove;
    function read_entry(out entry: TBookEntry): boolean; overload;
    function get_moves(const b: TBoard; out ml: TMoveList): integer;
    function get_key(): TKey; virtual; abstract;
    procedure convert(var entry: TBookEntry); virtual; abstract;
    procedure close();
    class var use_book: boolean;
    class var book_available: boolean;
    class var out_of_book: integer;
  private
    num_entries: integer;
    bookf: TBookFile;
    board: TBoard;
    function seek(idx: integer): boolean;
    function read_entry(idx: integer; out entry: TBookEntry): boolean; overload;
  end;

  TNativeBook = class(TBook)
    function get_key(): TKey; override;
    procedure convert(var entry: TBookEntry); override;
  end;

  TPolyglotBook = class(TBook)
    function get_key(): TKey; override;
    procedure convert(var entry: TBookEntry); override;
  end;

  TBookFactory = object
    num_entries: integer;
    procedure init();
    procedure update(const entry: TBookEntry);
    procedure save(book_name: PAnsiChar; min_games, min_score: integer; uniform: boolean);
    procedure destroy();
  private
    book: array of TBookEntry;
    capacity: integer;
    function insert(const entry: TBookEntry; idx: integer): integer;
    function find_first(key: TKey; out idx: integer): boolean;
  end;

  TPgnParser = object
    procedure init(var pgn_file: TextFile);
    procedure destroy();
    function next_game(out winner: integer): boolean;
    function next_move(var board: TBoard; out move: TMove): boolean;
  private
    f: ^TextFile;
    line_no: integer;
    game_text: AnsiString;
    gptr: PAnsiChar;
  end;

function book_init(book_name: PAnsiChar; test_board: TBoard): boolean;
procedure book_merge(src1, src2, book_name: PAnsiChar);
procedure book_create(pgn_name, book_name: PAnsiChar;
  max_ply, min_games, min_score, color_filter: integer; uniform: boolean);

var
  book: TBook = nil;

implementation

uses
  uNotation, uUtil, uSystem;

{$include polyglot.inc}

{ TBook }

function TBook.init(book_name: PAnsiChar): boolean;
var
  entry: TBookEntry;
  prev_key: TKey;
  i: integer;
begin
  result := false;
  book_available := false;
  strncpy(name, book_name, 255);
  AssignFile(bookf, ansistring(name));
  {$I-} Reset(bookf); {$I+}
  if IOResult <> 0 then exit;
  num_entries := FileSize(bookf);
  if num_entries < 2 then begin
    CloseFile(bookf);
    exit;
  end;
  read_entry(0, entry);
  prev_key := entry.key;
  for i := 1 to min(63, num_entries) do begin
    read_entry(entry);
    if entry.key < prev_key then begin
      CloseFile(bookf);
      exit;
    end;
  end;
  seek(0);
  book_available := true;
  result := true;
end;

procedure TBook.close();
begin
  {$I-} CloseFile(bookf); IOResult(); {$I+}
  book_available := false;
  name[0] := #0;
end;

function TBook.get_moves(const b: TBoard; out ml: TMoveList): integer;
var
  hi, lo, i: integer;
  hash_key: TKey;
  entry: TBookEntry;
begin
  result := 0;
  board := b;
  hash_key := get_key();
  hi := num_entries - 1;
  lo := 0;
  while (lo < hi) do begin
    i := (lo + hi) div 2;
    read_entry(i, entry);
    if entry.key = hash_key then begin
      while (i > 0) and (entry.key = hash_key) do begin
        dec(i);
        seek(i);
        read_entry(entry);
      end;
      while not eof(bookf) do begin
        read_entry(entry);
        if entry.key <> hash_key then break;
        if board.hm_is_ok(entry.move) then begin
          ml[result].move := entry.move;
          ml[result].score := entry.weight;
          inc(result);
        end;
      end;
      ml[result].move64 := 0;
      exit;
    end;
    if hash_key <= entry.key then
      hi := i
    else
      lo := i + 1;
  end;
  ml[result].move64 := 0;
end;

function TBook.probe(const b: TBoard): TMove;
var
  ml: TMoveList;
  sum, rnd: double;
  num_moves, total_freq, i: integer;
begin
  result := NOMOVE;
  num_moves := get_moves(b, ml);
  if num_moves = 0 then exit;

  total_freq := 0;
  sum := 0;
  rnd := random(32768) / 32768;

  for i := 0 to num_moves - 1 do
    inc(total_freq, ml[i].score);

  if total_freq = 0 then exit;

  for i := 0 to num_moves - 1 do begin
    sum := sum + ml[i].score / total_freq;
    if rnd < sum then begin
      result := ml[i].move;
      exit;
    end;
  end;

  result := ml[num_moves-1].move;
end;

function TBook.read_entry(idx: integer; out entry: TBookEntry): boolean;
begin
  if seek(idx) and not eof(bookf) then begin
    {$I-} read(bookf, entry); {$I+}
    if IOResult = 0 then begin
      convert(entry);
      result := true;
      exit;
    end;
  end;
  result := false;
end;

function TBook.read_entry(out entry: TBookEntry): boolean;
begin
  if not eof(bookf) then begin
    {$I-} read(bookf, entry); {$I+}
    if IOResult = 0 then begin
      convert(entry);
      result := true;
      exit;
    end;
  end;
  result := false;
end;

function TBook.seek(idx: integer): boolean;
begin
  if idx < num_entries then begin
    {$I-} system.seek(bookf, idx); {$I+}
    result := IOResult = 0;
  end
  else
    result := false
end;

{ TBookFactory }

procedure TBookFactory.init();
begin
  num_entries := 0;
  capacity := 65536;
  SetLength(book, capacity);
end;

procedure TBookFactory.destroy();
begin
  book := nil;
end;

function TBookFactory.find_first(key: TKey; out idx: integer): boolean;
var
  hi, lo, i: integer;
begin
  hi := num_entries - 1;
  lo := 0;
  while lo < hi do begin
    i := (lo + hi) div 2;
    if key <= book[i].key then
      hi := i
    else
      lo := i + 1;
  end;
  if book[lo].key = key then begin
    while (lo >= 0) and (book[lo].key = key) do dec(lo);
    idx := lo + 1;
    result := true;
    exit;
  end;
  idx := hi;
  result := false;
end;

function TBookFactory.insert(const entry: TBookEntry; idx: integer): integer;
begin
  if capacity < num_entries + 1 then begin
    inc(capacity, 65536);
    SetLength(book, capacity);
  end;

  while (idx < num_entries) and (book[idx].key <= entry.key) do inc(idx);

  if idx < num_entries then
    move(book[idx], book[idx+1], (num_entries - idx) * sizeof(TBookEntry));

  move(entry, book[idx], sizeof(TBookEntry));
  inc(num_entries);
  result := idx;
end;

procedure TBookFactory.save(book_name: PAnsiChar; min_games, min_score: integer;
  uniform: boolean);
var
  pos_cnt, entry_cnt, errno, i: integer;
  score: double;
  prev_key: TKey;
  bookf: TBookFile;
begin
  AssignFile(bookf, AnsiString(book_name));
  {$I-} rewrite(bookf); {$I+}
  errno := IOResult;

  if errno <> 0 then begin
    printf('Error creating output file "%s", errno = %d' + LineEnding, book_name, errno);
    exit;
  end;

  printf('->%s', book_name);
  entry_cnt := 0;
  pos_cnt := 0;
  prev_key := not uint64(0);
  for i := 0 to num_entries - 1 do begin
    if (book[i].weight < min_games) or (book[i].value = 0) then continue;
    score := book[i].value / book[i].weight / 2;
    if score < (min_score / 100) then continue;
    inc(entry_cnt);
    if uniform then
      book[i].weight := 1
    else
      book[i].weight := book[i].value;
    book[i].value := 0;
    write(bookf, book[i]);
    if book[i].key <> prev_key then begin
      prev_key := book[i].key;
      inc(pos_cnt);
    end;
  end;
  CloseFile(bookf);

  printf(' [%d positions, %d entries, %d bytes total]' + LineEnding + LineEnding,
    pos_cnt, entry_cnt, entry_cnt * sizeof(TBookEntry));
end;

procedure TBookFactory.update(const entry: TBookEntry);
var
  i, idx: integer;
begin
  if num_entries = 0 then begin
    move(entry, book[0], sizeof(TBookEntry));
    inc(num_entries);
    exit;
  end;

  if not find_first(entry.key, idx) then begin
    insert(entry, idx);
    exit;
  end;

  i := idx;
  while (book[i].key = entry.key) and (book[i].move <> entry.move) do
    inc(i);

  if (book[i].key = entry.key) and (book[i].move = entry.move) then begin
    inc(book[i].weight, entry.weight);
    inc(book[i].value, entry.value);
    if book[i].weight > MAX_WEIGHT then begin
      i := idx;
      while book[i].key = entry.key do begin
        book[i].weight := book[i].weight div 2;
        book[i].value := book[i].value div 2;
        inc(i);
      end;
    end;
    exit;
  end;

  insert(entry, idx);
end;

procedure book_merge(src1, src2, book_name: PAnsiChar);
var
  b1, b2: boolean;
  skipped: integer;
  e1, e2: TBookEntry;
  book1, book2: TBook;
  bookf: TBookFile;
begin
  printf(LineEnding + '"%s" + "%s" -> "%s"' + LineEnding + LineEnding, src1, src2, book_name);

  book1 := TNativeBook.Create();
  if not book1.init(src1) then begin
    book1.Free();
    exit;
  end;
  book2 := TNativeBook.Create();
  if not book2.init(src2) then begin
    book1.close();
    book1.Free();
    book2.Free();
    exit;
  end;

  AssignFile(bookf, AnsiString(book_name));
  {$I-}
  Rewrite(bookf);
  {$I+}
  if IOResult <> 0 then
  begin
    printf('Error creating output file' + LineEnding);
    book1.close();
    book2.close();
    exit;
  end;

  skipped := 0;
  b1 := book1.read_entry(e1);
  b2 := book2.read_entry(e2);

  while b1 or b2 do begin
    if b1 and not b2 then begin
      write(bookf, e1);
      b1 := book1.read_entry(e1);
    end
    else
    if not b1 and b2 then begin
      write(bookf, e2);
      b2 := book2.read_entry(e2);
    end
    else
    if e1.key < e2.key then begin
      write(bookf, e1);
      b1 := book1.read_entry(e1);
    end
    else
    if e2.key < e1.key then begin
      write(bookf, e2);
      b2 := book2.read_entry(e2);
    end
    else begin
      b2 := book2.read_entry(e2);
      inc(skipped);
    end;
  end;
  book1.close(); book1.Free();
  book2.close(); book2.Free();
  CloseFile(bookf);
  if skipped <> 0 then printf('%d entries skipped', skipped);
  printf('... done' + LineEnding + LineEnding);
end;


procedure book_create(pgn_name, book_name: PAnsiChar;
  max_ply, min_games, min_score, color_filter: integer; uniform: boolean);
var
  last_output, now: time_t;
  move: TMove;
  num_games, ply_cnt, winner: integer;
  entry: TBookEntry;
  st: TStateInfo;
  pgn: TextFile;
  factory: TBookFactory;
  parser: TPgnParser;
  board: TBoard;

begin
  AssignFile(pgn, AnsiString(pgn_name));

  {$I-} Reset(pgn); {$I+}
  if IOResult <> 0 then begin
    printf('Error reading "%s"' + LineEnding, pgn_name);
    exit;
  end;

  printf('Input: "%s"   MaxPly: %d   MinGames: %d   MinScore: %d' + LineEnding,
    pgn_name, max_ply, min_games, min_score);

  last_output := get_time();
  entry.weight := 1;
  num_games := 0;
  parser.init(pgn);
  factory.init();

  while parser.next_game(winner) do begin
    inc(num_games);
    ply_cnt := 0;
    board.reset(@st);
    while parser.next_move(board, move) do begin
      if color_filter <> integer(board.they) then begin
        entry.key := st.hashkey;
        entry.move := move;
        entry.weight := 1;
        entry.value := winner + 1;
        factory.update(entry);
      end;
      board.make_move_fast(move, st);
      winner := -winner;
      inc(ply_cnt);
      if ply_cnt >= max_ply then break;
    end;

    now := get_time();
    if now - last_output > 100 then begin
      last_output := now;
      printf('Processing... %d entries from %d games',
        factory.num_entries, num_games);
    end;
  end;
  printf('Processing... %d entries from %d games',
    factory.num_entries, num_games);
  printf(LineEnding);

  parser.destroy();

  CloseFile(pgn);
  factory.save(book_name, min_games, min_score, uniform);
end;

{ TPgnParser }

procedure TPgnParser.init(var pgn_file: TextFile);
begin
  line_no := 0;
  f := @pgn_file;
end;

procedure TPgnParser.destroy();
begin
  game_text := '';
end;

function TPgnParser.next_game(out winner: integer): boolean;
var
  line_buf: AnsiString;
  cline, br: PAnsiChar;
begin
  readln(f^, line_buf);
  inc(line_no);

  cline := PAnsiChar(line_buf);

  while cline^ in [#9, ' '] do
    inc(cline);

  while (cline^ = '[') and not eof(f^) do begin
    br := strchr(cline, integer(']'));
    if br <> nil then begin
      br^ := #0;
      if strstr(@cline[1], 'Result') <> nil then begin
        br := strchr(@cline[1], integer('"'));
        if br <> nil then begin
          if strstr(br, '1-0') <> nil then
            winner := 1
          else if strstr(br, '0-1') <> nil then
            winner := -1
          else
            winner := 0;
        end;
      end;
      readln(f^, line_buf);
      inc(line_no);
      cline := PAnsiChar(line_buf);
      while cline^ in [#9, ' '] do
        inc(cline);
    end;
  end;

  if eof(f^) then begin
    result := false;
    exit;
  end;

  game_text := cline;
  while not eof(f^) do begin
    readln(f^, line_buf);
    inc(line_no);
    cline := PAnsiChar(line_buf);
    while cline^ in [#9, ' '] do
      inc(cline);
    if cline^ = '[' then break;
    game_text := game_text + cline + ' ';
  end;
  gptr := PAnsiChar(game_text);
  result := true;
end;

function TPgnParser.next_move(var board: TBoard; out move: TMove): boolean;
var
  cmove: array [0..255] of AnsiChar;
  dest: PAnsiChar;
begin
  while true do begin
    while gptr^ in [#9, ' '] do
      inc(gptr);
    if gptr^ = '{' then begin
      while not (gptr^ in [#0, '}']) do
        inc(gptr);
      continue;
    end;
    if gptr^ = #0 then begin
      move := NOMOVE;
      result := false;
      exit;
    end;
    dest := @cmove;
    while not (gptr^ in [#0, #9, ' ']) do begin
      dest^ := gptr^;
      inc(dest);
      inc(gptr);
    end;
    dest^ := #0;
    dest := strchr(cmove, integer('.'));
    if dest <> nil then
      dest := @dest[1]
    else
      dest := cmove;

    move := san_to_move(dest, board);
    if move = NOMOVE then continue;
    result := board.hm_is_ok(move);
    exit;
  end;
end;

{ TNativeBook }

procedure TNativeBook.convert(var entry: TBookEntry);
begin
end;

function TNativeBook.get_key(): TKey;
begin
  result := board.st^.hashkey;
end;

{ TPolyglotBook }

procedure TPolyglotBook.convert(var entry: TBookEntry);

(* https://www.lazarusforum.de/viewtopic.php?p=39513#p39513 *)
procedure ZeroMemory(Destination: Pointer; Length: SizeInt);
begin
  FillByte(Destination^, Length, 0);
end;

var
  tmp: array [0..15] of byte;
  pgmove: integer;
  from_sq, to_sq: TSquare;
  p, victim, promotion: TPiece;
  i: integer;
begin
  ZeroMemory(@tmp, SizeOf(tmp));
  move(entry, tmp, 16);
  entry.key := 0;
  for i := 0 to 7 do
    entry.key := TBB(entry.key shl 8) or tmp[i];
  entry.weight := tmp[10] shl 8 or tmp[11];
  pgmove := tmp[8] shl 8 or tmp[9];
  to_sq := TSquare(pgmove and $3f);
  from_sq := TSquare((pgmove shr 6) and $3f);
  p := board.squares[from_sq];
  victim := board.squares[to_sq];
  entry.move := mk_move(from_sq, to_sq, p, victim);

  if (type_of(p) = PAWN) then begin
    if (to_sq = board.st^.epsq) and (victim = NOPIECE) then
      entry.move := mk_ep(from_sq, to_sq, p, flip(p))
    else
      if (abs(integer(from_sq) - integer(to_sq)) > 9) then
        entry.move := mk_dp(from_sq, to_sq, p)
      else
      if (pgmove and (not MASK_FROMTO) <> 0) then begin
        promotion := piece(PgPromotion[pgmove shr 12], board.us);
        entry.move := mk_promotion(entry.move, promotion);
      end;
  end
  else if (type_of(p) = KING) then begin
    if victim = piece(ROOK, board.us) then
      entry.move := mk_castle(from_sq, to_sq, p);
  end;
end;

function TPolyglotBook.get_key(): TKey;
var
  b: TBitboard;
  sq: TSquare;
begin
  result := pg_key_castle(board.st^.castle_rights) xor pg_key_ep(board.st^.epsq)
    xor pg_key_stm(board.us);
  b := board.bb.occupied;
  while b <> 0 do begin
    sq := find_lsb(b);
    result := result xor pg_key_piece(board.squares[sq], sq);
    clear_lsb(b);
  end;
end;

function book_init(book_name: PAnsiChar; test_board: TBoard): boolean;
var
  f: TBookFile;
  name: array [0..255] of AnsiChar;
begin
  if assigned(book) then book.Close();
  TBook.book_available := false;
  result := false;
  if (book_name = nil) or (book_name[0] = #0) then exit;

  strncpy(name, book_name, 255);
  AssignFile(f, ansistring(name)); {$I-} Reset(f); {$I+}
  if IOResult <> 0 then begin
    strncpy(name, program_dir, 255);
    strncpy(name, book_name, 255 - strlen(name));
    AssignFile(f, ansistring(name)); {$I-} Reset(f); {$I+}
    if IOResult <> 0 then begin
      printf('Opening book "%s" not found' + LineEnding, book_name);
      strcpy(name, default_book);
      exit;
    end;
    CloseFile(f);
  end;

  printf('Opening book: "%s" - ', name);

  book := TNativeBook.Create();
  if book.init(name) then begin
    if book.probe(test_board) <> NOMOVE then begin
      printf('%d entries [%s]' + LineEnding, book.num_entries, PAnsiChar('native'));
      result := true;
      exit;
    end;
  end;
  book.Free();

  book := TPolyglotBook.Create();
  if book.init(name) then begin
    if book.probe(test_board) <> NOMOVE then begin
      printf('%d entries [%s]' + LineEnding, book.num_entries, PAnsiChar('polyglot'));
      result := true;
      exit;
    end;
  end;
  book.Free(); book := nil;

  printf('unknown book format' + LineEnding);
  TBook.book_available := false;
  TBook.out_of_book := 0;
  result := false;
end;

end.
