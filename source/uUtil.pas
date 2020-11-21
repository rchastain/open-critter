
unit uUtil;

interface

uses
  uBitboard, uBoard, uMove, uPiece, uScore, uSquare, uSystem;

{$include platform.inc}

type
  TTimeString = array [0..9] of AnsiChar;

  TParser = object
    line: PAnsiChar;
    len, pos: integer;
    delim: AnsiChar;
    procedure init(str: PAnsiChar; delimiter: AnsiChar);
    function eof(): boolean; inline;
    function get_next(): PAnsiChar;
  end;

procedure print_bb(b: TBitboard);
procedure print_board(var board: TBoard);
procedure print_psq(var board: TBoard);
procedure print_move_list(var board: TBoard; ml: PMoveStack);
procedure print_book_moves(var board: TBoard);
procedure print_probe_result(var board: TBoard);
procedure flip_board(var board: TBoard);

function stricmp(s1: PAnsiChar; s2: PAnsiChar): boolean;
function time_to_string(ms: cardinal; c: PAnsiChar): PAnsiChar;
function pv_to_string(pv: PMove; var board: TBoard; c: PAnsiChar): PAnsiChar;

var
  program_dir: array [0..255] of AnsiChar;

implementation

uses
  uBook, uEval, uFen, uGtb, uMaterial, uMovegen, uMovepick, uNotation, uPsq;

procedure TParser.init(str: PAnsiChar; delimiter: AnsiChar);
begin
  pos := 0;
  line := str;
  len := strlen(str);
  delim := delimiter;
end;

function TParser.eof(): boolean;
begin
  result := pos >= len;
end;

function TParser.get_next(): PAnsiChar;
var
  start: integer;
begin
  if eof() then
  begin
    result := line + len;
    exit;
  end;
  while (line[pos] <> #0) and (line[pos] = delim) do
    inc(pos);
  start := pos;
  while (line[pos] <> #0) and (line[pos] <> delim) and (line[pos] <> #10) do
    inc(pos);
  line[pos] := #0;
  inc(pos);
  result := line + start;
end;

procedure print_board(var board: TBoard);
var
  r: TRank; f: TFile;
  st: PStateInfo;
  sq: TSquare;
  c: array [0..9] of AnsiChar;
  p: TPiece;
  fen: TFenString;
begin
  printf('    A   B   C   D   E   F   G   H' + LineEnding);
  for r := RANK_8 downto RANK_1 do begin
    printf('  +---+---+---+---+---+---+---+---+' + LineEnding);
    printf('%c ', rank_to_char(r));
    for f := FILE_A to FILE_H do begin
      sq := square(r, f);
      p := board.squares[sq];
      printf('| ');
      if (p <> NOPIECE) and (side_of(p) = WHITE) then begin
        highlight_on;
        printf('%c', piece_to_char(p));
        highlight_off;
      end
      else
        printf('%c', piece_to_char(p));
      printf(' ');
    end;
    printf('|' + LineEnding);
  end;
  printf('  +---+---+---+---+---+---+---+---+' + LineEnding);
  printf('FEN: %s' + LineEnding, encode_fen(board, fen));
  st := board.st;
  printf('Hashkey: %016'+ fmt64 +'x mgpsq: %s' + LineEnding,
    st^.hashkey, score_to_string(mg(st^.psqval), c));
  printf('Pawnkey: %016' + fmt64 + 'x egpsq: %s' + LineEnding,
    st^.pawnkey, score_to_string(eg(st^.psqval), c));
  //printf('Mtrlkey: %08x' + LineEnding + LineEnding, st^.mtrlkey); // Runtime error 201
end;

procedure print_psq(var board: TBoard);
var
  w, b, w_total, b_total: TScorePair;
  pt: TPieceType;
  mi: PMtrlInfo;
begin
  mi := get_mtrl_info(board, evaluator^.mtrl_table^);
  w_total := 0; b_total := 0;
  printf('             | Score | White     Mg     Eg | Black     Mg     Eg' + LineEnding);
  printf('-------------+-------+---------------------+--------------------' + LineEnding);
  for pt := PAWN to QUEEN do begin
    w := get_psq_score(piece(pt, WHITE), board.bb.ptype[pt, WHITE]);
    b := get_psq_score(piece(pt, BLACK), board.bb.ptype[pt, BLACK]);
    inc(w_total, w);
    inc(b_total, b);
    print_score(mi^.phase, PieceNames[pt], w, b);
  end;
  printf('-------------+-------+---------------------+--------------------' + LineEnding);
  print_score(mi^.phase, 'total', w_total, b_total);
  printf(LineEnding);
end;

procedure print_move_list(var board: TBoard; ml: PMoveStack);
var
  cmove: TMoveString;
begin
  while ml^.move <> NOMOVE do begin
    printf('0x%08x: %s = %d' + LineEnding, ml^.move, move_to_san(ml^.move, board, cmove), ml^.score);
    inc(ml);
  end;
  printf(LineEnding);
end;

procedure print_book_moves(var board: TBoard);
var
  num_moves, i: integer;
  c: TMoveString;
  ml: TMoveList;
begin
  if not TBook.book_available then exit;
  num_moves := book.get_moves(board, ml);
  if num_moves = 0 then exit;
  sort_moves(ml);
  printf('Book moves:' + LineEnding);
  for i := 0 to num_moves - 1 do
    printf('%7d %s' + LineEnding, ml[i].score, move_to_san(ml[i].move, board, c));
end;

procedure print_probe_result(var board: TBoard);
var
  wdl, dtm, num_moves, i: integer;
  move: TMove;
  score: TScore;
  cscore: TScoreString;
  cmove: TMoveString;
  u: TStateInfo;
  ml: TMoveList;
begin
  if gtb_probe_wdl(board, PROBE_HARD, wdl) then begin
    printf(LineEnding + 'WDL probe result: ');
    case wdl of
      ord(tb_DRAW): printf('Draw' + LineEnding);
      ord(tb_WMATE): printf('White wins' + LineEnding);
      ord(tb_BMATE): printf('Black wins' + LineEnding);
    else
      printf('Unknown' + LineEnding);
    end;
  end
  else
    printf('WDL probe failed' + LineEnding);

  if gtb_probe_dtm(board, PROBE_HARD, wdl, dtm) then begin
    case wdl of
      ord(tb_DRAW): score := SCORE_ZERO;
      ord(tb_WMATE): score := mate_in(dtm);
      ord(tb_BMATE): score := mated_in(dtm);
    else
      score := -SCORE_INF;
    end;
    printf('DTM probe result: %s' + LineEnding, score_to_string(score * sign[board.us], cmove));
  end
  else
    printf('DTM probe failed' + LineEnding);
  printf(LineEnding);

  num_moves := gen_legal_moves(board, ml);
  for i := 0 to num_moves - 1 do begin
    move := ml[i].move;
    score := -SCORE_INF;
    board.make_move(move, u);
    if gtb_probe_dtm(board, PROBE_HARD, wdl, dtm) then begin
      case wdl of
        ord(tb_DRAW): score := SCORE_ZERO;
        ord(tb_WMATE): score := mated_in(dtm) + 1;
        ord(tb_BMATE): score := mate_in(dtm) - 1;
      end;
    end;
    ml[i].score := score * sign[board.us];
    board.undo_move(move);
  end;
  sort_moves(ml);
  for i := 0 to num_moves - 1 do begin
    if ml[i].score <> -SCORE_INF then
      score_to_string(ml[i].score, cscore)
    else
      strcpy(cscore, 'n/a');
    printf('%-7s = %s' + LineEnding, move_to_san(ml[i].move, board, cmove), cscore);
  end;
  printf(LineEnding);
end;

function stricmp(s1: PAnsiChar; s2: PAnsiChar): boolean;
begin
  result := _stricmp(s1, s2) = 0;
end;

function time_to_string(ms: cardinal; c: PAnsiChar): PAnsiChar;
var
  mins, secs: cardinal;
begin
  secs := ms div 1000;
  mins := secs div 60;
  dec(secs, mins * 60);
  sprintf(c, '%02d:%02d', mins, secs);
  result := c;
end;

procedure print_bb(b: TBitboard);
var r: TRank; f: TFile;
begin
  for r := RANK_8 downto RANK_1 do begin
    for f := FILE_A to FILE_H do
      if (is_set(b, square(r, f))) then
        printf('1')
      else
        printf('0');
    printf(LineEnding);
  end;
  printf(LineEnding);
end;

procedure flip_board(var board: TBoard);
var
  b: TBoard;
  sq: TSquare;
  p: TPiece;
  cr: TCastleRights;
  st: TStateInfo;
begin
  b.clone_from(board, @st);
  board.clear_squares();
  for sq := A1 to H8 do begin
    p := b.squares[flip(sq)];
    if p <> NOPIECE then
      board.squares[sq] := flip(p)
    else
      board.squares[sq] := NOPIECE;
  end;
  cr := board.st^.castle_rights;
  board.st^.castle_rights := ((cr and CR_W_BOTH) shl 1) or ((cr and CR_B_BOTH) shr 1);
  board.us := b.they;
  board.they := flip(b.they);
  board.validate();
end;

function pv_to_string(pv: PMove; var board: TBoard; c: PAnsiChar): PAnsiChar;
var
  m: TMove;
  pvlen: integer;
  old_state: PStateInfo;
  st: TStateInfo;
begin
  old_state := board.st;
  result := c;
  pvlen := 0;
  while pv[pvlen] <> NOMOVE do begin
    m := pv[pvlen];
    move_to_san(m, board, c);
    repeat
      inc(c);
    until (c^ = #0);
    if pvlen >= 100 then break;
    inc(pvlen);
    if pv[pvlen] <> NOMOVE then begin
      c^ := ' ';
      inc(c);
    end;
    board.make_move_fast(m, st);
  end;
  while pvlen > 0 do begin
    dec(pvlen);
    board.undo_move(pv[pvlen]);
  end;
  board.st := old_state;
end;

end.
