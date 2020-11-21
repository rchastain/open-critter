
unit uFen;

interface

uses
  uBoard, uPiece, uSquare;

{$include platform.inc}

type
  TFenString = array [0..255] of AnsiChar;

function encode_fen(var board: TBoard; fen: PAnsichar): PAnsichar;
function decode_fen(var board: TBoard; fen: PAnsiChar): boolean;

implementation

uses
  uSystem;

function encode_fen(var board: TBoard; fen: PAnsichar): PAnsichar;
var
  i, emptycnt: integer;
  r: TRank; f: TFile;
  sq: TSquare;
  p: TPiece;
  cr: TCastleRights;
begin
  i := 0;
  for r := RANK_8 downto RANK_1 do begin
    emptycnt := 0;
    for f := FILE_A to FILE_H do begin
      sq := square(r, f);
      p := board.squares[sq];
      if (p = NOPIECE) then
        inc(emptycnt)
      else begin
        if (p <> NOPIECE) then begin
          if (emptycnt <> 0) then begin
            fen[i] := AnsiChar(ord('0') + emptycnt);
            inc(i);
          end;
          fen[i] := piece_to_char(p);
          inc(i);
          emptycnt :=0;
        end
      end;
    end;
    if (emptycnt <> 0) then begin
      fen[i] := AnsiChar(ord('0') + emptycnt);
      inc(i);
    end;
    fen[i] := '/';
    inc(i);
  end;

  fen[i-1] := ' ';
  fen[i] := color_to_char(board.us);
  fen[i+1] := ' ';
  inc(i, 2);

  cr := board.st^.castle_rights;
  if (cr <> CR_NONE) then begin
    if (board.start_file[START_KING] = FILE_E) and
       (board.start_file[START_ROOK_A] = FILE_A) and
       (board.start_file[START_ROOK_H] = FILE_H) then begin
      if (cr and CR_W_SHORT) <> 0 then begin
        fen[i] := 'K'; inc(i);
      end;
      if (cr and CR_W_LONG) <> 0 then begin
        fen[i] := 'Q'; inc(i);
      end;
      if (cr and CR_B_SHORT) <> 0 then begin
        fen[i] := 'k'; inc(i);
      end;
      if (cr and CR_B_LONG) <> 0 then begin
        fen[i] := 'q'; inc(i);
      end;
    end
    else begin
      if (cr and CR_W_SHORT) <> 0 then begin
        fen[i] := ansichar(ord(board.start_file[START_ROOK_H]) + ord('A'));
        inc(i);
      end;
      if (cr and CR_W_LONG) <> 0 then begin
        fen[i] := ansichar(ord(board.start_file[START_ROOK_A]) + ord('A'));
        inc(i);
      end;
      if (cr and CR_B_SHORT) <> 0 then begin
        fen[i] := file_to_char(board.start_file[START_ROOK_H]);
        inc(i);
      end;
      if (cr and CR_B_LONG) <> 0 then begin
        fen[i] := file_to_char(board.start_file[START_ROOK_A]);
        inc(i);
      end;
    end
  end
  else begin
    fen[i] := '-';
    inc(i);
  end;

  fen[i] := ' '; inc(i);
  if (board.st^.epsq <> NO_EP) then begin
    square_to_str(board.st^.epsq, @fen[i]);
    fen[i+2] := ' ';
    inc(i, 3);
  end
  else begin
    fen[i] := '-';
    fen[i+1] := ' ';
    inc(i, 2);
  end;

  sprintf(@fen[i], '%d %d', board.st^.rule50, board.gameply);

  result := fen;
end;

{$R-}

function decode_fen(var board: TBoard; fen: PAnsiChar): boolean;
var
  tokens: array [0..4] of integer;
  numtokens, i: integer;
  p: TPiece;
  sq: TSquare;
  c: AnsiChar;
  st: PStateInfo;
  r: TRank;
  f, rook_file, king_file: TFile;
begin
  {$ifdef debug}
  printf({$I %FILE%} + ' ' + {$I %LINE%} + ' decode_fen(%s)' + LineEnding, fen);
  {$endif}
  result := false;
  assert(board.st = board.root_state);
  st := board.root_state;
  board.gameply := 0;
  st^.castle_rights := CR_NONE;
  st^.epsq := NO_EP;
  st^.rule50 := 0;
  i := 0;
  numtokens := 0;
  while (fen[i] <> #0) and (numtokens < 5) do
  begin
    if fen[i] = ' ' then
    begin
      tokens[numtokens] := i + 1;
      inc(numtokens);
    end;
    inc(i);
  end;
  if numtokens < 1 then
    exit;
  i := 0;
  sq := A8;
  while True do
  begin
    c := fen[i];
    inc(i);
    if c = #0 then exit;
    if c = ' ' then break;
    if is_rank_digit(c) then
    begin
      inc(sq, ord(c) - ord('1') + 1);
      continue;
    end;
    if c = '/' then
    begin
      if file_of(sq) <> FILE_A then exit;
      inc(sq, 2 * ord(DELTA_S));
      continue;
    end;
    p := char_to_piece(c);
    if p = NOPIECE then exit;
    board.squares[sq] := p;
    inc(sq);
  end;
  case fen[tokens[0]] of
    'w': board.us := WHITE;
    'b': board.us := BLACK;
    else
      exit;
  end;
  board.they := flip(board.us);
  if numtokens > 1 then
  begin
    i := tokens[1];
    repeat
      c := fen[i];
      case c of
        'K': st^.castle_rights := st^.castle_rights or CR_W_SHORT;
        'Q': st^.castle_rights := st^.castle_rights or CR_W_LONG;
        'k': st^.castle_rights := st^.castle_rights or CR_B_SHORT;
        'q': st^.castle_rights := st^.castle_rights or CR_B_LONG;
        'A'..'H':
          begin
            rook_file := TFile(ord(c) - ord('A'));
            if board.squares[square(RANK_1, rook_file)] <> W_ROOK then
            begin
              {$ifdef debug}
              printf({$I %FILE%} + ' ' + {$I %LINE%} + ' rook not found' + LineEnding);
              {$endif}
              exit;
            end;
            king_file := FILE_A;
            for sq := B1 to G1 do
              if (board.squares[sq] = W_KING) then
              begin
                king_file := file_of(sq);
              end;
            if (king_file = FILE_A) then
            begin
              {$ifdef debug}
              printf({$I %FILE%} + ' ' + {$I %LINE%} + ' king not found' + LineEnding);
              {$endif}
              exit;
            end;
            board.start_file[START_KING] := king_file;
            if (rook_file < king_file) then
            begin
              board.start_file[START_ROOK_A] := rook_file;
              st^.castle_rights := st^.castle_rights or CR_W_LONG;
            end else
            begin
              board.start_file[START_ROOK_H] := rook_file;
              st^.castle_rights := st^.castle_rights or CR_W_SHORT;
            end;
          end;
        'a'..'h':
          begin
            rook_file := TFile(ord(c) - ord('a')); // 'A' -> 'a'
            if board.squares[square(RANK_8, rook_file)] <> B_ROOK then
            begin
              {$ifdef debug}
              printf({$I %FILE%} + ' ' + {$I %LINE%} + ' rook not found' + LineEnding);
              {$endif}
              exit;
            end;
            king_file := FILE_A;
            for sq := B8 to G8 do
              if (board.squares[sq] = B_KING) then
              begin
                king_file := file_of(sq);
              end;
            if (king_file = FILE_A) then
            begin
              {$ifdef debug}
              printf({$I %FILE%} + ' ' + {$I %LINE%} + ' king not found' + LineEnding);
              {$endif}
              exit;
            end;
            board.start_file[START_KING] := king_file;
            if (rook_file < king_file) then
            begin
              board.start_file[START_ROOK_A] := rook_file;
              st^.castle_rights := st^.castle_rights or CR_B_LONG;
            end else
            begin
              board.start_file[START_ROOK_H] := rook_file;
              st^.castle_rights := st^.castle_rights or CR_B_SHORT;
            end;
          end;
      end;
      inc(i);
    until (c = ' ') or (c = #0);
  end;
  if (numtokens > 2) then
  begin
    i := tokens[2];
    f := char_to_file(fen[i]);
    if (valid_file(f)) then
    begin
      r := char_to_rank(fen[i+1]);
      if (valid_rank(r)) then
      begin
        st^.epsq := square(r, f);
      end;
    end;
  end;
  if (numtokens > 3) then
  begin
    i := tokens[3];
    st^.rule50 := atoi(@fen[i]);
    if (st^.rule50 < 0) then st^.rule50 := 0;
    if (st^.rule50 > 99) then st^.rule50 := 99;
    board.gameply := st^.rule50;
  end;
  result := true;
end;

end.
