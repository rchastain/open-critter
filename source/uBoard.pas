
unit uBoard;

interface

uses
  uBitboard, uMove, uPiece, uPsq, uScore, uSquare, uZobrist;

{$include platform.inc}

type
  TCastleRights = integer;
  TBishopFlags = integer;

const
  CR_NONE    = 0;
  CR_W_SHORT = 1;
  CR_B_SHORT = 2;
  CR_W_LONG  = 4;
  CR_B_LONG  = 8;
  CR_W_BOTH  = CR_W_LONG + CR_W_SHORT;
  CR_B_BOTH  = CR_B_LONG + CR_B_SHORT;
  CR_ALL     = CR_W_BOTH + CR_B_BOTH;

  BF_WHITE_LIGHT = 1;
  BF_WHITE_DARK  = 2;
  BF_BLACK_LIGHT = 4;
  BF_BLACK_DARK  = 8;
  BF_WHITE_PAIR  = BF_WHITE_LIGHT + BF_WHITE_DARK;
  BF_BLACK_PAIR  = BF_BLACK_LIGHT + BF_BLACK_DARK;
  BF_OPPOSITE_1  = BF_WHITE_DARK  + BF_BLACK_LIGHT;
  BF_OPPOSITE_2  = BF_WHITE_LIGHT + BF_BLACK_DARK;

  MAX_PLY = 256;

  FLAG_W_NULL     = $0001;
  FLAG_B_NULL     = $0002;
  FLAG_AVOID_LAZY = $0004;
  FLAG_PROBE_TB   = $0008;
  FLAG_NO_UPDATE  = $0010;
  FLAG_REPETITION = $0020;
  FLAG_MATE_IN_1  = $0040;
  FLAG_DRAW       = $0080;
  FLAG_QVQ        = $0100;
  FLAG_RVR        = $0200;
  FLAG_W_KM       = $0400;
  FLAG_B_KM       = $0800;
  FLAG_SKIP_EVAL  = $1000;
  FLAG_MATE       = FLAG_MATE_IN_1;
  FLAG_EXACT      = FLAG_DRAW or FLAG_MATE_IN_1 or FLAG_REPETITION;

  NULL_MASK       = FLAG_W_NULL or FLAG_B_NULL;

  phase_val: array [TPiece] of integer = (
    0, 0, 0, 0, 0, 0,
    $000101, // wn
    $010001, // bn
    $000101, // wb
    $010001, // bb
    $000303, // wr
    $030003, // br
    $000606, // wq
    $060006, // bq
    0, 0
  );

  WN_LIST = 1;
  BN_LIST = WN_LIST + 11;
  WB_LIST = BN_LIST + 11;
  BB_LIST = WB_LIST + 11;
  WR_LIST = BB_LIST + 11;
  BR_LIST = WR_LIST + 11;
  WQ_LIST = BR_LIST + 11;
  BQ_LIST = WQ_LIST + 11;

  list_start: array [TPiece] of integer = (
    0, 0, 0, 0, 0, 0,
    WN_LIST, BN_LIST,
    WB_LIST, BB_LIST,
    WR_LIST, BR_LIST,
    WQ_LIST, BQ_LIST,
    0, 0
  );

  bishop_add: array [TSide, 0..1] of integer = (
    ( BF_WHITE_LIGHT, BF_WHITE_DARK ),
    ( BF_BLACK_LIGHT, BF_BLACK_DARK )
  );

  bishop_remove: array [TSide, 0..1] of integer = (
    ( not BF_WHITE_LIGHT, not BF_WHITE_DARK ),
    ( not BF_BLACK_LIGHT, not BF_BLACK_DARK )
  );

type
  TStartFile = ( START_ROOK_A, START_ROOK_H, START_KING );

  PCastleData = ^TCastleData;
  TCastleData = record
    king_xor, rook_xor: TBitboard;
    hash_xor: TKey;
    psq_delta: TScorePair;
    rook: TPiece;
    r_to, k_to: TSquare;
  end;

  TPieceBBs = record
    case integer of
      0: (white, black, w_pawns, b_pawns, w_king, b_king,
          w_knights, b_knights, w_bishops, b_bishops,
          w_rooks, b_rooks, w_queens, b_queens,
          occupied, empty: TBitboard);
      1: (pieces: array [TSide] of TBitboard);
      2: (piece: array [TPiece] of TBitboard);
      3: (ptype: array [TPieceType, TSide] of TBitboard);
      4: (all, pawns, king, knights, bishops,
          rooks, queens: array [TSide] of TBitboard);
  end;

  TPhase = packed record
    case integer of
      0: (int_phase: integer);
      1: (raw: byte; by_side: array [TSide] of byte; total: byte);
  end;

  TPieceList = packed record
    case integer of
      0: (x0: byte; wn, bn, wb, bb, wr, br, wq, bq: array [0..10] of shortint);
      1: (x1: byte; knights, bishops, rooks, queens: array [TSide, 0..10] of shortint);
      2: (pl: array [0..95] of shortint);
  end;

  PStateInfo = ^TStateInfo;
  TStateInfo = packed record
    hashkey, pawnkey: TKey;
    mtrlkey: TKey32;
    psqval: TScorePair;
    castle_rights: TCastleRights;
    bishop_flags: TBishopFlags;
    epsq: TSquare;
    phase: TPhase;
    qs_delay, rule50: integer;
    prev: PStateInfo;
    checkersBB: TBitboard;
    pins: array [TSide] of TBitboard;
    eval, psnl: TScore;
    flags: cardinal;
    lazy_count, tempo: integer;
    lastmove: TMove;
    threat: array [TSide] of integer;
    pinners: array [TSide, TSquare] of byte;
    case integer of
      0: (w_attacks, b_attacks: TBitboard);
      1: (attacks: array [TSide] of TBitboard);
  end;

  TStateInfoCopy = packed record
    hashkey, pawnkey: TKey;
    mtrlkey: TKey32;
    psqval: TScorePair;
    castle_rights: TCastleRights;
    bishop_flags: TBishopFlags;
    epsq: TSquare;
    phase: TPhase;
    qs_delay, rule50: integer;
  end;

  TZobristHistory = array [0 .. 2*MAX_PLY - 1] of TKey;

  PBoard = ^TBoard;
  TBoard = object
    us, they: TSide;
    ply, gameply: integer;
    st: PStateInfo;
    bb: TPieceBBs;
    squares: array [TSquare] of TPiece;
    kingsq: array [TSide] of TSquare;
    count: TPieceCount;
    root_state: PStateInfo;
    start_file: array [TStartFile] of TFile;
    list_index: array [TSquare] of byte;
    piece_list: TPieceList;
    zobrist_history: TZobristHistory;
  private
    procedure add_piece(p: TPiece; sq: TSquare);
    procedure remove_piece(p: TPiece; sq: TSquare);
    procedure piece_list_add(p: TPiece; sq: TSquare);
    procedure piece_list_remove(sq: TSquare);
    procedure backup(out u: TStateInfo);
    procedure init_castle_data();
    procedure update_attacks_w();
    procedure update_attacks_b();
    function calc_hash_key(): TKey;
    function calc_pawn_key(): TKey;
    function calc_mtrl_key(): TKey32;
  public
    function validate(): boolean;
    function load_fen(fen: PAnsiChar): boolean;
    function start_square(sf: TStartFile): TSquare; inline;
    procedure reset(root_st: PStateInfo);
    procedure clear(root_st: PStateInfo);
    procedure trim_stack();
    procedure clear_squares();
    procedure clone_from(const b: TBoard; root_st: PStateInfo);
    procedure update_state(); inline;
    procedure make_null(out u: TStateInfo);
    procedure make_move_fast(move: TMove; out u: TStateInfo);
    procedure make_move(move: TMove; out u: TStateInfo);
    procedure undo_move(move: TMove);
    procedure undo_null();

    function is_check_full(move: TMove): boolean;
    function is_check(move: TMove): boolean;
    function is_legal(move: TMove): boolean;
    function hm_is_legal(move: TMove): boolean;
    function hm_is_legal_evasion(move: TMove): boolean;
    function hm_is_ok(move: TMove): boolean; inline;
    function can_reach(piece: TPiece; from_sq, to_sq: TSquare): boolean;
    function is_draw(): boolean;
    function attackers(sq: TSquare; side: TSide): TBitboard;
    function is_dc(move: TMove): boolean; inline;
    function allow_null(): boolean; inline;

    function opp_bishops(): boolean; inline;
    function pawn_is_passed(sq: TSquare; side: TSide): boolean; overload; inline;
    function pawn_is_passed(sq: TSquare): boolean; overload; inline;
    function queens_rooks(side: TSide): TBitboard; inline;
    function queens_bishops(side: TSide): TBitboard; inline;
    function minors(side: TSide): TBitboard; inline;

    function bishop_attacks(sq: TSquare): TBitboard; inline;
    function rook_attacks(sq: TSquare): TBitboard; inline;
    function queen_attacks(sq: TSquare): TBitboard;

{$ifdef DEBUG_BOARD}
    function debug(): boolean;
    procedure print_move_history();
{$endif}
private
{$ifdef LOG_MOVES}
    procedure log_make_move(move: TMove);
{$endif}
  end;

var
  draw_score: array [TSide] of TScore = ( 0, 0 );
  Empty_OO, Empty_OOO: array [TSide] of TBitboard;
  Safe_OO, Safe_OOO: array [TSide] of TBitboard;

implementation

uses
  uEval, uFen, uTranstab, uSystem, uUtil;

var
  castle_mask: array [TSquare] of byte;
  castle_data: array [0 .. 4] of TCastleData;

procedure TBoard.clear_squares();
begin
  fillchar(squares, sizeof(squares), 0);
  fillchar(count, sizeof(count), 0);
  fillchar(bb, sizeof(bb), 0);
  start_file[START_ROOK_A] := FILE_A;
  start_file[START_ROOK_H] := FILE_H;
  start_file[START_KING] := FILE_E;
end;

procedure TBoard.clear(root_st: PStateInfo);
begin
  clear_squares();
  st := root_st;
  root_state := root_st;
  fillchar(st^, sizeof(TStateInfo), 0);
  st^.prev := root_st;
  ply := 0;
  gameply := 0;
end;

procedure TBoard.trim_stack();
begin
  assert(st^.rule50 = 0);
  move(st^, root_state^, sizeof(TStateInfo));
  st := root_state;
  st^.prev := root_state;
  ply := 0;
  gameply := 0;
end;

procedure TBoard.reset(root_st: PStateInfo);
begin
  clear(root_st);
  decode_fen(self, 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -');
  validate();
  {$ifdef DEBUG_BOARD}
  debug();
  {$endif}
end;

procedure TBoard.clone_from(const b: TBoard; root_st: PStateInfo);
var
  i: integer;
begin
  move(b, self, sizeof(TBoard) - sizeof(TZobristHistory));
  root_state := root_st;
  root_state^.prev := root_st;
  st := root_st;
  move(b.st^, st^, sizeof(TStateInfo));
  assert(root_state^.rule50 <= gameply);
  for i := gameply - root_state^.rule50 to gameply-1 do
    zobrist_history[i] := b.zobrist_history[i];
end;

function TBoard.load_fen(fen: PAnsiChar): boolean;
var
  fen_backup: array [0..255] of AnsiChar;
  a, b: boolean;
begin
  {$ifdef debug}
  printf({$I %FILE%} + ' ' + {$I %LINE%} + ' TBoard.load_fen(%s)' + LineEnding, fen);
  {$endif}
  encode_fen(self, fen_backup);
  clear(root_state);
  a := decode_fen(self, fen);
  b := validate();
  if not a or not b then
  begin
    {$ifdef debug}
    printf({$I %FILE%} + ' ' + {$I %LINE%} + ' decode_fen %d validate %d' + LineEnding, Ord(a), Ord(b));
    {$endif}
    clear(root_state);
    decode_fen(self, fen_backup);
    validate();
    result := false;
    exit;
  end;
  result := true;
end;

procedure TBoard.add_piece(p: TPiece; sq: TSquare);
begin
  squares[sq] := p;
  set_bit(bb.piece[p], sq);
  set_bit(bb.pieces[side_of(p)], sq);
  st^.hashkey := st^.hashkey xor KeyPieces[p, sq];
  inc(st^.psqval, PSQ[p, sq]);
  inc(count.by_piece[p]);
  st^.mtrlkey := st^.mtrlkey xor KeyMtrl[p, count.by_piece[p]];
  inc(st^.phase.int_phase, phase_val[p]);
end;

procedure TBoard.remove_piece(p: TPiece; sq: TSquare);
begin
  squares[sq] := NOPIECE;
  xor_bit(bb.piece[p], sq);
  xor_bit(bb.pieces[side_of(p)], sq);
  st^.hashkey := st^.hashkey xor KeyPieces[p, sq];
  dec(st^.psqval, PSQ[p, sq]);
  st^.mtrlkey := st^.mtrlkey xor KeyMtrl[p, count.by_piece[p]];
  dec(count.by_piece[p]);
  dec(st^.phase.int_phase, phase_val[p]);
end;

procedure TBoard.piece_list_add(p: TPiece; sq: TSquare);
var ofs: integer;
begin
  ofs := list_start[p] + count.by_piece[p];
  list_index[sq] := ofs;
  piece_list.pl[ofs] := ord(sq);
end;

procedure TBoard.piece_list_remove(sq: TSquare);
var ofs, next: integer;
begin
  ofs := list_index[sq];
  if ofs <> 0 then begin
    next := piece_list.pl[ofs + 1];
    piece_list.pl[ofs] := next;
    while (next <> -1) do begin
      list_index[TSquare(next)] := ofs;
      inc(ofs);
      next := piece_list.pl[ofs + 1];
      piece_list.pl[ofs] := next;
    end;
  end;
end;

procedure TBoard.backup(out u: TStateInfo);
begin
  u.prev := st;
  move(st^, u, sizeof(TStateInfoCopy));
  st := @u;
end;

procedure TBoard.make_move_fast(move: TMove; out u: TStateInfo);
begin
  make_move(move, u);
  update_state();
end;

procedure TBoard.make_move(move: TMove; out u: TStateInfo);
var
  s: PStateInfo;
  piece, victim, promo: TPiece;
  from_sq, to_sq: TSquare;
  hash_delta: TKey;
  ofs: integer;
  b: TBitboard;
  cr: TCastleRights;
  cd: PCastleData;
begin
{$ifdef LOG_MOVES}
  log_make_move(move);
{$endif}
{$ifdef DEBUG_BOARD}
  if not hm_is_ok(move) then begin
    print_board(self);
    printf('trying to make an illegal move (%s)' + LineEnding, move_to_notation(move, cmove));
    pause();
  end;
{$endif}
  backup(u);
  s := st;
  zobrist_history[gameply] := s^.hashkey;
  inc(ply);
  inc(gameply);
  inc(s^.rule50);

  if (s^.epsq <> NO_EP) then begin
    s^.hashkey := s^.hashkey xor KeyEP[file_of(s^.epsq)];
    s^.epsq := NO_EP;
  end;

  s^.lastmove := move;
  from_sq := source_sq(move);
  to_sq := target_sq(move);
  piece := piece_moved(move);

  hash_delta := KeyPieces[piece, from_sq] xor KeyPieces[piece, to_sq];

  cr := s^.castle_rights and castle_mask[from_sq] and castle_mask[to_sq];
  if cr <> s^.castle_rights then begin
    s^.pawnkey := s^.pawnkey xor KeyCastle[s^.castle_rights xor cr];
    s^.hashkey := s^.hashkey xor KeyCastle[s^.castle_rights xor cr];
    s^.castle_rights := cr;
  end;

  if not is_special(move) then begin
    s^.hashkey := not (s^.hashkey xor hash_delta);
    if is_capture(move) then begin
      s^.rule50 := 0;
      victim := capture_victim(move);
      xor_bit(bb.piece[victim], to_sq);
      xor_bit(bb.pieces[they], to_sq);
      dec(s^.psqval, PSQ[victim, to_sq]);
      s^.mtrlkey := s^.mtrlkey xor KeyMtrl[victim, count.by_piece[victim]];
      dec(count.by_piece[victim]);
      dec(s^.phase.int_phase, phase_val[victim]);
      s^.hashkey := s^.hashkey xor KeyPieces[victim, to_sq];
      if (victim <= B_PAWN) then
        s^.pawnkey := s^.pawnkey xor KeyPieces[victim, to_sq]
      else begin
        dec(count.np[they]);
        piece_list_remove(to_sq);
        if type_of(victim) = BISHOP then begin
          if (bb.bishops[they] and BB_Light) = 0 then
            s^.bishop_flags := s^.bishop_flags and bishop_remove[they, 0];
          if (bb.bishops[they] and BB_Dark) = 0 then
            s^.bishop_flags := s^.bishop_flags and bishop_remove[they, 1];
        end;
      end;
    end;
    squares[from_sq] := NOPIECE;
    squares[to_sq] := piece;
    inc(s^.psqval, PSQ[piece, to_sq] - PSQ[piece, from_sq]);
    b := SquareBB[from_sq] or SquareBB[to_sq];
    bb.piece[piece] := bb.piece[piece] xor b;
    bb.pieces[us] := bb.pieces[us] xor b;

    ofs := list_index[from_sq];
    list_index[to_sq] := ofs;
    if ofs <> 0 then
      piece_list.pl[ofs] := ord(to_sq);

    if (piece <= B_PAWN) then begin
      s^.pawnkey := s^.pawnkey xor hash_delta;
      s^.rule50 := 0;
      if is_doublepush(move) then
        if (bb.pawns[they] and EpMaskBB[to_sq]) <> 0 then begin
          s^.epsq := delta_sub(to_sq, PawnPush[us]);
          s^.hashkey := s^.hashkey xor KeyEP[file_of(s^.epsq)];
        end;
    end
    else if (piece <= B_KING) then begin
      s^.pawnkey := s^.pawnkey xor hash_delta;
      kingsq[us] := to_sq;
    end;
  end

  else if is_castle(move) then begin
    cd := @castle_data[(cardinal(us) shl 1) + cardinal(to_sq < from_sq)];
    squares[from_sq] := NOPIECE;
    squares[to_sq] := NOPIECE;
    kingsq[us] := cd^.k_to;
    squares[cd^.k_to] := piece;
    squares[cd^.r_to] := cd^.rook;
    bb.piece[piece] := bb.piece[piece] xor cd^.king_xor;
    bb.piece[cd^.rook] := bb.piece[cd^.rook] xor cd^.rook_xor;
    bb.pieces[us] := bb.pieces[us] xor cd^.king_xor xor cd^.rook_xor;
    inc(s^.psqval, cd^.psq_delta);
    s^.pawnkey := s^.pawnkey xor KeyPieces[piece, from_sq] xor KeyPieces
      [piece, cd^.k_to];
    s^.hashkey := s^.hashkey xor cd^.hash_xor;
    ofs := list_index[to_sq];
    list_index[cd^.r_to] := ofs;
    piece_list.pl[ofs] := ord(cd^.r_to);
    list_index[cd^.k_to] := 0;
  end

  else if is_promotion(move) then begin
    s^.rule50 := 0;
    s^.pawnkey := s^.pawnkey xor KeyPieces[piece, from_sq];
    s^.hashkey := not s^.hashkey;
    if is_capture(move) then begin
      victim := capture_victim(move);
      remove_piece(victim, to_sq);
      piece_list_remove(to_sq);
      dec(count.np[they]);
      if type_of(victim) = BISHOP then begin
        if (bb.bishops[they] and BB_Light) = 0 then
          s^.bishop_flags := s^.bishop_flags and bishop_remove[they, 0];
        if (bb.bishops[they] and BB_Dark) = 0 then
          s^.bishop_flags := s^.bishop_flags and bishop_remove[they, 1];
      end;
    end;
    remove_piece(piece, from_sq);
    promo := promoted_to(move);
    piece_list_add(promo, to_sq);
    add_piece(promo, to_sq);
    inc(count.np[us]);
    if type_of(promo) = BISHOP then
      s^.bishop_flags := s^.bishop_flags or bishop_add[us, cardinal(sq_color(to_sq))];
  end

  else begin
    assert(is_enpassant(move));
    s^.rule50 := 0;
    squares[from_sq] := NOPIECE;
    squares[to_sq] := piece;
    list_index[to_sq] := 0;
    inc(s^.psqval, PSQ[piece, to_sq] - PSQ[piece, from_sq]);
    b := SquareBB[from_sq] or SquareBB[to_sq];
    bb.piece[piece] := bb.piece[piece] xor b;
    bb.pieces[us] := bb.pieces[us] xor b;
    dec(to_sq, PawnPush[us]);
    remove_piece(flip(piece), to_sq);
    s^.hashkey := not (s^.hashkey xor hash_delta);
    s^.pawnkey := s^.pawnkey xor hash_delta xor KeyPieces[flip(piece), to_sq];
  end;

{$ifdef hasprefetch}
  tt.prefetch(s^.hashkey);
{$endif}

  bb.occupied := bb.black or bb.white;
  bb.empty := not bb.occupied;

  us := they;
  they := flip(they);

  s^.flags := 0;
  if is_draw() then begin
    s^.eval := - draw_score[us];
    s^.flags := FLAG_REPETITION;
  end;

  {if s^.hashkey = $330937249e3dd94a then
    printf('.');}
end;

procedure TBoard.undo_move(move: TMove);
var
  from_sq, to_sq: TSquare;
  p, victim, promo: TPiece;
  ofs: integer;
  cd: PCastleData;
  b: TBitboard;
begin
  assert(st^.prev <> nil);
  st := st^.prev;
  dec(ply);
  dec(gameply);
  us := they;
  they := flip(they);

  from_sq := source_sq(move);
  to_sq := target_sq(move);
  p := piece_moved(move);

  squares[from_sq] := p;
  if not is_special(move) then begin
    b := SquareBB[from_sq] or SquareBB[to_sq];
    bb.piece[p] := bb.piece[p] xor b;
    bb.pieces[us] := bb.pieces[us] xor b;
    victim := capture_victim(move);
    squares[to_sq] := victim;
    ofs := list_index[to_sq];
    list_index[from_sq] := ofs;
    if ofs <> 0 then
      piece_list.pl[ofs] := ord(from_sq);
    if (victim <> NOPIECE) then begin
      set_bit(bb.piece[victim], to_sq);
      set_bit(bb.pieces[they], to_sq);
      if (victim > B_PAWN) then  begin
        piece_list_add(victim, to_sq);
        inc(count.np[they]);
      end
      else
        list_index[to_sq] := 0;
      inc(count.by_piece[victim]);
    end;
    if (type_of(p) = KING) then
      kingsq[us] := from_sq;
  end

  else if is_castle(move) then begin
    cd := @castle_data[(ord(us) shl 1) + ord(to_sq < from_sq)];
    squares[cd^.k_to] := NOPIECE;
    squares[cd^.r_to] := NOPIECE;
    squares[to_sq] := cd^.rook;
    squares[from_sq] := p;
    kingsq[us] := from_sq;
    bb.piece[p] := bb.piece[p] xor cd^.king_xor;
    bb.piece[cd^.rook] := bb.piece[cd^.rook] xor cd^.rook_xor;
    bb.pieces[us] := bb.pieces[us] xor cd^.king_xor xor cd^.rook_xor;
    ofs := list_index[cd^.r_to];
    list_index[to_sq] := ofs;
    list_index[from_sq] := 0;
    piece_list.pl[ofs] := ord(to_sq);
  end

  else if is_promotion(move) then begin
    promo := promoted_to(move);
    dec(count.by_piece[promo]);
    dec(count.np[us]);
    xor_bit(bb.piece[promo], to_sq);
    xor_bit(bb.pieces[us], to_sq);
    set_bit(bb.pieces[us], from_sq);
    set_bit(bb.piece[p], from_sq);
    inc(count.by_piece[p]);
    piece_list_remove(to_sq);
    list_index[from_sq] := 0;
    victim := capture_victim(move);
    squares[to_sq] := victim;
    if (victim <> NOPIECE) then begin
      set_bit(bb.piece[victim], to_sq);
      set_bit(bb.pieces[they], to_sq);
      piece_list_add(victim, to_sq);
      inc(count.by_piece[victim]);
      inc(count.np[they]);
    end;
  end

  else begin
    assert(is_enpassant(move));
    squares[to_sq] := NOPIECE;
    b := SquareBB[from_sq] or SquareBB[to_sq];
    bb.piece[p] := bb.piece[p] xor b;
    bb.pieces[us] := bb.pieces[us] xor b;
    to_sq := delta_add(to_sq, PawnPush[they]);
    p := flip(p);
    squares[to_sq] := p;
    list_index[from_sq] := 0;
    list_index[to_sq] := 0;
    inc(count.by_piece[p]);
    set_bit(bb.piece[p], to_sq);
    set_bit(bb.pieces[they], to_sq);
  end;

  bb.occupied := bb.black or bb.white;
  bb.empty := not bb.occupied;
end;

procedure TBoard.make_null(out u: TStateInfo);

(* https://www.lazarusforum.de/viewtopic.php?p=39513#p39513 *)
procedure ZeroMemory(Destination: Pointer; Length: SizeInt);
begin
  FillByte(Destination^, Length, 0);
end;

var
  prev_state: PStateInfo;
begin
{$ifdef LOG_MOVES}
  log_make_move(NOMOVE);
{$endif}
  assert(st^.lastmove <> NOMOVE);
  prev_state := st;
  ZeroMemory(@u, SizeOf(u));
  move(st^, u, sizeof(TStateInfoCopy));
  u.prev := st;
  st := @u;
  with st^ do
  begin
    zobrist_history[gameply] := hashkey;
    inc(gameply);
    inc(ply);
    lastmove := NOMOVE;
    inc(rule50);
    hashkey := not hashkey;
    if epsq <> NO_EP then begin
      hashkey := hashkey xor KeyEP[file_of(epsq)];
      epsq := NO_EP;
    end;

    {$ifdef hasprefetch}
    tt.prefetch(hashkey);
    {$endif}

    lazy_count := prev_state^.lazy_count;
    psnl := prev_state^.psnl;
    flags := prev_state^.flags and not NULL_MASK;
    phase := prev_state^.phase;
    tempo := prev_state^.tempo;
    eval := -(prev_state^.eval + tempo);

    checkersBB := 0;

    attacks[WHITE] := prev_state^.attacks[WHITE];
    attacks[BLACK] := prev_state^.attacks[BLACK];

    pins[WHITE] := prev_state^.pins[WHITE];
    pins[BLACK] := prev_state^.pins[BLACK];

    threat[WHITE] := prev_state^.threat[WHITE];
    threat[BLACK] := prev_state^.threat[BLACK];

    if (pins[WHITE] <> 0) or (pins[BLACK] <> 0) then
      move(prev_state^.pinners, pinners, sizeof(pinners));

    if is_draw() then begin
      eval := - draw_score[us];
      flags := flags or FLAG_REPETITION;
    end;

  end;
  us := they;
  they := flip(they);
{$ifdef DEBUG_BOARD}
  if not debug() then begin
    printf('error after making null move' + LineEnding);
    print_board(self);
    print_move_history();
    pause();
  end;
{$endif}
end;

procedure TBoard.undo_null();
begin
  st := st^.prev;
  dec(gameply);
  dec(ply);
  us := they;
  they := flip(they);
{$ifdef DEBUG_BOARD}
  if not debug() then begin
    printf('error after unmaking null move' + LineEnding);
    print_board(self);
    print_move_history();
    pause();
  end;
{$endif}
end;

function TBoard.is_draw(): boolean;
var
  i: integer;
begin

  if (st^.rule50 >= 100) then begin
    result := true;
    exit;
  end;

  i := 4;
  while (i <= st^.rule50) do begin
    if (zobrist_history[gameply - i] = st^.hashkey) then begin
      result := true;
      exit;
    end;
    inc(i, 2);
  end;

  result := false;
end;

procedure TBoard.update_state();
begin
  if us = WHITE then
    update_attacks_b()
  else
    update_attacks_w();
end;

procedure TBoard.update_attacks_w();
var
  b, a, chk: TBitboard;
  lp: PShortInt;
  sq: TSquare;
begin
  st^.checkersBB := 0;
  st^.pins[WHITE] := 0;
  st^.pins[BLACK] := 0;
  st^.w_attacks := KingStep[kingsq[WHITE]];
  st^.b_attacks := KingStep[kingsq[BLACK]] or pawn_attk_b(bb.b_pawns);

  a := pawn_west_w(bb.w_pawns);
  st^.w_attacks := st^.w_attacks or a;
  chk := a and bb.b_king;
  if (chk <> 0) then
    st^.checkersBB := st^.checkersBB or pawn_east_b(chk);

  a := pawn_east_w(bb.w_pawns);
  st^.w_attacks := st^.w_attacks or a;
  chk := a and bb.b_king;
  if (chk <> 0) then
    st^.checkersBB := st^.checkersBB or pawn_west_b(chk);

  lp := @piece_list.wn;
  while lp^ <> -1 do begin
    sq := TSquare(lp^);
    a := KnightStep[sq];
    st^.w_attacks := st^.w_attacks or a;
    chk := a and bb.b_king;
    if chk <> 0 then
      set_bit(st^.checkersBB, sq);
    inc(lp);
  end;

  lp := @piece_list.wb;
  while lp^ <> -1 do begin
    sq := TSquare(lp^);
    st^.w_attacks := st^.w_attacks or bishop_attacks(sq);
    if Direction[kingsq[BLACK], sq] and DIR_DIAG <> 0 then begin
      b := BetweenBB[kingsq[BLACK], sq] and bb.occupied;
      if b <> 0 then begin
        if b and TBB(b-1) = 0 then begin
          st^.pins[WHITE] := st^.pins[WHITE] or b;
          st^.pinners[WHITE, find_lsb(b)] := ord(sq);
        end
      end
      else
        set_bit(st^.checkersBB, sq);
    end;
    inc(lp);
  end;

  lp := @piece_list.wr;
  while lp^ <> -1 do begin
    sq := TSquare(lp^);
    st^.w_attacks := st^.w_attacks or rook_attacks(sq);
    if Direction[kingsq[BLACK], sq] and DIR_ORTH <> 0 then begin
      b := BetweenBB[kingsq[BLACK], sq] and bb.occupied;
      if b <> 0 then begin
        if b and TBB(b-1) = 0 then begin
          st^.pins[WHITE] := st^.pins[WHITE] or b;
          st^.pinners[WHITE, find_lsb(b)] := ord(sq);
        end
      end
      else
        set_bit(st^.checkersBB, sq);
    end;
    inc(lp);
  end;

  lp := @piece_list.wq;
  while lp^ <> -1 do begin
    sq := TSquare(lp^);
    st^.w_attacks := st^.w_attacks or queen_attacks(sq);
    if Direction[kingsq[BLACK], sq] <> 0 then begin
      b := BetweenBB[kingsq[BLACK], sq] and bb.occupied;
      if b <> 0 then begin
        if b and TBB(b-1) = 0 then begin
          st^.pins[WHITE] := st^.pins[WHITE] or b;
          st^.pinners[WHITE, find_lsb(b)] := ord(sq);
        end
      end
      else
        set_bit(st^.checkersBB, sq);
    end;
    inc(lp);
  end;

  lp := @piece_list.bn;
  while lp^ <> -1 do begin
    st^.b_attacks := st^.b_attacks or KnightStep[TSquare(lp^)];
    inc(lp);
  end;

  lp := @piece_list.bb;
  while lp^ <> -1 do begin
    sq := TSquare(lp^);
    st^.b_attacks := st^.b_attacks or bishop_attacks(sq);
    if Direction[kingsq[WHITE], sq] and DIR_DIAG <> 0 then begin
      b := BetweenBB[kingsq[WHITE], sq] and bb.occupied;
      if (b <> 0) and (b and TBB(b-1) = 0) then begin
        st^.pins[BLACK] := st^.pins[BLACK] or b;
        st^.pinners[BLACK, find_lsb(b)] := ord(sq);
      end;
    end;
    inc(lp);
  end;

  lp := @piece_list.br;
  while lp^ <> -1 do begin
    sq := TSquare(lp^);
    st^.b_attacks := st^.b_attacks or rook_attacks(sq);
    if Direction[kingsq[WHITE], sq] and DIR_ORTH <> 0 then begin
      b := BetweenBB[kingsq[WHITE], sq] and bb.occupied;
      if (b <> 0) and (b and TBB(b-1) = 0) then begin
        st^.pins[BLACK] := st^.pins[BLACK] or b;
        st^.pinners[BLACK, find_lsb(b)] := ord(sq);
      end;
    end;
    inc(lp);
  end;

  lp := @piece_list.bq;
  while lp^ <> -1 do begin
    sq := TSquare(lp^);
    st^.b_attacks := st^.b_attacks or queen_attacks(sq);
    if Direction[kingsq[WHITE], sq] <> 0 then begin
      b := BetweenBB[kingsq[WHITE], sq] and bb.occupied;
      if (b <> 0) and (b and TBB(b-1) = 0) then begin
        st^.pins[BLACK] := st^.pins[BLACK] or b;
        st^.pinners[BLACK, find_lsb(b)] := ord(sq);
      end;
    end;
    inc(lp);
  end;
{$ifdef DEBUG_BOARD}
  if not debug() then begin
    printf('error after making move %s' + LineEnding, move_to_notation(st^.lastmove, cmove));
    print_board(self);
    print_move_history();
    pause();
  end;
{$endif}
end;

procedure TBoard.update_attacks_b();
var
  b, a, chk: TBitboard;
  lp: PShortInt;
  sq: TSquare;
begin
  st^.checkersBB := 0;
  st^.pins[WHITE] := 0;
  st^.pins[BLACK] := 0;
  st^.w_attacks := KingStep[kingsq[WHITE]] or pawn_attk_w(bb.w_pawns);
  st^.b_attacks := KingStep[kingsq[BLACK]];

  a := pawn_west_b(bb.b_pawns);
  st^.b_attacks := st^.b_attacks or a;
  chk := a and bb.w_king;
  if (chk <> 0) then
    st^.checkersBB := st^.checkersBB or pawn_east_w(chk);

  a := pawn_east_b(bb.b_pawns);
  st^.b_attacks := st^.b_attacks or a;
  chk := a and bb.w_king;
  if (chk <> 0) then
    st^.checkersBB := st^.checkersBB or pawn_west_w(chk);

  lp := @piece_list.bn;
  while lp^ <> -1 do begin
    sq := TSquare(lp^);
    a := KnightStep[sq];
    st^.b_attacks := st^.b_attacks or a;
    chk := a and bb.w_king;
    if chk <> 0 then
      set_bit(st^.checkersBB, sq);
    inc(lp);
  end;

  lp := @piece_list.bb;
  while lp^ <> -1 do begin
    sq := TSquare(lp^);
    st^.b_attacks := st^.b_attacks or bishop_attacks(sq);
    if Direction[kingsq[WHITE], sq] and DIR_DIAG <> 0 then begin
      b := BetweenBB[kingsq[WHITE], sq] and bb.occupied;
      if b <> 0 then begin
        if b and TBB(b-1) = 0 then begin
          st^.pins[BLACK] := st^.pins[BLACK] or b;
          st^.pinners[BLACK, find_lsb(b)] := ord(sq);
        end
      end
      else
        set_bit(st^.checkersBB, sq);
    end;
    inc(lp);
  end;

  lp := @piece_list.br;
  while lp^ <> -1 do begin
    sq := TSquare(lp^);
    st^.b_attacks := st^.b_attacks or rook_attacks(sq);
    if Direction[kingsq[WHITE], sq] and DIR_ORTH <> 0 then begin
      b := BetweenBB[kingsq[WHITE], sq] and bb.occupied;
      if b <> 0 then begin
        if b and TBB(b-1) = 0 then begin
          st^.pins[BLACK] := st^.pins[BLACK] or b;
          st^.pinners[BLACK, find_lsb(b)] := ord(sq);
        end
      end
      else
        set_bit(st^.checkersBB, sq);
    end;
    inc(lp);
  end;

  lp := @piece_list.bq;
  while lp^ <> -1 do begin
    sq := TSquare(lp^);
    st^.b_attacks := st^.b_attacks or queen_attacks(sq);
    if Direction[kingsq[WHITE], sq] <> 0 then begin
      b := BetweenBB[kingsq[WHITE], sq] and bb.occupied;
      if b <> 0 then begin
        if b and TBB(b-1) = 0 then begin
          st^.pins[BLACK] := st^.pins[BLACK] or b;
          st^.pinners[BLACK, find_lsb(b)] := ord(sq);
        end
      end
      else
        set_bit(st^.checkersBB, sq);
    end;
    inc(lp);
  end;

  lp := @piece_list.wn;
  while lp^ <> -1 do begin
    st^.w_attacks := st^.w_attacks or KnightStep[TSquare(lp^)];
    inc(lp);
  end;

  lp := @piece_list.wb;
  while lp^ <> -1 do begin
    sq := TSquare(lp^);
    st^.w_attacks := st^.w_attacks or bishop_attacks(sq);
    if Direction[kingsq[BLACK], sq] and DIR_DIAG <> 0 then begin
      b := BetweenBB[kingsq[BLACK], sq] and bb.occupied;
      if (b <> 0) and (b and TBB(b-1) = 0) then begin
        st^.pins[WHITE] := st^.pins[WHITE] or b;
        st^.pinners[WHITE, find_lsb(b)] := ord(sq);
      end;
    end;
    inc(lp);
  end;

  lp := @piece_list.wr;
  while lp^ <> -1 do begin
    sq := TSquare(lp^);
    st^.w_attacks := st^.w_attacks or rook_attacks(sq);
    if Direction[kingsq[BLACK], sq] and DIR_ORTH <> 0 then begin
      b := BetweenBB[kingsq[BLACK], sq] and bb.occupied;
      if (b <> 0) and (b and TBB(b-1) = 0) then begin
        st^.pins[WHITE] := st^.pins[WHITE] or b;
        st^.pinners[WHITE, find_lsb(b)] := ord(sq);
      end;
    end;
    inc(lp);
  end;

  lp := @piece_list.wq;
  while lp^ <> -1 do begin
    sq := TSquare(lp^);
    st^.w_attacks := st^.w_attacks or queen_attacks(sq);
    if Direction[kingsq[BLACK], sq] <> 0 then begin
      b := BetweenBB[kingsq[BLACK], sq] and bb.occupied;
      if (b <> 0) and (b and TBB(b-1) = 0) then begin
        st^.pins[WHITE] := st^.pins[WHITE] or b;
        st^.pinners[WHITE, find_lsb(b)] := ord(sq);
      end;
    end;
    inc(lp);
  end;
{$ifdef DEBUG_BOARD}
  if not debug() then begin
    printf('error after making move %s' + LineEnding, move_to_notation(st^.lastmove, cmove));
    print_board(self);
    print_move_history();
    pause();
  end;
{$endif}
end;

function TBoard.can_reach(piece: TPiece; from_sq, to_sq: TSquare): boolean;
begin
  case piece of
    W_PAWN:
      result := PawnAttacksBB[WHITE, from_sq] and SquareBB[to_sq] <> 0;

    B_PAWN:
      result := PawnAttacksBB[BLACK, from_sq] and SquareBB[to_sq] <> 0;

    W_KING, B_KING:
      result := KingStep[from_sq] and SquareBB[to_sq] <> 0;

    W_KNIGHT, B_KNIGHT:
      result := KnightStep[from_sq] and SquareBB[to_sq] <> 0;

    W_BISHOP, B_BISHOP:
      result := (Direction[from_sq, to_sq] and DIR_DIAG <> 0)
        and (BetweenBB[from_sq, to_sq] and bb.occupied = 0);

    W_ROOK, B_ROOK:
      result := (Direction[from_sq, to_sq] and DIR_ORTH <> 0)
        and (BetweenBB[from_sq, to_sq] and bb.occupied = 0);

    W_QUEEN, B_QUEEN:
      result := (Direction[from_sq, to_sq] <> DIR_NONE)
        and (BetweenBB[from_sq, to_sq] and bb.occupied = 0);

    else begin
      assert(false, 'can_reach(): illegal piece');
      result := false;
    end;
  end;
end;

function TBoard.attackers(sq: TSquare; side: TSide): TBitboard;
begin
  result := bishop_attacks(sq) and (bb.queens[side] or bb.bishops[side])
    or rook_attacks(sq) and (bb.queens[side] or bb.rooks[side])
    or KnightStep[sq] and bb.knights[side]
    or KingStep[sq] and bb.king[side]
    or PawnAttacksBB[flip(side), sq] and bb.pawns[side];
end;

function TBoard.is_check(move: TMove): boolean;
var
  from_sq, to_sq, ksq: TSquare;

begin
  from_sq := source_sq(move);
  to_sq := target_sq(move);
  ksq := kingsq[they];

  case piece_moved(move) of
    W_PAWN, B_PAWN:
      result := (PawnAttacksBB[they, ksq] and SquareBB[to_sq] <> 0)
        or ((st^.pins[us] and SquareBB[from_sq] <> 0)
            and (Direction[ksq, from_sq] <> Direction[ksq, to_sq]));

    W_KING, B_KING:
      result := ((st^.pins[us] and SquareBB[from_sq] <> 0)
        and (Direction[ksq, from_sq] <> Direction[ksq, to_sq]));

    W_KNIGHT, B_KNIGHT:
      result := (st^.pins[us] and SquareBB[from_sq] <> 0)
        or (KnightStep[ksq] and SquareBB[to_sq] <> 0);

    W_BISHOP, B_BISHOP:
      result := (st^.pins[us] and SquareBB[from_sq] <> 0) or
        (Direction[to_sq, ksq] and DIR_DIAG <> 0)
         and (BetweenBB[to_sq, ksq] and bb.occupied = 0);

    W_ROOK, B_ROOK:
      result := (st^.pins[us] and SquareBB[from_sq] <> 0) or
        (Direction[to_sq, ksq] and DIR_ORTH <> 0)
         and (BetweenBB[to_sq, ksq] and bb.occupied = 0);
  else
    result := (Direction[to_sq, ksq] <> DIR_NONE)
      and (BetweenBB[to_sq, ksq] and bb.occupied = 0);
  end;
end;

function TBoard.is_check_full(move: TMove): boolean;
var
  ksq, from_sq, to_sq, xsq: TSquare;
  cd: PCastleData;
  occ: TBitboard;
begin
  if not is_special(move) then begin
    result := is_check(move);
    exit;
  end;

  ksq := kingsq[they];
  from_sq := source_sq(move);
  to_sq := target_sq(move);

  if is_promotion(move) then begin
    occ := bb.occupied xor SquareBB[from_sq];
    case type_of(promoted_to(move)) of
      KNIGHT:
        result := KnightStep[ksq] and SquareBB[to_sq] <> 0;
      BISHOP:
        result := (Direction[to_sq, ksq] and DIR_DIAG <> 0)
          and (BetweenBB[to_sq, ksq] and occ = 0);
      ROOK:
        result := (Direction[to_sq, ksq] and DIR_ORTH <> 0)
          and (BetweenBB[to_sq, ksq] and occ = 0);
      QUEEN:
        result := (Direction[to_sq, ksq] <> DIR_NONE)
          and (BetweenBB[to_sq, ksq] and occ = 0);
      else begin
        assert(false, 'is_check(): illegal promotion');
        result := false;
      end
    end;
  end else

  if is_enpassant(move) then begin
    xsq := square(rank_of(from_sq), file_of(to_sq));
    occ := bb.occupied xor SquareBB[xsq]
      xor SquareBB[from_sq] xor SquareBB[to_sq];
    result := uBitboard.rook_attacks(ksq, occ) and queens_rooks(us) or
      uBitboard.bishop_attacks(ksq, occ) and queens_bishops(us) <> 0;
  end else

  begin
    assert(is_castle(move));
    cd := @castle_data[(cardinal(us) shl 1) + cardinal(to_sq < from_sq)];
    if Direction[ksq, cd^.r_to] and DIR_ORTH <> 0 then begin
      occ := bb.occupied xor cd^.rook_xor xor cd^.king_xor;
      result := BetweenBB[ksq, cd^.r_to] and occ = 0;
    end
    else
      result := false;
  end;
end;

function TBoard.is_legal(move: TMove): boolean;
var
  ksq, from_sq, to_sq, sq: TSquare;
  b: TBitboard;
begin
  assert(st^.checkersBB = 0, 'is_legal() called while in check!');

  if is_king_move(move) then begin
    result := true;
    exit;
  end;

  ksq := kingsq[us];
  from_sq := source_sq(move);
  to_sq := target_sq(move);

  if not is_enpassant(move) then begin
    if not is_set(st^.pins[they], from_sq) then begin
      result := true;
      exit;
    end;
    if type_of(piece_moved(move)) <> KNIGHT then
      result := (Direction[ksq, from_sq] xor Direction[ksq, to_sq]) and $0f = 0
    else
      result := false;
  end
  else begin
    sq := square(rank_of(from_sq), file_of(to_sq));
    b := (bb.occupied xor SquareBB[from_sq] xor SquareBB[sq]) or SquareBB[to_sq];
    result := (uBitboard.rook_attacks(ksq, b) and queens_rooks(they) = 0)
          and (uBitboard.bishop_attacks(ksq, b) and queens_bishops(they) = 0);
  end;
end;

function TBoard.hm_is_legal(move: TMove): boolean;
var
  from_sq, to_sq, sq, ksq: TSquare;
  b: TBitboard;
  p: TPiece;
begin
  assert(st^.checkersBB = 0, 'is_legal() called while in check!');

  result := false;
  from_sq := source_sq(move);
  to_sq := target_sq(move);
  p := piece_moved(move);

  if (side_of(p) <> us) or (squares[from_sq] <> p) then exit;

  if is_enpassant(move) then begin
    if (to_sq <> st^.epsq) then exit;
    sq := square(rank_of(from_sq), file_of(to_sq));
    b := (bb.occupied xor SquareBB[from_sq] xor SquareBB[sq]) or SquareBB[to_sq];
    result := ((uBitboard.rook_attacks(kingsq[us], b) and queens_rooks(they)) = 0)
          and ((uBitboard.bishop_attacks(kingsq[us], b) and queens_bishops(they)) = 0);
    exit;
  end;

  if is_king_move(move) then begin
    if is_castle(move) then begin
      if to_sq > from_sq then
        result := (st^.castle_rights and (cardinal(us) + 1) <> 0)
              and (bb.occupied and Empty_OO[us] = 0)
              and (st^.attacks[they] and Safe_OO[us] = 0)
      else
        result := (st^.castle_rights and (cardinal(us) shl 2 + 4) <> 0)
              and (bb.occupied and Empty_OOO[us] = 0)
              and (st^.attacks[they] and Safe_OOO[us] = 0)
              and ((start_file[START_ROOK_A] <> FILE_B)
                   or (queens_rooks(they) and SquareBB[relative(A1, us)] <> 0));
      exit;
    end;
    result := (squares[to_sq] = capture_victim(move))
          and (st^.attacks[they] and SquareBB[to_sq] = 0);
    exit;
  end;

  if squares[to_sq] <> capture_victim(move) then exit;

  if is_doublepush(move) then
    if squares[TSquare((cardinal(from_sq) + cardinal(to_sq)) shr 1)] <> NOPIECE then
      exit;

  if is_sliding(move) then
    if BetweenBB[from_sq, to_sq] and bb.occupied <> 0 then exit;

  if not is_set(st^.pins[they], from_sq) then begin
    result := true;
    exit;
  end;

  if type_of(p) = KNIGHT then exit;

  ksq := kingsq[us];
  result := ((Direction[ksq, from_sq] xor Direction[ksq, to_sq]) and $0f) = 0;
end;

function TBoard.hm_is_legal_evasion(move: TMove): boolean;
var
  from_sq, to_sq, chk_sq: TSquare;
  checkers, shadow: TBitboard;
  p: TPiece;
begin
  assert(st^.checkersBB <> 0, 'is_legal_evasion() called while not in check!');

  to_sq := target_sq(move);
  p := piece_moved(move);

  if (side_of(p) <> us) or (squares[source_sq(move)] <> p) then
  begin
    result := false;
    exit;
  end;

  checkers := st^.checkersBB;
  chk_sq := find_lsb(checkers);

  if is_king_move(move) then begin
    if is_castle(move) or (st^.attacks[they] and SquareBB[to_sq] <> 0) then begin
      result := false;
      exit;
    end;
    clear_lsb(checkers);
    if squares[chk_sq] <= B_PAWN then
      shadow := BB_Full
    else
      shadow := ShadowBB[chk_sq, kingsq[us]];
    if checkers <> BB_Empty then begin
      chk_sq := find_lsb(checkers);
      if squares[chk_sq] > B_PAWN then
        shadow := shadow and ShadowBB[chk_sq, kingsq[us]];
    end;
    result := is_set(shadow, to_sq);
    exit;
  end;

  from_sq := source_sq(move);

  if (checkers and (checkers-1) <> 0) or (st^.pins[they] and SquareBB[from_sq] <> 0) then begin
    result := false;
    exit;
  end;

  if is_enpassant(move) then begin
    result := (to_sq = st^.epsq) and (squares[chk_sq] = piece(PAWN, they));
    exit;
  end;

  if squares[to_sq] <> capture_victim(move) then begin
    result := false;
    exit;
  end;

  if is_doublepush(move) then
    if squares[TSquare((cardinal(from_sq) + cardinal(to_sq)) shr 1)] <> NOPIECE then begin
      result := false;
      exit;
    end;

  if is_sliding(move) and (BetweenBB[from_sq, to_sq] and bb.occupied <> 0) then begin
    result := false;
    exit;
  end;

  result := (to_sq = chk_sq) or (BetweenBB[chk_sq, kingsq[us]] and SquareBB[to_sq] <> 0);
end;

function TBoard.hm_is_ok(move: TMove): boolean;
begin
  if (st^.checkersBB = 0) then
    result := hm_is_legal(move)
  else
    result := hm_is_legal_evasion(move)
end;

function TBoard.is_dc(move: TMove): boolean;
var sq: TSquare;
begin
  sq := source_sq(move);
  result := (st^.pins[us] and SquareBB[sq] <> 0)
    and (Direction[kingsq[they], sq] <> Direction[kingsq[they], target_sq(move)]);
end;

function TBoard.allow_null(): boolean;
begin
  result := st^.flags and (cardinal(us)+1) <> 0;
end;

function TBoard.pawn_is_passed(sq: TSquare): boolean;
begin
  result := PassedMaskBB[us, sq] and bb.pawns[they] = 0;
end;

function TBoard.pawn_is_passed(sq: TSquare; side: TSide): boolean;
begin
  result := PassedMaskBB[side, sq] and bb.pawns[flip(side)] = 0;
end;

function TBoard.queens_rooks(side: TSide): TBitboard;
begin
  result := bb.rooks[side] or bb.queens[side];
end;

function TBoard.queens_bishops(side: TSide): TBitboard;
begin
  result := bb.bishops[side] or bb.queens[side];
end;

function TBoard.minors(side: TSide): TBitboard;
begin
  result := bb.knights[side] or bb.bishops[side];
end;

function TBoard.bishop_attacks(sq: TSquare): TBitboard;
begin
  result := uBitboard.bishop_attacks(sq, bb.occupied);
end;

function TBoard.rook_attacks(sq: TSquare): TBitboard;
begin
  result := uBitboard.rook_attacks(sq, bb.occupied);
end;

function TBoard.queen_attacks(sq: TSquare): TBitboard;
begin
  result := uBitboard.queen_attacks(sq, bb.occupied);
end;

function TBoard.validate(): boolean;
var
  sq: TSquare;
  p: TPiece;
begin
  //WriteLn({$I %FILE%}, ' ', {$I %LINE%});
  st^.psqval := 0;
  st^.mtrlkey := 0;
  st^.flags := 0;
  st^.phase.int_phase := 0;

  fillchar(piece_list, sizeof(piece_list), char(-1));
  fillchar(list_index, sizeof(list_index), 0);

  for sq := A1 to H8 do begin
    p := squares[sq];
    if (p <> NOPIECE) then begin
      if (p > B_PAWN) then
        if (p <= B_KING) then
          kingsq[side_of(p)] := sq
        else begin
          piece_list_add(p, sq);
          inc(count.np[side_of(p)]);
        end;
      add_piece(p, sq);
    end;
  end;

  bb.occupied := bb.white or bb.black;
  bb.empty := not bb.occupied;

  result := false;
  if ((bb.w_pawns or bb.b_pawns) and (BB_Rank1 or BB_Rank8)) <> 0 then
    exit;

  with count do begin
    if (bk <> 1) or (bp > 8) or (bp + b_np > 16) or (bp + bq > 9) then exit;
    if (wk <> 1) or (wp > 8) or (wp + b_np > 16) or (wp + wq > 9) then exit;
    if (bp + bn > 10) or (bp + bb > 10) or (bp + br > 10) then exit;
    if (wp + wn > 10) or (wp + wb > 10) or (wp + wr > 10) then exit;
  end;

  init_castle_data();

  if kingsq[WHITE] <> square(RANK_1, start_file[START_KING]) then
    st^.castle_rights := st^.castle_rights and not CR_W_BOTH
  else begin
    if squares[square(RANK_1, start_file[START_ROOK_H])] <> W_ROOK then
      st^.castle_rights := st^.castle_rights and not CR_W_SHORT;
    if squares[square(RANK_1, start_file[START_ROOK_A])] <> W_ROOK then
      st^.castle_rights := st^.castle_rights and not CR_W_LONG;
  end;

  if (kingsq[BLACK]) <> square(RANK_8, start_file[START_KING]) then
    st^.castle_rights := st^.castle_rights and not CR_B_BOTH
  else begin
    if squares[square(RANK_8, start_file[START_ROOK_H])] <> B_ROOK then
      st^.castle_rights := st^.castle_rights and not CR_B_SHORT;
    if squares[square(RANK_8, start_file[START_ROOK_A])] <> B_ROOK then
      st^.castle_rights := st^.castle_rights and not CR_B_LONG;
  end;

  st^.bishop_flags := 0;
  if (bb.w_bishops and BB_Light) <> 0 then
    inc(st^.bishop_flags, BF_WHITE_LIGHT);
  if (bb.w_bishops and BB_Dark) <> 0 then
    inc(st^.bishop_flags, BF_WHITE_DARK);
  if (bb.b_bishops and BB_Light) <> 0 then
    inc(st^.bishop_flags, BF_BLACK_LIGHT);
  if (bb.b_bishops and BB_Dark) <> 0 then
    inc(st^.bishop_flags, BF_BLACK_DARK);

  st^.hashkey := calc_hash_key();
  st^.pawnkey := calc_pawn_key();
  st^.mtrlkey := calc_mtrl_key();

  fillchar(zobrist_history, sizeof(zobrist_history), 0);
  evaluator^.evaluate(self, 0, 0, 0);
  //WriteLn({$I %FILE%}, ' ', {$I %LINE%});
  result := true;
end;

function TBoard.calc_hash_key(): TKey;
var
  b: TBitboard;
  sq: TSquare;
begin
  result := KeyCastle[st^.castle_rights] xor KeyStm[us];
  if (st^.epsq <> NO_EP) then
    result := result xor KeyEP[file_of(st^.epsq)];
  b := bb.occupied;
  while b <> 0 do begin
    sq := find_lsb(b);
    result := result xor KeyPieces[squares[sq], sq];
    clear_lsb(b);
  end;
end;

function TBoard.calc_pawn_key(): TKey;
var
  b: TBitboard;
begin
  result := KeyCastle[st^.castle_rights]
        xor KeyPieces[W_KING, kingsq[WHITE]]
        xor KeyPieces[B_KING, kingsq[BLACK]];

  b := bb.w_pawns;
  while (b <> 0) do begin
    result := result xor KeyPieces[W_PAWN, find_lsb(b)];
    clear_lsb(b);
  end;

  b := bb.b_pawns;
  while (b <> 0) do begin
    result := result xor KeyPieces[B_PAWN, find_lsb(b)];
    clear_lsb(b);
  end;
end;

function TBoard.calc_mtrl_key(): TKey32;
var
  p: TPiece;
  i: integer;
begin
  result := 0;
  for p := W_PAWN to B_QUEEN do
    for i := 1 to count.by_piece[p] do
      result := result xor KeyMtrl[p, i];
end;

function TBoard.start_square(sf: TStartFile): TSquare;
begin
  result := square(relative_rank(RANK_1, us), start_file[sf]);
end;

function TBoard.opp_bishops(): boolean;
begin
  result := st^.bishop_flags in [BF_OPPOSITE_1, BF_OPPOSITE_2];
end;

procedure TBoard.init_castle_data();
var
  k_from, r_from, sq: TSquare;
  k: TPiece;
  i: integer;
  cd: PCastleData;
  bb_safe, bb_empty: TBitboard;
begin
  fillchar(castle_mask, sizeof(castle_mask), byte(not CR_NONE));
  sq := square(RANK_1, start_file[START_ROOK_A]);
  castle_mask[sq] := byte(not CR_W_LONG); inc(sq, 7*8);
  castle_mask[sq] := byte(not CR_B_LONG);

  sq := square(RANK_1, start_file[START_ROOK_H]);
  castle_mask[sq] := byte(not CR_W_SHORT); inc(sq, 7*8);
  castle_mask[sq] := byte(not CR_B_SHORT);

  sq := square(RANK_1, start_file[START_KING]);
  castle_mask[sq] := byte(not CR_W_BOTH); inc(sq, 7*8);
  castle_mask[sq] := byte(not CR_B_BOTH);

  for i := 0 to 3 do begin
    cd := @castle_data[i];
    k_from := square(RANK_1, start_file[START_KING]);
    k := W_KING;
    cd^.rook := W_ROOK;

    if (i and 1) <> 0 then begin
      cd^.k_to := C1;
      cd^.r_to := D1;
      r_from := square(RANK_1, start_file[START_ROOK_A]);
    end
    else begin
      cd^.k_to := G1;
      cd^.r_to := F1;
      r_from := square(RANK_1, start_file[START_ROOK_H]);
    end;

    if (i and 2) <> 0 then begin
      k_from := flip(k_from); r_from := flip(r_from);
      cd^.k_to := flip(cd^.k_to); cd^.r_to := flip(cd^.r_to);
      cd^.rook := flip(cd^.rook); k := flip(k);
    end;

    cd^.king_xor := SquareBB[k_from] xor SquareBB[cd^.k_to];
    cd^.rook_xor := SquareBB[r_from] xor SquareBB[cd^.r_to];
    cd^.psq_delta := PSQ[k, cd^.k_to] - PSQ[k, k_from]
      + PSQ[cd^.rook, cd^.r_to] - PSQ[cd^.rook, r_from];
    cd^.hash_xor := KeyPieces[cd^.rook, r_from] xor KeyPieces[cd^.rook, cd^.r_to]
      xor KeyPieces[k, k_from] xor KeyPieces[k, cd^.k_to] xor TKey(KeyWtm);

    bb_safe := 0; bb_empty := 0;

    for sq := min(k_from, cd^.k_to) to max(k_from, cd^.k_to) do begin
      set_bit(bb_safe, sq);
      if (sq <> k_from) and (sq <> r_from) then
        set_bit(bb_empty, sq);
    end;

    for sq := min(r_from, cd^.r_to) to max(r_from, cd^.r_to) do begin
      if (sq <> k_from) and (sq <> r_from) then
        set_bit(bb_empty, sq);
    end;

    if (i and 1) = 0 then begin
      Empty_OO[TSide((i and 2) shr 1)] := bb_empty;
      Safe_OO[TSide((i and 2) shr 1)] := bb_safe;
    end
    else begin
      Empty_OOO[TSide((i and 2) shr 1)] := bb_empty;
      Safe_OOO[TSide((i and 2) shr 1)] := bb_safe;
    end;
  end;
end;

{$ifdef DEBUG_BOARD}
function TBoard.debug(): boolean;
var
  chkbb, b: TBitboard;
  sq: TSquare;
  p, p2: TPiece;
  s: TSide;
  pcnt: TPieceCount;
  lp: PShortInt;
  c: array [0..7] of AnsiChar;
  ph: array [TSide] of integer;
  i: integer;
const
  phasetab: array [TPieceType] of integer = ( 0, 0, 0, 1, 1, 3, 6, 0 );
begin
  result := false;
  for p := W_PAWN to B_QUEEN do begin
    for p2 := W_PAWN to B_QUEEN do
      if p <> p2 then begin
        b := bb.piece[p] and bb.piece[p2];
        if b <> 0 then begin
          sq := find_lsb(b);
          printf('more than 1 piece on square %s' + LineEnding, square_to_str(sq, c));
          exit;
        end;
      end;
    if popcnt(bb.piece[p]) <> count.by_piece[p] then begin
      printf('bad popcount for piece %c (%d != %d)' + LineEnding, piece_to_char(p),
        popcnt(bb.piece[p]), count.by_piece[p]);
      exit;
    end;
  end;

  for sq := A1 to H8 do begin
    p := squares[sq];
    if p = NOPIECE then begin
      if is_set(bb.occupied, sq) then begin
        printf('occupied contains empty square %s' + LineEnding, square_to_str(sq, c));
        exit;
      end;
    end
    else begin
      if not is_set(bb.piece[p], sq) then begin
        printf('piece %c is not in its bitboard' + LineEnding, piece_to_char(p));
        exit;
      end;
      if not is_set(bb.occupied, sq) then begin
        printf('piece %c is missing from occupied' + LineEnding, piece_to_char(p));
        exit;
      end;
      if not is_set(bb.pieces[side_of(p)], sq) then begin
        printf('piece %c is missing from side_bb' + LineEnding, piece_to_char(p));
        exit;
      end;
    end;
  end;

  b := (bb.w_pawns or bb.w_pawns) and (BB_Rank1 or BB_Rank8);
  if b <> 0 then begin
    printf('pawn on illegal square %s' + LineEnding, square_to_str(find_lsb(b), c));
    exit;
  end;

  chkbb := attackers(kingsq[us], they);
  if chkbb <> st^.checkersBB then begin
    printf('bad checkersBB' + LineEnding);
    printf('incremental:' + LineEnding); print_bb(st^.checkersBB);
    printf('full:' + LineEnding); print_bb(chkbb);
    exit;
  end;

  if st^.attacks[us] and bb.king[they] <> 0 then begin
    printf('opponent''s king in check!' + LineEnding);
    print_bb(st^.attacks[us]);
    exit;
  end;

  if (st^.castle_rights < 0) or (st^.castle_rights > 15) then begin
    printf('bogus castle rights (%d)' + LineEnding, st^.castle_rights);
    exit;
  end;

  if (st^.bishop_flags < 0) or (st^.bishop_flags > 15) then begin
    printf('bogus bishop flags (%d)' + LineEnding, st^.castle_rights);
    exit;
  end;

  if calc_hash_key() <> st^.hashkey then begin
    printf('bad hash key' + LineEnding);
    exit;
  end;

  if calc_pawn_key() <> st^.pawnkey then begin
    printf('bad pawn key' + LineEnding);
    exit;
  end;

  if calc_mtrl_key() <> st^.mtrlkey then begin
    printf('bad mtrl key' + LineEnding);
    exit;
  end;

  clear_piece_counts(pcnt);
  ph[WHITE] := 0;
  ph[BLACK] := 0;

  for sq := A1 to H8 do begin
    p := squares[sq];
    if p = NOPIECE then continue;
    inc(ph[side_of(p)], phasetab[type_of(p)]);
    inc(pcnt.by_piece[p]);
    if p >= W_KNIGHT then
      inc(pcnt.np[side_of(p)]);
  end;

  for p := W_PAWN to B_QUEEN do begin
    if pcnt.by_piece[p] <> count.by_piece[p] then begin
      printf('bad piece cnt for piece %c (%d != %d)' + LineEnding,
        piece_to_char(p), pcnt.by_piece[p], count.by_piece[p]);
      exit;
    end;
  end;

  for s := WHITE to BLACK do begin
    if ph[s] <> st^.phase.by_side[s] then begin
      printf('bad phase for side %c (%d != %d)' + LineEnding,
        color_to_char(s), ph[s], st^.phase.by_side[s]);
      exit;
    end;
    if pcnt.np[s] <> count.np[s] then begin
      printf('bad piece cnt for side %c (%d != %d)' + LineEnding,
        color_to_char(s), pcnt.np[s], count.np[s]);
      exit;
    end;
  end;

  if ph[WHITE] + ph[BLACK] <> st^.phase.raw then begin
    printf('bad total phase (%d != %d + %d)' + LineEnding,
      st^.phase.raw, ph[WHITE], ph[BLACK]);
    exit;
  end;

  for p := W_PAWN to B_QUEEN do begin
    lp := @piece_list.pl[list_start[p]];
    i := 0;
    while (lp^ <> -1) do begin
      sq := TSquare(lp^);
      if squares[sq] <> p then begin
        printf('bad entry in %c%c piece list (%c != %c)' + LineEnding, color_to_char(side_of(p)),
          piece_to_char(p), piece_to_char(squares[sq]), piece_to_char(p));
        exit;
      end;
      inc(i);
      inc(lp);
    end;
    if (p >= W_KNIGHT) and (i <> count.by_piece[p]) then begin
      printf('a %c%c is missing from piece list' + LineEnding, color_to_char(side_of(p)),
        piece_to_char(p));
      exit;
    end;
  end;

  for sq := A1 to H8 do begin
    if (type_of(squares[sq]) = PAWN) or (type_of(squares[sq]) = KING) then
      if (list_index[sq] <> 0) then begin
        printf('nonzero list_index for square %s piece %c' + LineEnding,
          square_to_str(sq, c), piece_to_char(squares[sq]));
        exit;
      end;
  end;

  result := true;
end;

procedure TBoard.print_move_history();
var
  ml: TMoveList;
  u: PStateInfo;
  c: TMoveString;
  i: integer;
begin
  u := st;
  i := gameply;
  while i <> 0 do begin
    ml[i].move := u^.lastmove;
    u := u^.prev;
    dec(i);
  end;
  for i := 1 to gameply do
    printf('%s; ', move_to_notation(ml[i].move, c));
end;

{$endif}

{$ifdef LOG_MOVES}
procedure TBoard.log_make_move(move: TMove);
var
  f: TextFile;
  sx: PStateInfo;
  cmove: TMoveString;
  ms: array [0..127] of TMove;
  x: integer;
begin
  AssignFile(f, 'makemove.log');
  {$I-}
  Append(f);
  {$I+}
  if IOResult = 0 then
  begin
    sx := st;
    for x := 0 to ply - 1 do
    begin
      ms[x] := sx^.lastmove;
      sx := sx^.prev;
    end;
    for x := 1 to ply do
    begin
      write(f, move_to_string(ms[ply-x], cmove));
      write(f, #9);
    end;
    writeln(f, move_to_string(move, cmove));
    CloseFile(f);
  end;
end;
{$endif}

end.
