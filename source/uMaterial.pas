
unit uMaterial;
{$Z4}

interface

uses
  uBoard, uPiece, uScore, uSquare, uZobrist;

{$include platform.inc}

const
  MCACHE_SIZE = 2048;

type
  PMtrlInfo = ^TMtrlInfo;

  TScales = array [TSide] of TScale;

  TEgEvalFunc = function (var board: TBoard; mi: PMtrlInfo;
    out eval: TScore; var scale: TScales): boolean;

  TKeyPair = array [TSide] of TKey32;

  TEgFnID = (
    EG_NONE,
    EG_KPK,     EG_KKP,
    EG_KXK,     EG_KKX,
    EG_KBNK,    EG_KKBN,
    EG_KBKP,    EG_KPKB,
    EG_KRKN,    EG_KNKR,
    EG_KRKP,    EG_KPKR,
    EG_KQKP,    EG_KPKQ,
    EG_KBPKB,   EG_KBKBP,
    EG_KBPPKB,  EG_KBKBPP,
    EG_KBKPP,   EG_KPPKB,
    EG_KRPKR,   EG_KRKRP,
    EG_KRPPKRP, EG_KRPKRPP,
    EG_KQKRP,   EG_KRPKQ,
    EG_KBBKN,   EG_KNKBB,
    EG_KM_W,    EG_KM_B,
    EG_KP,
    EG_FIDE_DRAW,
    EG_SIMPLE_DRAW
  );

  TMtrlInfo = record
    mtrl_key: TKey32;
    flags: cardinal;
    mtrl_score: smallint;
    space_weight: byte;
    eg_fn_id: byte;
    side: byte;
    phase: byte;
    scale: array [TSide] of byte;
  end;

  TMtrlValues = record
    value_q, value_r, value_b, value_n, value_p: integer;
    bishop_pair: integer;
    np_bonus: integer;
    rp_penalty: integer;
    bn_vs_rp: integer;
    redundant_r: integer;
    redundant_qr: integer;
  end;

  TMtrlTable = array [0..MCACHE_SIZE-1] of TMtrlInfo;
  PMtrlTable = ^TMtrlTable;

procedure init_mtrl();
function get_mtrl_info(var board: TBoard; var mtrl_table: TMtrlTable): PMtrlInfo;

var
  mtrl_values: array [TMtrlStage] of TMtrlValues;
  mkey_knnk, mkey_kbpkn: TKeyPair;

const mtrl_data: array [0..32, 0..10] of word = (
 ( 3004, 1533, 910, 875, 298, 118, 13,  0, 13, 82, 41 ),
 ( 2964, 1515, 899, 864, 293, 117, 12,  1, 14, 81, 40 ),
 ( 2923, 1496, 888, 854, 289, 116, 12,  1, 16, 79, 40 ),
 ( 2882, 1477, 877, 843, 284, 114, 12,  2, 18, 78, 39 ),
 ( 2841, 1458, 866, 832, 280, 113, 12,  3, 19, 77, 38 ),
 ( 2800, 1439, 855, 821, 275, 112, 11,  3, 21, 76, 38 ),
 ( 2759, 1420, 844, 811, 271, 110, 11,  4, 22, 74, 37 ),
 ( 2719, 1402, 833, 800, 266, 109, 11,  4, 24, 73, 36 ),
 ( 2678, 1383, 822, 789, 262, 108, 10,  5, 26, 72, 36 ),
 ( 2653, 1367, 817, 783, 259, 106, 10,  5, 26, 70, 35 ),
 ( 2629, 1351, 812, 777, 256, 105, 10,  6, 27, 69, 35 ),
 ( 2604, 1336, 808, 770, 253, 103,  9,  6, 28, 68, 34 ),
 ( 2580, 1320, 803, 764, 250, 102,  9,  6, 29, 67, 33 ),
 ( 2555, 1304, 798, 758, 247, 101,  9,  7, 30, 65, 33 ),
 ( 2531, 1288, 793, 752, 244,  99,  8,  7, 30, 64, 32 ),
 ( 2506, 1273, 789, 746, 241,  98,  8,  7, 31, 63, 31 ),
 ( 2482, 1257, 784, 740, 238,  97,  8,  8, 32, 61, 31 ),
 ( 2457, 1241, 779, 733, 235,  95,  7,  8, 33, 60, 30 ),
 ( 2433, 1226, 774, 727, 232,  94,  7,  8, 34, 59, 29 ),
 ( 2408, 1210, 770, 721, 229,  93,  7,  9, 34, 58, 29 ),
 ( 2384, 1194, 765, 715, 226,  91,  6,  9, 35, 56, 28 ),
 ( 2359, 1178, 760, 709, 223,  90,  6,  9, 36, 55, 28 ),
 ( 2335, 1163, 755, 703, 220,  89,  6, 10, 37, 54, 27 ),
 ( 2310, 1147, 751, 696, 217,  87,  5, 10, 38, 52, 26 ),
 ( 2286, 1131, 746, 690, 214,  86,  5, 10, 38, 51, 26 ),
 ( 2261, 1117, 741, 686, 211,  85,  4, 11, 40, 50, 25 ),
 ( 2237, 1103, 736, 681, 208,  83,  4, 11, 42, 49, 24 ),
 ( 2212, 1089, 732, 676, 205,  82,  3, 11, 43, 47, 24 ),
 ( 2188, 1075, 727, 672, 202,  81,  3, 12, 45, 46, 23 ),
 ( 2163, 1061, 722, 667, 199,  79,  2, 12, 46, 45, 22 ),
 ( 2139, 1046, 717, 663, 196,  78,  1, 12, 48, 44, 22 ),
 ( 2115, 1032, 713, 658, 193,  77,  1, 12, 50, 42, 21 ),
 ( 2090, 1018, 708, 653, 190,  75,  0, 13, 51, 41, 20 )
);

implementation

uses
  uGtb, uSearchDef;

type
  TEgEvalEntry = packed record
    mtrl_key: TKey32;
    eg_fn_id: TEgFnID;
    flags: cardinal;
  end;

var
  eg_fn_tab: array [0..31] of TEgEvalEntry;
  eg_fn_cnt: integer;

procedure make_mtrl_keys(matconfig: PAnsiChar; out keys: TKeyPair);
var
  piececnt: TPieceCount;
  side: TSide;
  p: TPiece;
begin
  side := BLACK;
  keys[WHITE] := 0; keys[BLACK] := 0;
  clear_piece_counts(piececnt);
  while matconfig^ <> #0 do begin
    p := char_to_piece(matconfig^);
    if type_of(p) = KING then side := flip(side);
    if side = BLACK then p := flip(p);
    inc(piececnt.by_piece[p]);
    keys[WHITE] := keys[WHITE] xor KeyMtrl[p, piececnt.by_piece[p]];
    keys[BLACK] := keys[BLACK] xor KeyMtrl[flip(p), piececnt.by_piece[p]];
    inc(matconfig);
  end;
end;

procedure add_eg_fn(matconfig: PAnsiChar; w_id, b_id: TEgFnID; f: integer);
var
  keys: TKeyPair;
begin
  make_mtrl_keys(matconfig, keys);
  with eg_fn_tab[eg_fn_cnt] do begin
    mtrl_key := keys[WHITE];
    eg_fn_id := w_id;
    flags := f;
  end;
  inc(eg_fn_cnt);

  with eg_fn_tab[eg_fn_cnt] do begin
    mtrl_key := keys[BLACK];
    eg_fn_id := b_id;
    flags := f;
  end;
  inc(eg_fn_cnt);
end;

procedure init_mtrl();
var
  ph: TMtrlStage;
begin
  eg_fn_cnt := 0;

  for ph := STAGE_ENDGAME to STAGE_OPENING do
    with mtrl_values[ph] do begin
      value_q      := mtrl_data[ph, 0];
      value_r      := mtrl_data[ph, 1];
      value_b      := mtrl_data[ph, 2];
      value_n      := mtrl_data[ph, 3];
      value_p      := mtrl_data[ph, 4];
      bishop_pair  := mtrl_data[ph, 5];
      np_bonus     := mtrl_data[ph, 6];
      rp_penalty   := mtrl_data[ph, 7];
      bn_vs_rp     := mtrl_data[ph, 8];
      redundant_r  := mtrl_data[ph, 9];
      redundant_qr := mtrl_data[ph, 10];
    end;

  make_mtrl_keys('KNNK' , mkey_knnk);
  make_mtrl_keys('KBPKN', mkey_kbpkn);

  add_eg_fn('KPK'    , EG_KPK    , EG_KKP     , FLAG_SKIP_EVAL);
  add_eg_fn('KBNK'   , EG_KBNK   , EG_KKBN    , FLAG_SKIP_EVAL);
  add_eg_fn('KBKP'   , EG_KBKP   , EG_KPKB    , 0);
  add_eg_fn('KRKP'   , EG_KRKP   , EG_KPKR    , 0);
  add_eg_fn('KRKN'   , EG_KRKN   , EG_KNKR    , FLAG_SKIP_EVAL);
  add_eg_fn('KQKP'   , EG_KQKP   , EG_KPKQ    , 0);
  add_eg_fn('KBPKB'  , EG_KBPKB  , EG_KBKBP   , 0);
  add_eg_fn('KBKPP'  , EG_KBKPP  , EG_KPPKB   , 0);
  add_eg_fn('KRPKR'  , EG_KRPKR  , EG_KRKRP   , 0);
  add_eg_fn('KBBKN'  , EG_KBBKN  , EG_KNKBB   , FLAG_SKIP_EVAL);
  add_eg_fn('KRPPKRP', EG_KRPPKRP, EG_KRPKRPP , 0);

end;


function get_mtrl_scale(var board: TBoard; side: TSide): integer; forward;
function get_win_chance(var board: TBoard; mi: PMtrlInfo): TScale; forward;

function get_mtrl_info(var board: TBoard; var mtrl_table: TMtrlTable): PMtrlInfo;
var
  count: ^TPieceCount;
  w_np, b_np, w_minor, b_minor: integer;
  w_phase, b_phase: integer;
  total_pcs, mpc: integer;
  mtrl_score: TScorePair;
  mult: integer;
  mi: PMtrlInfo;
  st: PStateInfo;
  pure_key, full_key: TKey32;
  i: integer;
begin
  st := board.st;
  pure_key := st^.mtrlkey;
  full_key := st^.mtrlkey xor cardinal(KeyCastle[integer(st^.bishop_flags)]);
  mi := @mtrl_table[full_key and (MCACHE_SIZE-1)];
  result := mi;
  if full_key = mi^.mtrl_key then exit;

  with mi^ do begin
    mtrl_key := full_key;
    scale[WHITE] := SCALE_NORMAL;
    scale[BLACK] := SCALE_NORMAL;
    eg_fn_id := byte(EG_NONE);
    space_weight := 0;
    flags := 0;
  end;

  count := @board.count;

  w_np := count^.w_np;
  b_np := count^.b_np;
  w_minor := count^.wn + count^.wb;
  b_minor := count^.bn + count^.bb;

  mi^.phase := board.st^.phase.raw;
  if mi^.phase >= 2*6 + 4*3 + 2*1 then begin
    if mi^.phase >= STAGE_OPENING then
      mi^.phase := STAGE_OPENING;
    mpc := w_minor + b_minor;
    mi^.space_weight := mpc * mpc;
  end;

  with mtrl_values[mi^.phase], count^ do begin
    mtrl_score := value_q * (wq - bq)
                + value_r * (wr - br)
                + value_b * (wb - bb)
                + value_n * (wn - bn)
                + value_p * (wp - bp);

    if st^.bishop_flags and BF_WHITE_PAIR = BF_WHITE_PAIR then
      inc(mtrl_score, bishop_pair);

    if st^.bishop_flags and BF_BLACK_PAIR = BF_BLACK_PAIR then
      dec(mtrl_score, bishop_pair);

    if wq + wr >= 2 then dec(mtrl_score, redundant_qr);
    if bq + br >= 2 then inc(mtrl_score, redundant_qr);
    if wr >= 2 then dec(mtrl_score, redundant_r);
    if br >= 2 then inc(mtrl_score, redundant_r);

    dec(mtrl_score, (wp - 5) * wr * rp_penalty);
    inc(mtrl_score, (bp - 5) * br * rp_penalty);
    inc(mtrl_score, (wp - 5) * wn * np_bonus);
    dec(mtrl_score, (bp - 5) * bn * np_bonus);

    if w_minor <> b_minor then
      if w_minor > b_minor then
        inc(mtrl_score, bn_vs_rp)
      else
        dec(mtrl_score, bn_vs_rp);
  end;

  mult := 128;
  if mtrl_score > 0 then begin
    if count^.wp <= 1 then
      mult := get_mtrl_scale(board, WHITE)
  end
  else begin
    if count^.bp <= 1 then
      mult := get_mtrl_scale(board, BLACK);
  end;
  mi^.mtrl_score := mtrl_score * mult div 128;

  if (w_np + b_np > 4) or (w_np <> b_np) then
    mi^.scale[WHITE] := 128
  else
    mi^.scale[WHITE] := get_win_chance(board, mi);

  mi^.scale[BLACK] := mi^.scale[WHITE];
  w_phase := st^.phase.by_side[WHITE];
  b_phase := st^.phase.by_side[BLACK];

  if w_phase > 1 then
    mi^.flags := mi^.flags or FLAG_W_NULL
  else
    if w_phase = 1 then begin
      mi^.flags := mi^.flags or FLAG_W_KM;
      if (count^.wp > 4) or (count^.bp > 4) then
        mi^.flags := mi^.flags or FLAG_W_NULL;
    end;

  if b_phase > 1 then
    mi^.flags := mi^.flags or FLAG_B_NULL
  else
    if b_phase = 1 then begin
      mi^.flags := mi^.flags or FLAG_B_KM;
      if (count^.wp > 4) or (count^.bp > 4) then
        mi^.flags := mi^.flags or FLAG_B_NULL;
    end;

  with count^ do
    total_pcs := w_np + b_np + wp + bp;

  if total_pcs <= gtb_num_pieces then begin
    if gtb_available and (use_gtb and TB_IN_SEARCH <> 0) then
      mi^.flags := mi^.flags or FLAG_PROBE_TB;
    for i := 0 to eg_fn_cnt - 1 do begin
      if eg_fn_tab[i].mtrl_key = pure_key then begin
        mi^.eg_fn_id := byte(eg_fn_tab[i].eg_fn_id);
        mi^.flags := mi^.flags or (eg_fn_tab[i].flags or FLAG_AVOID_LAZY);
        exit;
      end;
    end;
  end;

  if mi^.phase = 0 then begin
    mi^.eg_fn_id := ord(EG_KP);
    mi^.flags := mi^.flags or (FLAG_NO_UPDATE or FLAG_SKIP_EVAL);
    exit;
  end;

  if (w_phase = 0) and (b_phase >= 2) then
    if count^.wp = 0 then begin
      mi^.eg_fn_id := ord(EG_KKX);
      mi^.flags := mi^.flags or (FLAG_NO_UPDATE or FLAG_SKIP_EVAL);
      exit;
    end;

  if (w_phase = 1) and (mi^.mtrl_score > 0) then
    mi^.flags := mi^.flags or FLAG_AVOID_LAZY;

  if (b_phase = 0) and (w_phase >= 2) then
    if count^.bp = 0 then begin
      mi^.eg_fn_id := ord(EG_KXK);
      mi^.flags := mi^.flags or (FLAG_NO_UPDATE or FLAG_SKIP_EVAL);
      exit;
    end;

  if (b_phase = 1) and (mi^.mtrl_score < 0) then
    mi^.flags := mi^.flags or FLAG_AVOID_LAZY;

  with count^ do
    if (wp = 0) and (bp = 0) and (wr + br = 0) and (wq + bq = 0) then
      if (w_minor <= 2) and (b_minor <= 2) then begin
        mi^.eg_fn_id := ord(EG_SIMPLE_DRAW);
        mi^.flags := mi^.flags or (FLAG_NO_UPDATE or FLAG_SKIP_EVAL);
      end;


  if (count^.wp = 0) then begin
    if (w_phase = 6) and (count^.wq = 1) then
      if (b_phase = 3) and (count^.br = 1) and (count^.bp <= 2) then begin
        mi^.flags := mi^.flags and (not FLAG_B_NULL);
        mi^.eg_fn_id := ord(EG_KQKRP);
        exit;
      end;
    if (w_phase = 2) and (count^.wb = 2) then
      if (st^.bishop_flags and BF_WHITE_PAIR <> BF_WHITE_PAIR) then
        mi^.scale[WHITE] := SCALE_ZERO;
  end;

  if (count^.bp = 0) then begin
    if (b_phase = 6) and (count^.bq = 1) then
      if (w_phase = 3) and (count^.wr = 1) and (count^.wp <= 2) then begin
        mi^.flags := mi^.flags and (not FLAG_W_NULL);
        mi^.eg_fn_id := ord(EG_KRPKQ);
        exit;
      end;
    if (b_phase = 2) and (count^.bb = 2) then
      if (st^.bishop_flags and BF_WHITE_PAIR <> BF_WHITE_PAIR) then
        mi^.scale[BLACK] := SCALE_ZERO;
  end;
end;

function get_mtrl_scale(var board: TBoard; side: TSide): integer;
var
  xside: TSide;
  _wp, _wn, _wb, _wr, _wq: integer;
  _bp, _bn, _bb, _br, _bq: integer;
  w_phase, b_phase: integer;
  w_eval, b_eval: integer;
begin
  xside := flip(side);
  with board.count do begin
    _wp := pawn  [side]; _bp := pawn  [xside];
    _wn := knight[side]; _bn := knight[xside];
    _wb := bishop[side]; _bb := bishop[xside];
    _wr := rook  [side]; _br := rook  [xside];
    _wq := queen [side]; _bq := queen [xside];
  end;

  result := 128;

  w_phase := 4 * _wq + 2 * _wr + _wb + _wn;
  b_phase := 4 * _bq + 2 * _br + _bb + _bn;

  if _wp = 0 then begin
    if (w_phase <= 1) then result := 0;          // lone king or only one minor
    if (w_phase = 2) then                        // two minors or one rook
      case b_phase of
        0: if (_wn = 2) then result := 0;        // KNNK
        1: begin
             result := 13;
             if (_bn = 1) then begin
               if (_wb = 2) then result := 102;    // KBBKN
               if (_wr = 1) then result := 26;     // KRKN
             end;
           end;
        2: result := 13;                         // KRKR, KmmKR, KRKmm, KmmKmm
      end;

    if (w_phase = 3) then begin                  // one minor + rook
      if (_wr = 1) then begin

        if (b_phase = 2) then begin
          if (_br = 1) then                      // opponent has only a rook (KRmKR)
            if (_wn = 1) or (_wb = 1) then
              result := 13;                      // KRNKR, KRBKR

          if (_br = 0) then begin                // opponent has two minors
            result := 26;
            if (_wb = 1) and (_bn = 2) then
              result := 77;                      // KRBKNN
            if (_bn = 1) then begin
              if board.opp_bishops() then
                result := 90                     // KRBKNB with opposite colored bishops
              else
                result := 26;                    // KRBKNB with same colored bishops
            end;
          end;
        end;

        if (b_phase = 3) then result := 26;      // opponent has 3 minors or rook + 1 minor
      end;

      if (_wr = 0) then begin                    // three minors
        if (b_phase = 2) then begin
          if (_br = 1) then begin                // opponent has a single rook
            if (_wn = 2) then result := 26;      // KNNBKR
            if (_wb = 2) then result := 90;      // KBBNKR
          end;
          if (_br = 0) then begin                // opponent has 2 minors
            result := 26;
            if (_wb = 2) and (_bn = 2) then
              result := 51;                      // KBBmKNN
          end;
        end;
        if (b_phase = 3) then result := 26;      // opponent has 3 minors or rook + 1 minor
      end;
    end;

    if (w_phase = 4) then begin
      if (_wq <> 0) then begin                   // queen
        if (b_phase = 2) then begin
          if (_bn = 2) then result := 26;        // KQKNN
          if (_bn = 1) then result := 102;       // KQKBN
          if (_bn = 0) then result := 90;        // KQKR or KQKBB
        end;
        if (b_phase = 3) then result := 13;      // KQKmmm or KQKRm
        if (b_phase = 4) then result := 13;      // KQKQ, KQKRR, KQKRmm, KQKmmmm
      end;

      if (_wr = 2) then                          // two rooks
        case b_phase of
          2: if (_br = 0) then result := 90;     // KRRKmm
          3: result := 26;                       // KRRKmmm, KRRKRm
          4: result := 26;                       // KRRKRR, KRRKQ, KRRKRmm ...
      end;

      if (_wr = 1) then begin                    // rook + 2 minors
        if (b_phase = 3) then begin
          if (_br = 1) then result := 38;        // KRmmKRm
          if (_br = 0) then result := 26;        // KRmmKmmm
        end;
        if (b_phase = 4) then result := 26;      // KRmmKQ, KRmmKRR, KRmmKmmmm
      end;

      if (_wq + _wr = 0) then begin              // 4 minors
        if (b_phase = 3) then begin
          if (_br = 1) then result := 51;        // KmmmmKRm
          if (_br = 0) then result := 26;        // KmmmmKmmm
        end;
        if (b_phase = 4) then begin
          if (_bq <> 0) then
            result := 102                        // KmmmmKQ
          else
            result := 13;                        // KmmmmKRR, KmmmmKRmm, KmmmmKmmmm
        end;
      end;
    end;

    if (w_phase = 5) then begin
      if (_wq <> 0) then begin                     // Queen + minor
        if (b_phase = 5) then result := 13;
        if (b_phase = 4) then begin
          result := 26;
          if (_br = 2) then begin
            if (_wn <> 0) then result := 38;       // KQNKRR
            if (_wb <> 0) then result := 90;       // KQBKRR
          end;
        end;
      end;

      if (_wr = 1) then begin                      // Rook + 3 minors
        if (b_phase = 4) then begin
          if (_bq <> 0) then result := 115;        // KRmmmKQ
          if (_br = 2) then result := 90;          // KRmmmKRR
          if (_br = 1) then result := 38;          // KRmmmKRmm
          if (_bq + _br = 0) then result := 13;    // KRmmmKmmmm
        end;
        if (b_phase = 5) then result := 26;
      end;

      if (_wr = 2) then begin                      // 2 Rooks + minor
        if (b_phase = 4) then begin
          if (_bq = 1) then begin
            if (_wb = 1) then result := 102;       // KRRBKQ
            if (_wn = 1) then result := 90;        // KRRNKQ
          end;
          if (_br = 2) then result := 38;          // KRRKRR
          if (_br = 1) then result := 26;          // KRRKRmm
          if (_bq + _br = 0) then result := 13;    // KRRKmmmm
        end;
        if (b_phase = 5) then result := 13;
      end;
    end;

    if (w_phase = 6) then begin
      if (_wq <> 0) then begin
        if (_wr <> 0) then                         // Queen + Rook
          case b_phase of
            4: case _br of
                 0: if (_bq = 0) then result := 26;
                 1: result := 77;
                 2: result := 38;
               end;
            5: if (_bq <> 0) or (_br <> 0) then result := 13;
            6: result := 13;
          end
        else                                       // Queen + 2 minors
          case b_phase of
            4: if (_bq = 0) and (_br = 0) then result := 64;
            5: begin
                 if (_bq <> 0) then result := 26;
                 if (_br = 2) then result := 26;
                 if (_br = 1) then result := 13;
               end;
            6: result := 13;
          end;
      end;

      if (_wr = 2) then begin                      // 2 rooks + 2 minors
        if (b_phase = 5) then begin
          if (_bq <> 0) then result := 90;         // KRRmmKQm
          if (_br = 2) then result := 26;          // KRRmmKRRm
          if (_br = 1) then result := 13;          // KRRmmKRmmm
        end;
        if (b_phase = 6) then result := 13;
      end;

      if (_wq = 0) and (_wr = 1) then begin        // rook + 4 minors
        if (b_phase = 5) then begin
          if (_bq <> 0) then result := 90;         // KRmmmmKQm
          if (_br = 2) then result := 26;          // KRmmmmKRRm
          if (_br = 1) then result := 13;          // KRmmmmKRmmm
        end;
        if (b_phase = 6) then result := 13;
      end;
    end;

    if (w_phase >= 7) then begin
      w_eval := 3 * (_wb + _wn) + 5 * _wr + 9 * _wq;
      b_eval := 3 * (_bb + _bn) + 5 * _br + 9 * _bq;
      if (w_eval > b_eval + 4) then result := 115;
      if (w_eval = b_eval + 4) then result := 90;
      if (w_eval = b_eval + 3) then result := 51;
      if (w_eval = b_eval + 2) then result := 26;
      if (w_eval < b_eval + 2) then result := 13;
    end;

  end;

  if (_wp = 1) then begin
    if (b_phase = 1) then begin
      if (w_phase = 1) then result := 38;
      if (w_phase = 2) then begin
        if (_wn = 2) then
          if (_bp = 0) then result := 38 else result := 64;
        if (_wr = 1) then result := 90;
      end;
    end;
    if (w_phase = b_phase) then begin
      if (b_phase = 2) then begin
        if (_br = 1) and (_wr = 1) then result := 102;
        if (_br = 0) then result := 51;
      end;
      if (b_phase >= 3) then begin
        if (_bb + _bn > 0) then result := 38;
        if (_bb + _bn = 0) then result := 64;
      end;
      if (b_phase = 4) and (_bq = 1) then result := 90;
    end;
  end;
end;

function get_win_chance(var board: TBoard; mi: PMtrlInfo): TScale;
var
  count: ^TPieceCount;
  pawn_cnt: integer;
begin
  result := SCALE_NORMAL;
  count := @board.count;

  with count^ do begin
    pawn_cnt := max(wp, bp);
    case w_np of

      0: result := $c0 - 8*pawn_cnt;

      1: if wq + bq = 2 then begin
           mi^.flags := mi^.flags or FLAG_QVQ;
           result := $70 + pawn_cnt;
         end
         else if wr + br = 2 then begin
           mi^.flags := mi^.flags or FLAG_RVR;
           result := $60 + 2*pawn_cnt;
         end
         else if wb + bb = 2 then begin
           if board.st^.bishop_flags in [BF_OPPOSITE_1, BF_OPPOSITE_2] then
             result := $30 + 4*pawn_cnt
           else
             result := $78 + 2*pawn_cnt
         end
         else if wn + bn = 2 then
           result := $80 + pawn_cnt;

      2: if (wb = 1) and (wr = 1) and (bb = 1) and (br = 1) then
           if board.st^.bishop_flags in [BF_OPPOSITE_1, BF_OPPOSITE_2] then
             result := $70 + pawn_cnt;
    end;
  end;
end;


end.

