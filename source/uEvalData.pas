
unit uEvalData;

interface

uses
  uPiece, uScore, uSquare;

{$include platform.inc}

const
  NO_OPEN_FILES      = $000a0000;

  ATK_WEIGHT_PAWN    = 0;
  ATK_WEIGHT_KNIGHT  = 38;
  ATK_WEIGHT_BISHOP  = 38;
  ATK_WEIGHT_ROOK    = 64;
  ATK_WEIGHT_QUEEN   = 102;

  ROOK_TRAPPED       = 130;

  ROOK_HALF_OPEN     = $0008000f;   // Pair( 8, 15);
  ROOK_OPEN_BLOCK    = $00190000;   // Pair(25,  0);
  ROOK_OPEN_MINOR    = $0026000d;   // Pair(38, 13);
  ROOK_OPEN          = $0033001a;   // Pair(51, 26);
  ROOK_ALMOST_OPEN   = $000d000d;   // Pair(13, 13);
  ROOK_KING_FILE     = $00190000;   // Pair(25,  0);
  ROOK_ON_6TH        = $000d0026;   // Pair(13, 38);
  ROOK_ON_7TH        = $001e004b;   // Pair(30, 75);
  ROOK_ON_7TH_DBL    = $000d001a;   // Pair(13, 26);
  ROOK_ON_8TH        = $000d001a;   // Pair(13, 26);
  ROOK_DEFENSE       = $00080003;   // Pair( 8,  3);
  ROOK_VS_PAWN       = $00050008;   // Pair( 5,  8);
  ROOK_VS_MINOR      = $000a000d;   // Pair(10, 13);
  ROOK_VS_QUEEN      = $000d000d;   // Pair(13, 13);

  QUEEN_ON_7TH       = $000d0040;   // Pair(13, 64);
  QUEEN_ON_7TH_DBL   = $00190026;   // Pair(25, 38);
  QUEEN_VS_PIECE     = $000a000a;   // Pair(10, 10);
  QUEEN_DEFENSE      = $000c0005;   // Pair(12,  5);
  QUEEN_CONTACT_CHK  = $000d0012;   // Pair(13, 18);
  QUEEN_MATE_THREAT  = $0014001f;   // Pair(20, 31);

  BISHOP_DEFENSE     = $00050003;   // Pair(  5,  3);
  BISHOP_VS_PAWN     = $0007000a;   // Pair(  7, 10);
  BISHOP_VS_MINOR    = $000d000d;   // Pair( 13, 13);
  BISHOP_VS_MAJOR    = $0012001a;   // Pair( 18, 26);
  BISHOP_TRAPPED     = $00640064;   // Pair(100,100);

  KNIGHT_DEFENSE     = $000a0005;   // Pair(10,  5);
  KNIGHT_VS_PAWN     = $0007000a;   // Pair( 7, 10);
  KNIGHT_VS_MINOR    = $000d000d;   // Pair(13, 13);
  KNIGHT_VS_MAJOR    = $0012001a;   // Pair(18, 26);
  KNIGHT_TRAPPED1    = $000c0019;   // Pair(12, 25);
  KNIGHT_TRAPPED2    = $0033004d;   // Pair( 51, 77);

  PAWN_VS_MINOR      = $000d0012;   // Pair(13, 18);
  PAWN_VS_ROOK       = $0012001a;   // Pair(18, 26);
  PAWN_VS_QUEEN      = $0014001e;   // Pair(20, 30);

  KING_VS_PAWN       = $0000000d;   // Pair( 0, 13);

  TRAPPED_PIECE      = $000d000d;   // Pair(13, 13);

  MULTI_ATTACK       = $00260040;   // Pair(38, 64);
  PAWN_BLOCK         = $00070015;   // Pair( 7, 21);
  PAWN_HIDDEN        = $00000064;   // Pair( 0,100);

  MOB_KNIGHT         = $000f0012;   // Pair(15, 18);
  MOB_BISHOP         = $000c000d;   // Pair(12, 13);
  MOB_ROOK           = $00050008;   // Pair( 5,  8);
  MOB_QUEEN          = $00050005;   // Pair( 5,  5);

  OUTPOST_N          = $00050008;   // Pair(  5,  8);
  OUTPOST_N_PROT     = $00050008;   // Pair(  5,  8);
  OUTPOST_B          = $00050008;   // Pair(  5,  8);
  OUTPOST_B_ATK      = $0008000a;   // Pair(  8, 10);
  OUTPOST_R          = $00030005;   // Pair(  3,  5);
  OUTPOST_R_ATK      = $0008000a;   // Pair(  8, 10);

  PAWN_CHAIN: array [TFile] of TScorePair = (
    $000a0002, $000c0003, $000c0003, $000f0004,
    $000f0004, $000c0003, $000c0003, $000a0002
  );

  PAWN_ISOLATED: array [boolean, TFile] of TScorePair = (
    ( $000c0014, $000c0014, $000c0014, $000c0014,
      $000c0014, $000c0014, $000c0014, $000c0014 ),
    ( $00260033, $00260033, $00260033, $00260033,
      $00260033, $00260033, $00260033, $00260033 )
  );

  PAWN_DOUBLED: array [boolean, TFile] of TScorePair = (
    ( $0005000a, $0005000a, $0005000a, $0005000a,
      $0005000a, $0005000a, $0005000a, $0005000a ),
    ( $000a0014, $000a0014, $000a0014, $000a0014,
      $000a0014, $000a0014, $000a0014, $000a0014 )
  );

  PAWN_DBL_ISO: array [boolean, TFile] of TScorePair = (
    ( $0005000a, $0005000a, $0005000a, $0005000a,
      $0005000a, $0005000a, $0005000a, $0005000a ),
    ( $000f0019, $000f0019, $000f0019, $000f0019,
      $000f0019, $000f0019, $000f0019, $000f0019 )
  );

  PAWN_WEAK: array [boolean, TFile] of TScorePair = (
    ( $000d000d, $000d000d, $000d000d, $000d000d,
      $000d000d, $000d000d, $000d000d, $000d000d ),
    ( $001a0026, $001a0026, $001a0026, $001a0026,
      $001a0026, $001a0026, $001a0026, $001a0026 )
  );

  PAWN_CANDIDATE: array [TRank] of TScorePair = (
    $00000000, $000a0014, $000a0014, $00140028,
    $001e003c, $00320064, $00000000, $00000000
  );

  PP_BASE: array [TRank] of TScorePair = (
    $00000000, $00000000, $00000000, $001a001a,
    $00330040, $00660080, $009a00c0, $00000000
  );

  PP_OUTSIDE: array [TRank] of TScorePair = (
    $00000000, $00000000, $00000000, $00000000,
    $0005000d, $000d001a, $001a0033, $00000000
  );

  PP_PROTECTED: array [TRank] of TScorePair = (
    $00000000, $00000000, $00000000, $00000000,
    $000d001a, $001a0026, $00260040, $00000000
  );

  PP_CONNECTED: array [TRank] of TScorePair = (
    $00000000, $00000000, $00000000, $00000000,
    $000d001a, $001a0026, $0034004d, $00000000
  );


  PP_CLEAR_OWN: array [TRank] of TScorePair = (
    $00000000, $00000000, $00000000, $00000000,
    $00000000, $0008000d, $000d001a, $00000000
  );

  PP_CLEAR_OPP: array [TRank] of TScorePair = (
    $00000000, $00000000, $00000000, $00000000,
    $000d001a, $0026004d, $00400080, $00000000
  );

  PP_CAN_MOVE: array [TRank] of TScorePair = (
    $00000000, $00000000, $00000000, $00030005,
    $00050008, $0008000d, $000d001a, $00000000
  );

  PP_FREE: array [TRank] of TScorePair = (
    $00000000, $00000000, $00000000, $00000000,
    $000d001a, $001a0033, $00330066, $00000000
  );

  PP_QVQ_BONUS: array [TRank] of TScorePair = (
    $00000000, $00000000, $00000000, $000c000c,
    $00180018, $00300030, $00600060, $00000000
  );

  PP_KING_DIST: array [TRank] of integer = (
    0, 0, 0, 8, 15, 23, 38, 0
  );

  PP_UNSTOPPABLE = $500;

  CENTER_BLOCK: array [TSquare] of byte = (
    0, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 2, 2, 2, 2, 1, 1,
    1, 2, 3, 3, 3, 3, 2, 1,
    1, 2, 3, 5, 5, 3, 2, 1,
    1, 2, 3, 5, 5, 3, 2, 1,
    1, 2, 3, 3, 3, 3, 2, 1,
    1, 1, 2, 2, 2, 2, 1, 1,
    0, 0, 0, 0, 0, 0, 0, 0
  );

  KNIGHT_OUTPOST_SQ: array [TSquare] of byte = (
    0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,
    0, 13, 13, 21, 21, 13, 13,  0,
    0, 18, 18, 26, 26, 18, 18,  0,
    0, 13, 13, 21, 21, 13, 13,  0,
    0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0
  );

  PSHIELD: array [0..2, TRank] of byte = (
    ( 77,  0, 13, 38, 51, 64, 64, 64 ),
    (141,  0, 38,102,128,141,141,141 ),
    ( 77,  0, 26, 51, 64, 77, 77, 77 )
  );

  PSTORM: array [0..2, TRank] of byte = (
    ( 13,  0, 90, 38, 13,  0,  0,  0 ),
    ( 26,  0,128, 51, 26,  0,  0,  0 ),
    ( 26,  0,128, 51, 26,  0,  0,  0 )
  );

  NOT_DEVELOPED: array [0..15] of TScore = (
    0, 3, 10, 15, 25, 38, 51, 77, 69, 79, 89, 100, 115, 115, 115, 115
  );

  OPEN_FILE_MULT: array [0..8] of integer = ( 6, 5, 4, 3, 2, 1, 0, 0, 0 );
  PAWN_COUNT_MULT: array [0..8] of integer = ( 6, 5, 4, 3, 2, 1, 0, 0, 0 );


  BISHOP_PIN: array [TPiece, TSide] of TScorePair = (
    ( 0, 0 ), ( 0, 0 ),

    ( $0007000d, $0005000d ),   // W_PAWN
    ( $0005000d, $0007000d ),   // B_PAWN

    ( $0007000d, $00000000 ),   // W_KING
    ( $00000000, $0007000d ),   // B_KING

    ( $0007000d, $0005000d ),   // W_KNIGHT
    ( $0005000d, $0007000d ),   // B_KNIGHT

    ( 0, 0 ), ( 0, 0 ),         // W_BISHOP, B_BISHOP

    ( $0007000d, $00260040 ),   // W_ROOK
    ( $00260040, $0007000d ),   // B_ROOK

    ( $00000000, $00190033 ),   // W_QUEEN
    ( $00190033, $00000000 ),   // B_QUEEN

    ( 0, 0 ), ( 0, 0 )
  );

  ROOK_PIN: array [TPiece, TSide] of TScorePair = (
    ( 0, 0 ), ( 0, 0 ),

    ( $00000000, $0005000d ),   // W_PAWN
    ( $0005000d, $00000000 ),   // B_PAWN

    ( $0007000d, $00000000 ),   // W_KING
    ( $00000000, $0007000d ),   // B_KING

    ( $0007000d, $0005000d ),   // W_KNIGHT
    ( $0005000d, $0007000d ),   // B_KNIGHT

    ( $0007000d, $0005000d ),   // W_BISHOP
    ( $0005000d, $0007000d ),   // B_BISHOP

    ( 0, 0 ), ( 0, 0 ),         // W_ROOK, B_ROOK

    ( $00000000, $00190033 ),   // W_QUEEN
    ( $00190033, $00000000 ),   // B_QUEEN

    ( 0, 0 ), ( 0, 0 )
  );

  QUEEN_PIN_ORTH: array [TPiece, TSide] of TScorePair = (
    ( 0, 0 ), ( 0, 0 ),
    ( 0, 0 ), ( 0, 0 ),         // W_PAWN, B_PAWN

    ( $0005000d, $00000000 ),   // W_KING
    ( $00000000, $0005000d ),   // B_KING

    ( $0005000a, $0005000d ),   // W_KNIGHT
    ( $0005000d, $0005000a ),   // B_KNIGHT

    ( $0005000a, $0005000d ),   // W_BISHOP
    ( $0005000d, $0005000a ),   // B_BISHOP

    ( 0, 0 ), ( 0, 0 ),         // W_ROOK, B_ROOK
    ( 0, 0 ), ( 0, 0 ),         // W_QUEEN, B_QUEEN
    ( 0, 0 ), ( 0, 0 )
  );

  QUEEN_PIN_DIAG: array [TPiece, TSide] of TScorePair = (
    ( 0, 0 ), ( 0, 0 ),

    ( $00020005, $00000000 ),   // W_PAWN
    ( $00000000, $00020005 ),   // B_PAWN

    ( $0005000d, $00000000 ),   // W_KING
    ( $00000000, $0005000d ),   // B_KING

    ( $0005000a, $0005000d ),   // W_KNIGHT
    ( $0005000d, $0005000a ),   // B_KNIGHT

    ( 0, 0 ), ( 0, 0 ),         // W_BISHOP, B_BISHOP

    ( $0005000a, $0005000d ),   // W_ROOK
    ( $0005000d, $0005000a ),   // B_ROOK

    ( 0, 0 ), ( 0, 0 ),         // W_QUEEN, B_QUEEN
    ( 0, 0 ), ( 0, 0 )
  );

  CR_BONUS: array [0..15] of TScore = (
    0, 11, -11, 0, 11, 22, 0, 11, -11, 0, -22, -11, 0, 11, -11, 0
  );

implementation

end.
