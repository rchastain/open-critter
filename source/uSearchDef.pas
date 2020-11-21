
unit uSearchDef;

interface

uses
  uMove, uPiece;

{$include platform.inc}

type
  TNodeType = integer;

const
  NODE_CUT    = 0;
  NODE_ALL    = 1;
  NODE_PV     = 2;

type
  PSearchStack = ^TSearchStack;
  TSearchStack = record
    curr_move: TMove;
    killer_a, killer_b: TMove;
    threat: array [TSide] of integer;
    reduction: integer;
    in_chk: boolean;
  end;
  PSS = ^TSearchStack;

  PSearchStats = ^TSearchStats;
  TSearchStats = record
    nodes: int64;
    tbhits: int64;
    maxply: integer;
  end;

  TTimeManagement = ( TM_NORMAL, TM_FIXED );

  TTimeCtrl = record
    mode: TTimeManagement;
    time, increment: array [TSide] of integer;
    moves_to_go: integer;
    move_time: integer;
    max_depth: integer;
    max_nodes: integer;
  end;

var
  use_gtb: integer;

const
  TB_DISABLED   = 0;
  TB_AT_ROOT    = 1;
  TB_IN_SEARCH  = 2;
  TB_EVERYWHERE = 3;

  LAZY_PV    = $300;
  LAZY_PV_QS = $180;
  LAZY_NONPV = $300;
  LAZY_QS    = $180;

  EASY_MARGIN = $180;
  NULL_MARGIN = 0;

  AW_WIDTH    = 22;

  TIME_NODES  = 30000;
  LATENCY     = 10;

  EGTB_DRAFT  = 128;

  MAX_ITER    = 64;
  MAX_PLY     = 256;
  PLYUNIT     = 2;
  FULL_PLY    = PLYUNIT;
  HALF_PLY    = PLYUNIT div 2;
  ONE_PLY     = PLYUNIT;
  TWO_PLIES   = PLYUNIT * 2;

  FUTILITY_DEPTH  = 8 * PLYUNIT;
  SELECTIVE_DEPTH = 5 * PLYUNIT;
  EGTB_DEPTH      = 5 * PLYUNIT;
  LMR_DEPTH_PV    = ONE_PLY;
  LMR_COUNT_PV    = 8;

implementation

end.
