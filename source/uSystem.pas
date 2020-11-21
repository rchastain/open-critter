
unit uSystem;

interface

uses
{$ifdef unix}
  CThreads;
{$else}
  Windows;
{$endif}

{$include platform.inc}

const
  MAX_THREADS = 8;

type
  time_t = {$ifdef unix}ptrint;{$else}int64;{$endif}

  timeb = packed record
    time: time_t;
    millitime: word;
    timezone: word;
    dstflag: word;
  end;


  TEvent = object
    procedure init();
    procedure wait_for(ms: cardinal);
    procedure signal();
    procedure done();
  private
    event: PRtlEvent;
  end;

{$ifdef cpu64}
  TThreadResult = int64;
{$else}
  TThreadResult = integer;
{$endif}

var
{$ifndef unix}
  hConsole: THandle;
{$endif}
  stdout: pointer;{$ifdef unix} external 'libc';{$endif}
  stdin: pointer;{$ifdef unix} external 'libc';{$endif}

const
{$ifdef unix}
  _IONBF = 2;
{$else}
  _IONBF = 4;
{$endif}

function printf(format: PAnsiChar): integer; cdecl; varargs; external libc;
function sprintf(s, format: PAnsiChar): integer; cdecl; varargs; external libc;
function strtok(strToken, strDelimit: PAnsiChar): PAnsiChar; cdecl; external libc;
function strncmp(s1, s2: PAnsiChar; count: integer): integer; cdecl; external libc;
function strcpy(strDest, strSource: PAnsiChar): PAnsiChar; cdecl; external libc;
function strcat(strDest, strSource: PAnsiChar): PAnsiChar; cdecl; external libc;
function strchr(str: PAnsiChar; c: integer): PAnsiChar; cdecl; external libc;
function strstr(str, strSearch: PAnsiChar): PAnsiChar; cdecl; external libc;
function strlen(str: PAnsiChar): integer; cdecl; external libc;
function atoi(s: PAnsiChar): integer; cdecl; external libc;
function setvbuf(stream: pointer; buffer: pointer; mode, size: integer): integer; cdecl; external libc;
function strncpy(strDest, strSource: PAnsiChar; count: integer): PAnsiChar; cdecl; external libc;
function strncat(strDest, strSource: PAnsiChar; count: integer): PAnsiChar; cdecl; external libc;
function _strtoui64(const nptr, endptr: PAnsiChar; base: integer): uint64; cdecl; external libc{$ifdef unix} name 'strtoull'{$endif};
function _ftime64(out timeb: timeb): integer; cdecl; external libc{$ifdef unix} name 'ftime'{$endif};
function _stricmp(s1, s2: PAnsiChar): integer; cdecl; external libc{$ifdef unix} name 'strcasecmp'{$endif};
function fgets(buf: PAnsiChar; max_count: integer; f: pointer): pointer; cdecl; external libc;
function aligned_malloc(size: cardinal; var rawptr: pointer): pointer;
function get_cpu_count(): integer;
function get_time(): time_t;
function pause(): char;
procedure highlight_on();
procedure highlight_off();
procedure start_thread(proc: TThreadFunc; param: pointer; out tid: TThreadID);
{$ifdef unix}
function sysconf(name: integer): integer; cdecl; external libc;
{$endif}
procedure zero_mem(dest: pointer; count: integer); inline;

implementation

procedure TEvent.init();
begin
  event := RtlEventCreate();
end;

procedure TEvent.wait_for(ms: cardinal);
begin
  if ms = $ffffffff then
    RtlEventWaitFor(event)
  else
    RtlEventWaitFor(event, ms);
end;

procedure TEvent.signal();
begin
  RtlEventSetEvent(event);
end;

procedure TEvent.done();
begin
  RtlEventDestroy(event);
end;

procedure start_thread(proc: TThreadFunc; param: pointer; out tid: TThreadID);
begin
  //tid := 0;
  BeginThread(proc, param, tid);
end;

function aligned_malloc(size: cardinal; var rawptr: pointer): pointer;
begin
  getmem(rawptr, size + $ff);
  result := align(rawptr, $100);
end;

function get_cpu_count(): integer;
{$ifdef unix}
const
  _SC_NPROCESSORS_ONLN = 84;
begin
  result := sysconf(_SC_NPROCESSORS_ONLN);
{$else}
var
  si: SYSTEM_INFO;
begin
  si.dwOemId := 0;
  GetSystemInfo(si);
  result := si.dwNumberOfProcessors;
{$endif}
  if result > MAX_THREADS then result := MAX_THREADS;
  if result < 1 then result := 1;
end;

function get_time(): time_t;
var
  t: timeb;
begin
  _ftime64(t);
  //result := t.time * 1000 + t.millitime;
  result := (t.time mod 86400) * 1000 + t.millitime; // RC
end;

{$ifndef unix}procedure get_iob();
var
  hCrt: THandle;
begin
  hCrt := GetModuleHandleA(libc);
  stdin := PByte(GetProcAddress(hCrt, PAnsiChar('_iob')));
{$ifdef cpu64}
  stdout := PByte(stdin) + 48;
{$else}
  stdout := PByte(stdin) + 32;
{$endif}
  FreeLibrary(hCrt);
end;
{$endif}

procedure zero_mem(dest: pointer; count: integer);
begin
  {$hints off}
  fillchar(dest^, count, 0);
  {$hints on}
end;

procedure highlight_on();
begin
  {$ifndef unix}
  SetConsoleTextAttribute(hConsole, $0f);
  {$endif}
end;

procedure highlight_off();
begin
  {$ifndef unix}
  SetConsoleTextAttribute(hConsole, $07);
  {$endif}
end;
function pause(): char;begin  readln(result);
end;

initialization
{$ifndef unix}
hConsole := GetStdHandle(cardinal(STD_OUTPUT_HANDLE));
get_iob();
{$endif}

end.
