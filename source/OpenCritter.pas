
program OpenCritter;
{$ifndef unix}
{$apptype console}
{$endif}

uses
{$ifdef unix}
  CThreads,
{$else}
  Windows,
{$endif}
  uBitbase, uBitboard, uBook, uEval, uGtb, uInput, uMaterial, uMovegen,
  uPsq, uSearchBase, uSystem, uUtil, uZobrist;

var
  argc, i: integer;
{$ifdef windows}
  file_name: PAnsiChar = nil;
{$endif}

begin
  setvbuf(stdout, nil, _IONBF, 0);

  printf('OpenCritter %s %s by Richard Vida, Slovakia' + LineEnding, CRITTER_VER, PAnsiChar(ARCH_STR));
  
  {$ifdef unix}
  program_dir := strcpy(program_dir, './');
  {$else}
  GetFullPathNameA(PAnsiChar(AnsiString(ParamStr(0))), 256, program_dir, file_name);
  file_name[0] := #0;
  {$endif}

  init_bitboard();
  init_mtrl();
  init_gtb();
  init_margins();
  randomize();

  input_buffer[0] := #0;
  argc := ParamCount();
  for i := 1 to argc do
  begin
    if i <> 1 then strcat(input_buffer, ' ');
    strcat(input_buffer, PAnsiChar(AnsiString(ParamStr(i))));
  end;

  UCI();

  destroy_gtb();
  if Assigned(book) then book.Free();
  if Assigned(evaluator) then evaluator^.cache_destroy();
end.
