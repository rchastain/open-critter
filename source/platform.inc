
{$ifndef cpu64}
{$asmmode intel}
{$endif}
{$define hasprefetch}
{$define hasbitscan}

const
{$ifdef cpu64}
  BITS = 64;
  ARCH_STR = '64-bit';
{$else}
  BITS = 32;
  ARCH_STR = '32-bit';
{$endif}
{$ifdef unix}
  libc = 'c';
  fmt64 = 'll';
{$else}
  libc = 'msvcrt.dll';
  fmt64 = 'I64';
{$endif}
