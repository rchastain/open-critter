
u=source/units
s=source/OpenCritter.pas
x=./critter

mkdir -p $u

if [ "${1,,}" == "release" ]
then
  fpc -FcUTF8 -B -Mobjfpc -Sh -FU$u $s -o$x -dRELEASE -CX -XX -Xs -vm5024,5025 | tee build.log
else
  fpc -FcUTF8 -B -Mobjfpc -Sh -FU$u $s -o$x -Sa -dDEBUG -ghl -vm5024,5025 | tee build.log
fi
