PC=fpc
ifeq ($(OS),Windows_NT)
PC32=
PC64=
else
PC32=/home/roland/Applications/freepascal32/fpc
PC64=/home/roland/Applications/freepascal64/fpc
endif
DEBUG=true
# Pour compiler en mode RELEASE, on utilisera la commande "make DEBUG=false".
ifeq ($(DEBUG),true)
	PFLAGS=-FcUTF8 -B -Mobjfpc -Sh -FUunits -Sa -dDEBUG -ghl -vm5024,5025,6058
else
	PFLAGS=-FcUTF8 -B -Mobjfpc -Sh -FUunits -dRELEASE -CX -XX -Xs -vm5024,5025,6058
endif

critter: source/OpenCritter.pas
ifeq ($(OS),Windows_NT)
	if not exist units mkdir units
else
	mkdir -p units
endif
	$(PC) $^ -o$@ $(PFLAGS)
 
critter32: source/OpenCritter.pas
ifeq ($(OS),Windows_NT)
	if not exist units mkdir units
else
	mkdir -p units
endif
	$(PC32) $^ -o$@ $(PFLAGS)
 
critter64: source/OpenCritter.pas
ifeq ($(OS),Windows_NT)
	if not exist units mkdir units
else
	mkdir -p units
endif
	$(PC64) $^ -o$@ $(PFLAGS)
 
clean:
ifeq ($(OS),Windows_NT)
	del /q units\*.o
	del /q units\*.ppu
else
	rm -f units/*.o
	rm -f units/*.ppu
endif
