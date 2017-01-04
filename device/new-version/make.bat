@set ASM_INCLUDE="..\..\..\NDK_3.1\INCLUDES_LIBS\INCLUDE_I"
del o\*.o
del *.device
vasm -quiet -align -Fhunk -I.. -I%ASM_INCLUDE% -ldots -nosym -opt-speed -o o\dev.o ..\dev\dev.asm
vasm -quiet -align -Fhunk -I.. -I%ASM_INCLUDE% -ldots -nosym -opt-speed -o o\rdwt.o ..\rdwt\rdwt.asm
vasm -quiet -align -Fhunk -I.. -I%ASM_INCLUDE% -ldots -nosym -opt-speed -o o\bootstrap.o ..\dev\bootstrap.asm
vasm -quiet -align -Fhunk -I.. -I%ASM_INCLUDE% -ldots -nosym -opt-speed -o o\init.o ..\rdwt\init.asm
vasm -quiet -align -Fhunk -I.. -I%ASM_INCLUDE% -ldots -nosym -opt-speed -o o\debug-wrapper.o ..\debug\debug-wrapper.asm

vlink -Bstatic -bamigahunk -L"..\..\..\NDK_3.1\INCLUDES_LIBS\LINKER_LIBS" -l"AMIGA" -l"DEBUG" -o"ide.device" -s -F idefiles.txt
vlink -Bstatic -bamigahunk -L"..\..\..\NDK_3.1\INCLUDES_LIBS\LINKER_LIBS" -l"AMIGA" -l"DEBUG" -o"bootide.device" -s -F bootidefiles.txt

copy ide.device "..\..\..\..\roms\Modules\40.63(A500-2000)"\
copy bootide.device "..\..\..\..\roms\Modules\40.63(A500-2000)"\