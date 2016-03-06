@set ASM_INCLUDE="..\..\..\NDK_3.1\INCLUDES_LIBS\INCLUDE_I"

@for /f "tokens=1,2 delims=." %%a in (files.txt) do @call vasm -quiet -Fhunk -I%ASM_INCLUDE% -I.. -ldots -nosym -o ..\%%a.o ..\%%a.asm & @echo Done assembling %%a.asm

@vlink -Bstatic -bamigahunk -L"..\..\..\NDK_3.1\INCLUDES_LIBS\LINKER_LIBS" -l"AMIGA" -l"DEBUG" -o"%1.device" -s -F files.txt

@echo Done linking %1.device