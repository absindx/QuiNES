@echo off
set OutputName=QuiNES
set Emulator=..\Mesen\Mesen.exe
set Assembler=..\nesasm
set MainSource=QuiNES

echo --------------------------------------------------
echo               %date% %time%
echo --------------------------------------------------

rem Force update
if "%1"=="" goto Assemble
	del "%OutputName%.nes"
	start %Emulator% "%OutputName%"

:Assemble
%Assembler% -s -f %MainSource%.asm > build.log
type build.log

move %MainSource%.nes "%OutputName%.nes" > NUL 2>&1
move %MainSource%.fns "%OutputName%.fns" > NUL 2>&1

if "%1"=="" goto Return
	start %Emulator% "%OutputName%.nes"

:Return
