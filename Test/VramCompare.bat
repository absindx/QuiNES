@echo off
setlocal enabledelayedexpansion
pushd %~dp0

set EmulatorPath=..\..\Mesen
set Emulator=%EmulatorPath%\Mesen.exe

echo %date% %time% : Start
%Emulator% --testrunner ..\QuiNES.nes VramCompare.lua > VramCompare.log 2>&1
echo %date% %time% : End (ExitCode = %ERRORLEVEL%)

move %EmulatorPath%\VramDump.asm VramDump.asm > NUL 2>&1

fc ..\QuiNES.asm VramDump.asm > NUL 2>&1
if %ERRORLEVEL% == 0 (
	echo [PASSED] Source matched
) else (
	echo [FAILED] Source did not match
)

popd
