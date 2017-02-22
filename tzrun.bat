@echo off
cd %1

rem Finding SWIPL binary:
SET SwiplRoot=%ProgramFiles%\swipl
IF NOT "%SwiplRoot=%"=="" GOTO win64
SET SwiplRoot=%ProgramFiles(x86)%\swipl
:win64

if not exist "%cd%\bin" mkdir bin
rem Recompiling tonnetz if QLF not found:
IF NOT EXIST "%cd%\bin\tonnetz.qlf"	 (
	pushd src
	"%SwiplRoot%\bin\swipl-win" -s tzcompileme.pl -g "tzcompileme,halt(0)"
	move /Y tonnetz.qlf "..\bin\"
	popd
)

rem Launching Tonnetz:
"%SwiplRoot%\bin\swipl-win" -f ".\bin\tonnetz.qlf" -g tz_mainScreen
