@echo off

echo (Re)compile demos, for debugging.
set obj_dir=ACU_Debg
call make_ali

set gnat_opt=-i -aO%obj_dir% -gnatk8 -gnat95 -I. -I../lib_src -I../lib_src/dos -I../lib_src/3DModels -I../lib_src/Misc -I../lib_src/UzA_Src

set gnat_opt=%gnat_opt% -gnato -fstack-check -g -gnatwkmpz -gnatVa -gnatec%obj_dir%\debug.pra -gnatyhiknp

gnatmake %1 %2 %3 demos %gnat_opt%

rem BUG in DJGPP 4.3.2: the current directory is not set back for project files (.gpr), unused now
rem cd..

rem if exist b_*.* del b_*.*
