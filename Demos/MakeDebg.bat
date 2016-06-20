@echo off

echo (Re)compile demos, for debugging.
md ACU_Debg

gnatmake %1 %2 %3 demos -Pe3d.gpr -XBuild_Mode=Debug

rem BUG in DJGPP 4.3.2: the current directory is not set back
cd..

rem if exist b_*.* del b_*.*
