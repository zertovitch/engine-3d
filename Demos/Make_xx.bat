@echo off

if "%1"=="" echo.
if "%1"=="" echo Make_xx builds an unit with optimisations
if "%1"=="" echo and access to Ada sources directories (libraries, 3D models)
if "%1"=="" echo.
if "%1"=="" echo usage: Make_xx name
if "%1"=="" goto fin

md ACU_Opti

@echo on
              gnatmake %1 %2 %3 %4  -Pe3d.gpr -XBuild_Mode=Fast
@echo off

rem BUG in DJGPP 4.3.2: the current directory is not set back
cd..


if exist b_*.* del b_*.*

:fin
