@echo off

if "%1"=="" echo.
if "%1"=="" echo Make_xx builds an unit with optimisations for speed
if "%1"=="" echo and access to Ada sources directories (libraries, 3D models)
if "%1"=="" echo.
if "%1"=="" echo usage: Make_xx name
if "%1"=="" goto fin

set obj_dir=ACU_Opti
call make_ali

set gnat_opt=-i -aO%obj_dir% -gnatk8 -gnat95 -I. -I../lib_src -I../lib_src/dos -I../lib_src/3DModels -I../lib_src/Misc -I../lib_src/UzA_Src

set gnat_opt=%gnat_opt% -O2 -gnatn -funroll-loops -fpeel-loops -ftracer -funswitch-loops -gnatp -fomit-frame-pointer


@echo on
              gnatmake %1 %2 %3 %4 %gnat_opt%
@echo off

rem BUG in DJGPP 4.3.2: the current directory is not set back for project files (.gpr), unused now
rem cd..


if exist b_*.* del b_*.*

:fin
