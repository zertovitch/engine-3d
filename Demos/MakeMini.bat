@echo off

echo (Re)compile some packages optimized in *space*, not in *time*.

set obj_dir=ACU_Opti

echo * Call the "fast" make to ensure that the "fast" .o files exist
call makefast

echo * Delete the object files that can be "smaller" instead of "faster"
echo   without ruining display performance.
echo on

del %obj_dir%\eng3dini.o
del %obj_dir%\eng3dcon.o
del %obj_dir%\eng3dmer.o
del %obj_dir%\eng3dmor.o
del %obj_dir%\eng3dopt.o

del %obj_dir%\x29.o
del %obj_dir%\vehic001.o
del %obj_dir%\shuttle3.o
del %obj_dir%\icosaedr.o

del %obj_dir%\time_log.o
del %obj_dir%\finetime.o
del %obj_dir%\gamedriv.o
del %obj_dir%\multkeys.o
del %obj_dir%\io_ports.o
del %obj_dir%\pc_mouse.o
del %obj_dir%\dosinter.o
del %obj_dir%\scanprof.o
del %obj_dir%\svga.o
del %obj_dir%\svga-io.o
del %obj_dir%\svgeffio.o

del %obj_dir%\megramod.o
del %obj_dir%\demo3d00.o
del %obj_dir%\demo3d01.o
del %obj_dir%\demos.o

@echo off

echo * Recompile all of Zip-Ada as "small"
gnatmake -f unzip -Pe3d.gpr -XBuild_Mode=Small
rem BUG in DJGPP 4.3.2: the current directory is not set back
cd..

echo * Build demos as "small"
del demos.
gnatmake demos -Pe3d.gpr -XBuild_Mode=Small
rem BUG in DJGPP 4.3.2: the current directory is not set back
cd..

upx --ultra-brute demos.exe

if exist b_*.* del b_*.*
