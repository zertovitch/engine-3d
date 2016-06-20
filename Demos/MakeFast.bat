@echo * Make_it: build demos, optimised. 
@echo       If the build process stops for any reason, just re-run "make_it".
del demos.
@make_xx %1 %2 %3 demos
