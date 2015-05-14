del BearLake.exe
LF95 -c -zero -g -stchk -chk -nfix budget.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
LF95 -c -zero -g -stchk -chk -nfix budsub.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
LF95 -c -zero -g -stchk -chk -nfix arsub.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
LF95 -c -zero -g -stchk -chk -nfix AccessSubs.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
LF95 -c -zero -g -stchk -chk -nfix BearLake.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
lf95 -mod .\mods BearLake.OBJ BUDGET.obj AccessSubs.OBJ ARSUB.OBJ BUDSUB.OBJ -out BearLake.exe -win -ZERO -PAUSE -lib "C:\CanaimaSoft\Library\f90sql.lib"  -lib winspool gdi32.lib -G -SAV -WINCONSOLE
