LF95 -c -zero -g -stchk -chk -nfix Evanston 01-03-01.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
lf95 -mod .\mods Evanston 01-03-01.OBJ BUDGET.obj AccessSubs.OBJ ARSUB.OBJ BUDSUB.OBJ -out Evanston 01-03-01.exe -win -ZERO -PAUSE -lib "C:\CanaimaSoft\Library\f90sql.lib"  -lib winspool gdi32.lib -G -SAV -WINCONSOLE
