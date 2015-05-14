LF95 -c -zero -g -stchk -chk -nfix Lower Green River.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
lf95 -mod .\mods Lower Green River.OBJ BUDGET.obj AccessSubs.OBJ ARSUB.OBJ BUDSUB.OBJ -out Lower Green River.exe -win -ZERO -PAUSE -lib "C:\CanaimaSoft\Library\f90sql.lib"  -lib winspool gdi32.lib -G -SAV -WINCONSOLE
