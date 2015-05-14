del CoveFortBlackRock.exe
LF95 -c -zero -g -stchk -chk -nfix budget.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
LF95 -c -zero -g -stchk -chk -nfix budsub.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
LF95 -c -zero -g -stchk -chk -nfix arsub.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
LF95 -c -zero -g -stchk -chk -nfix AccessSubs.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
LF95 -c -zero -g -stchk -chk -nfix CoveFortBlackRock.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
lf95 -mod .\mods CoveFortBlackRock.OBJ BUDGET.obj AccessSubs.OBJ ARSUB.OBJ BUDSUB.OBJ -out CoveFortBlackRock.exe -win -ZERO -PAUSE -lib "C:\CanaimaSoft\Library\f90sql.lib"  -lib winspool gdi32.lib -G -SAV -WINCONSOLE
