LF95 -c -zero -g -stchk -chk -nfix Ivie Creek.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
lf95 -mod .\mods Ivie Creek.OBJ BUDGET.obj AccessSubs.OBJ ARSUB.OBJ BUDSUB.OBJ -out ..\Executable\Ivie Creek.exe -win -ZERO -lib "C:\CanaimaSoft\Library\f90sql.lib"  -lib winspool gdi32.lib -G -SAV -WINCONSOLE
