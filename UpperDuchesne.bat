O:
CD \DATABASE\WATBUDG\MODELS\CODE
del UpperDuchesne.exe
LF95 -c -zero -g -stchk -chk -nfix budget.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
LF95 -c -zero -g -stchk -chk -nfix budsub.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
LF95 -c -zero -g -stchk -chk -nfix arsub.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
LF95 -c -zero -g -stchk -chk -nfix AccessSubs.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
LF95 -c -zero -g -stchk -chk -nfix O:\DATABASE\WATBUDG\Models\Code\UpperDuchesne.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
lf95 -map O:\DATABASE\WATBUDG\Models\Code\UpperDuchesne.map -mod .\mods;C:\CanaimaSoft\Include O:\DATABASE\WATBUDG\Models\Code\UpperDuchesne.OBJ BUDGET.obj AccessSubs.OBJ ARSUB.OBJ BUDSUB.OBJ -win -ZERO -PAUSE -lib "C:\CanaimaSoft\Library\f90sql.lib"  -lib winspool  gdi32.lib -G -SAV -WINCONSOLE -out O:\DATABASE\WATBUDG\Models\Code\UpperDuchesne.exe
