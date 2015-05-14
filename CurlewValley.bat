O:
CD \DATABASE\WATBUDG\MODELS\CODE
del CurlewValley.exe
LF95 -c -zero -g -stchk -chk -nfix budget.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
LF95 -c -zero -g -stchk -chk -nfix budsub.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
LF95 -c -zero -g -stchk -chk -nfix arsub.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
LF95 -c -zero -g -stchk -chk -nfix AccessSubs.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
LF95 -c -zero -g -stchk -chk -nfix O:\DATABASE\WATBUDG\Models\Code\CurlewValley.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
lf95 -map O:\DATABASE\WATBUDG\Models\Code\CurlewValley.map -mod .\mods;C:\CanaimaSoft\Include O:\DATABASE\WATBUDG\Models\Code\CurlewValley.OBJ BUDGET.obj AccessSubs.OBJ ARSUB.OBJ BUDSUB.OBJ -win -ZERO -PAUSE -lib "C:\CanaimaSoft\Library\f90sql.lib"  -lib winspool  gdi32.lib -G -SAV -WINCONSOLE -out O:\DATABASE\WATBUDG\Models\Code\CurlewValley.exe
