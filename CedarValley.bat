O:
CD \DATABASE\WATBUDG\MODELS\CODE
del CedarValley.exe
LF95 -c -zero -g -stchk -chk -nfix O:\DATABASE\WATBUDG\Models\Code\CedarValley.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
lf95 -map O:\DATABASE\WATBUDG\Models\Code\CedarValley.map -mod .\mods;C:\CanaimaSoft\Include O:\DATABASE\WATBUDG\Models\Code\CedarValley.OBJ BUDGET.obj AccessSubs.OBJ ARSUB.OBJ BUDSUB.OBJ -win -ZERO -PAUSE -lib "C:\CanaimaSoft\Library\f90sql.lib"  -lib winspool  gdi32.lib -G -SAV -WINCONSOLE -out O:\DATABASE\WATBUDG\Models\Code\CedarValley.exe
