O:
CD \DATABASE\WATBUDG\MODELS\CODE
del ProvoRivRes.exe
LF95 -c -zero -g -stchk -chk -nfix O:\DATABASE\WATBUDG\Models\Code\ProvoRivRes.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
lf95 -map O:\DATABASE\WATBUDG\Models\Code\ProvoRivRes.map -mod .\mods;C:\CanaimaSoft\Include O:\DATABASE\WATBUDG\Models\Code\ProvoRivRes.OBJ BUDGET.obj AccessSubs.OBJ ARSUB.OBJ BUDSUB.OBJ -win -ZERO -PAUSE -lib "C:\CanaimaSoft\Library\f90sql.lib"  -lib winspool  gdi32.lib -G -SAV -WINCONSOLE -out O:\DATABASE\WATBUDG\Models\Code\ProvoRivRes.exe
