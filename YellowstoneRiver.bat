O:
CD \DATABASE\WATBUDG\MODELS\CODE
del YellowstoneRiver.exe
LF95 -c -zero -g -stchk -chk -nfix budget.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
LF95 -c -zero -g -stchk -chk -nfix budsub.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
LF95 -c -zero -g -stchk -chk -nfix arsub.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
LF95 -c -zero -g -stchk -chk -nfix AccessSubs.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
LF95 -c -zero -g -stchk -chk -nfix O:\DATABASE\WATBUDG\Models\Code\YellowstoneRiver.f90 -mod .\MODS -lib "C:\CanaimaSoft\Library\f90sql.lib"
lf95 -map O:\DATABASE\WATBUDG\Models\Code\YellowstoneRiver.map -mod .\mods;C:\CanaimaSoft\Include O:\DATABASE\WATBUDG\Models\Code\YellowstoneRiver.OBJ BUDGET.obj AccessSubs.OBJ ARSUB.OBJ BUDSUB.OBJ -win -ZERO -PAUSE -lib "C:\CanaimaSoft\Library\f90sql.lib"  -lib winspool  gdi32.lib -G -SAV -WINCONSOLE -out O:\DATABASE\WATBUDG\Models\Code\YellowstoneRiver.exe
