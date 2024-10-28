$execute store result storage redvm args.0 int 1 run scoreboard players add sp2 redvm.regs $(_)
execute store result storage redvm args.1 int 1 run scoreboard players remove sp redvm.regs 1
function redvm:utils/set_local with storage redvm args
$scoreboard players remove sp2 redvm.regs $(_)
