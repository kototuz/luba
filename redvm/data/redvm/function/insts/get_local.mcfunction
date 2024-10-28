$scoreboard players add sp2 redvm.regs $(_)
execute store result storage redvm args.1 int 1 run scoreboard players get sp2 redvm.regs
execute store result storage redvm args.0 int 1 run scoreboard players get sp redvm.regs
function redvm:utils/set_local with storage redvm args
scoreboard players add sp redvm.regs 1
$scoreboard players remove sp2 redvm.regs $(_)
