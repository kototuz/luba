$scoreboard players add sp2 redvm.regs $(_)
execute store result storage redvm args.0 int 1 run scoreboard players get sp2 redvm.regs
$scoreboard players remove sp2 redvm.regs $(_)
return run function redvm:utils/see_local with storage redvm args
