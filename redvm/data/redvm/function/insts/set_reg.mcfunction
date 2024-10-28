$execute store result storage redvm args.0 int 1 run scoreboard players get $(_) redvm.regs
execute store result storage redvm args.1 int 1 run scoreboard players remove sp redvm.regs 1
function redvm:utils/set_reg with storage redvm args
