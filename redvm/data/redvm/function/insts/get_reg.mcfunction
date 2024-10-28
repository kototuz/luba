execute store result storage redvm args.0 int 1 run scoreboard players get sp redvm.regs
$execute store result storage redvm args.1 int 1 run scoreboard players get $(_) redvm.regs
function redvm:utils/get_reg with storage redvm args
scoreboard players add sp redvm.regs 1
