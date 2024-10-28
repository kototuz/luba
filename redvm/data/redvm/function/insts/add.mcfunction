execute store result storage redvm args.1 int 1 run scoreboard players remove sp redvm.regs 1
execute store result storage redvm args.0 int 1 run scoreboard players remove sp redvm.regs 1
function redvm:utils/add with storage redvm args
scoreboard players add sp redvm.regs 1
