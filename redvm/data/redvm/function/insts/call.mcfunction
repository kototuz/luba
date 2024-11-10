execute store result storage redvm args.0 int 1 run scoreboard players get sp redvm.regs
data modify storage redvm args.1 set value "ip"
function redvm:utils/get_reg with storage redvm args
scoreboard players add sp redvm.regs 1
$scoreboard players set ip redvm.regs $(_)
