execute store result storage redvm args.0 int 1 run scoreboard players get sp redvm.regs
$data modify storage redvm args.1 set value $(_)
function redvm:utils/const with storage redvm args
scoreboard players add sp redvm.regs 1
