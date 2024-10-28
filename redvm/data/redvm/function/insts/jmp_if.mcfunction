execute store result storage redvm args.0 int 1 run scoreboard players remove sp redvm.regs 1
$data modify storage redvm args.1 set value $(_)
function redvm:utils/jmp_if with storage redvm args
