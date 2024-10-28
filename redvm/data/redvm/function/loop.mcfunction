execute store result storage redvm args.0 int 1 run scoreboard players get ip redvm.regs
function redvm:utils/get_inst with storage redvm args
execute unless data storage redvm args.0 run return 1
scoreboard players add ip redvm.regs 1
function redvm:utils/run_cmd with storage redvm args

schedule function redvm:loop 0.5t
