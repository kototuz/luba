data remove storage redvm insts
scoreboard players reset * redvm.local
scoreboard objectives add redvm.local dummy
scoreboard objectives add redvm.regs dummy
scoreboard players set sp redvm.regs 0
scoreboard players set sp2 redvm.regs 0
scoreboard players set ip redvm.regs 0

$$(load_program)

function redvm:loop
