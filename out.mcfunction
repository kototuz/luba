data modify storage redvm insts append value 'function redvm:insts/get_reg {_:ip}'
data modify storage redvm insts append value 'scoreboard players set ip redvm.regs 14'
data modify storage redvm insts append value 'scoreboard players set ip redvm.regs 1000'
data modify storage redvm insts append value 'function redvm:insts/get_reg {_:sp2}'
data modify storage redvm insts append value 'scoreboard players operation sp2 redvm.regs = sp redvm.regs'
data modify storage redvm insts append value 'scoreboard players remove sp2 redvm.regs 5'
data modify storage redvm insts append value 'scoreboard players add sp redvm.regs 0'
data modify storage redvm insts append value 'function redvm:insts/get_local {_:1}'
data modify storage redvm insts append value 'function redvm:insts/get_local {_:2}'
data modify storage redvm insts append value 'function redvm:insts/add'
data modify storage redvm insts append value 'function redvm:insts/set_local {_:0}'
data modify storage redvm insts append value 'scoreboard players remove sp redvm.regs 0'
data modify storage redvm insts append value 'function redvm:insts/set_reg {_:sp2}'
data modify storage redvm insts append value 'function redvm:insts/set_reg {_:ip}'
data modify storage redvm insts append value 'function redvm:insts/get_reg {_:sp2}'
data modify storage redvm insts append value 'scoreboard players operation sp2 redvm.regs = sp redvm.regs'
data modify storage redvm insts append value 'scoreboard players remove sp2 redvm.regs 2'
data modify storage redvm insts append value 'scoreboard players add sp redvm.regs 1'
data modify storage redvm insts append value 'scoreboard players add sp redvm.regs 1'
data modify storage redvm insts append value 'function redvm:insts/const {_:12}'
data modify storage redvm insts append value 'function redvm:insts/const {_:69}'
data modify storage redvm insts append value 'function redvm:insts/get_reg {_:ip}'
data modify storage redvm insts append value 'scoreboard players set ip redvm.regs 3'
data modify storage redvm insts append value 'scoreboard players remove sp redvm.regs 2'
data modify storage redvm insts append value 'function redvm:insts/set_local {_:2}'
data modify storage redvm insts append value 'scoreboard players remove sp redvm.regs 1'
data modify storage redvm insts append value 'function redvm:insts/set_reg {_:sp2}'
data modify storage redvm insts append value 'function redvm:insts/set_reg {_:ip}'