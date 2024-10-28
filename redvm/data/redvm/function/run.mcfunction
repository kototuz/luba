data remove storage redvm insts
scoreboard players reset * redvm.local
scoreboard objectives add redvm.local dummy
scoreboard objectives add redvm.regs dummy
scoreboard players set sp redvm.regs 0
scoreboard players set sp2 redvm.regs 0
scoreboard players set ip redvm.regs 0

$$(load_program)

##data modify storage mcs insts append value "scoreboard players operation sp2 regs = sp regs"
##data modify storage mcs insts append value "function test:vm/insts/const {_:100000}"
##data modify storage mcs insts append value "function test:vm/insts/get_local {_:0}"
##data modify storage mcs insts append value "function test:vm/insts/const {_:1}"
##data modify storage mcs insts append value "function test:vm/insts/sub"
##data modify storage mcs insts append value "function test:vm/insts/jmp_if {_:7}"
##data modify storage mcs insts append value "scoreboard players set ip regs 10"     
##data modify storage mcs insts append value "function test:vm/insts/const {_:1}"
##data modify storage mcs insts append value "function test:vm/insts/sub"        
##data modify storage mcs insts append value "scoreboard players set ip regs 2"
#
##data modify storage mcs insts append value "function test:vm/insts/const {_:0}"
##data modify storage mcs insts append value "function test:vm/insts/const {_:1}"
##data modify storage mcs insts append value "function test:vm/insts/add"
##data modify storage mcs insts append value "scoreboard players set ip regs 1"

function redvm:loop
