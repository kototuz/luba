# projects-mcs
Programming language based on minecraft datapacks

## Virtual Machine
### Instructions:
Constant:
    - const {_:`<value>`}

Binary operations:
    - add
    - sub
    - mul
    - div
    - mod

Comparision:
    - eq
    - ne
    - gt
    - ge
    - lt
    - le

Logical operations:
    - and
    - or

Variable operations:
    - getlocal {_:`<idx>`}
    - setlocal {_:`<idx>`}

Register operations:
    - setsp
    - setsp2
    - setip

## Current state
``` rust
fn main() { // you can create functions
    v0 = 1 * (2 + 3); // create variables
    v1 = (7 + 9) - 3 * 5;
}

fn other() {
    v0 = 123 + 1;
}
```
