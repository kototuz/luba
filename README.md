# red
Programming language based on minecraft commands

## Snippet

``` text
fn main() {
    a := 2;
    b := a*a;
    c := a + b;

    d;
    d = c/2;

    if d == 12 {
        @cmd "say hello, world!";
    } else {
        @cmd "say bye, world!";
    }
}
```

## Virtual Machine

The language compiles into the `mcfunction` file.
The **redvm** virtual machine executes this file.
Those the vm loads your program.
After that the vm runs.

You can implement your own vm instead of using the default.
Note that instructions are basicly minecraft functions and
must have prefix `redvm:insts/`.

List of instructions (functions):
- const     `{_:<number>}`
- get_local `{_:<index>}`
- set_local `{_:<index>}`
- see_local `{_:<index>}`
- log       `{_:<index>}`
- set_reg   `{_:<reg>}`
- get_reg   `{_:<reg>}`
- jmp_if    `{_:<ip>}`
- call      `{_:<ip>}`
- add
- sub
- mul
- div
- eq
- ne
- gt
- ge
- lt
- le
- and
- or
