fn add(a, b) int {
    return a + b
}

fn fizzbuzz() {
    fizz := 12341234
    for i := 1; i <= 20; i = i+1 {
        fizz := i % 3 == 0
        buzz := i % 5 == 0
        if fizz && buzz {
            @cmd "say FizzBuzz"
        } else if fizz {
            @cmd "say Fizz"
        } else if buzz {
            @cmd "say Buzz"
        } else {
            @log "i"
        }
    }
}

fn main() {
    fizzbuzz()

    for i := 0; i < 10; i = i+1 {
        @cmd "say Hello"
    }

    for i := 0; i < 10; i = i+1 {
        @cmd "say Bye"
    }
}
