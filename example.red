fn pow(n, p) int {
    for result := n; p > 1; p = p-1 {
        result = result*n
    }

    return result
}

fn bn(b1, q, n) int {
    return b1*pow(q, n-1)
}

fn sn(b1, q, n) int {
    return b1*(1 - pow(q, n))/(1-q)
}

fn fizzbuzz() {
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
}
