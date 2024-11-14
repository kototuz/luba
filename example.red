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

fn main() {
    res := bn(1, 2, 3)
    @log "res"

    for i := 0; i < 10; i = i+1 {
        @cmd "say hello"
    }

    @cmd "say end!"
}
