fn pow(n, p) int {
    result := n;
    for {
        if p > 1 {
            result = result*n;
            p = p-1;
            continue;
        }
        break;
    }
    return result;
}

fn bn(b1, q, n) int {
    return b1*pow(q, n-1);
}

fn sn(b1, q, n) int {
    return b1*(1 - pow(q, n))/(1-q);
}

fn main() {
    res := bn(2, 2, 5);
    sum := sn(2, 2, 5);

    @log "res";
    @log "sum";
}
