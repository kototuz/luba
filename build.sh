mkdir -p ./bin/

case "$1" in
    "example-vars")
        rustc -o ./bin/mcs -g ./src/main.rs
        ./bin/mcs examples/vars
        ;;

    "example-funcs")
        rustc -o ./bin/mcs -g ./src/main.rs
        ./bin/mcs examples/functions
        ;;

    "example-if")
        rustc -o ./bin/mcs -g ./src/main.rs
        ./bin/mcs examples/if_stmt
        ;;

    "test")
        rustc --test -o ./bin/mcs -g ./src/main.rs
        ./bin/mcs
        ;;

    "debug-parse-expr")
        rustc -g -o ./bin/debug-parse-expr ./debug/parse_expr.rs
        ./bin/debug-parse-expr
        ;;

    "debug-parse-stmt")
        rustc -g -o ./bin/debug-parse-stmt ./debug/parse_stmt.rs
        ./bin/debug-parse-stmt
        ;;

    *)
        rustc -o ./bin/mcs -g ./src/main.rs
        ;;
esac
