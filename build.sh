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

    "test")
        rustc --test -o ./bin/mcs -g ./src/main.rs
        ./bin/mcs
        ;;

    "debug-parser")
        rustc -o ./bin/debug-parser ./src/debug_parser.rs
        ./bin/debug-parser
        ;;

    *)
        rustc -o ./bin/mcs -g ./src/main.rs
        ;;
esac
