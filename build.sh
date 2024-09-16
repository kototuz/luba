mkdir -p ./bin/

case "$1" in
    "run")
        rustc -o ./bin/mcs -g ./src/main.rs
        ./bin/mcs example.mcs
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
