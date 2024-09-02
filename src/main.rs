mod lexer;

fn main() {
    let source = "num1 = 324;\n\t    num2 =    345;\n\n\nnum3=4;\"Hello world\"".as_bytes();
    let lexer = lexer::Lexer::new(source);

    let expected = [
        "num1".as_bytes(),
        "=".as_bytes(),
        "324".as_bytes(),
        ";".as_bytes(),
        "num2".as_bytes(),
        "=".as_bytes(),
        "345".as_bytes(),
        ";".as_bytes(),
        "num3".as_bytes(),
        "=".as_bytes(),
        "4".as_bytes(),
        ";".as_bytes(),
        "\"Hello world\"".as_bytes(),
    ];

    for (i, x) in lexer.enumerate() {
        println!("{}", x);
        assert_eq!(expected[i], x.data);
    }
}
