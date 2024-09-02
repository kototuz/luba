use std::{fmt, process};

#[derive(Debug)]
pub struct Lexer<'a> {
    src: &'a [u8],
}

#[derive(Debug)]
pub enum TokenKind {
    Name,
    StrLit,
    Num,

    Eq,
    Plus,
    Minus,
    Slash,
    Star,
    Semicolon
}

#[derive(Debug)]
pub struct Token<'a> {
    pub data: &'a [u8],
    pub kind: TokenKind
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a [u8]) -> Self {
        Self { src }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.src = self.src.trim_ascii_start();
        if self.src.is_empty() { return None; }

        let mut result_len = 1;
        let mut result = Token { data: &self.src, kind: TokenKind::Name };
        match self.src[0] {
            b'=' => result.kind = TokenKind::Eq,
            b'+' => result.kind = TokenKind::Plus,
            b'-' => result.kind = TokenKind::Minus,
            b'*' => result.kind = TokenKind::Star,
            b'/' => result.kind = TokenKind::Slash,
            b';' => result.kind = TokenKind::Semicolon,
            _    => {
                if self.src[0].is_ascii_alphabetic() {
                    result.kind = TokenKind::Name;
                    while result.data[result_len].is_ascii_alphanumeric() { result_len += 1; }
                } else if self.src[0].is_ascii_digit() {
                    result.kind = TokenKind::Num;
                    while result.data[result_len].is_ascii_digit() { result_len += 1; }
                } else if self.src[0] == b'"' {
                    result.kind = TokenKind::StrLit;
                    while result.data[result_len] != b'"' { result_len += 1 }
                    result_len += 1;
                } else {
                    eprintln!("ERROR: undefined token at {:?}", std::str::from_utf8(&self.src[..5]).unwrap());
                    process::exit(1);
                }
            }
        }

        result.data = &result.data[..result_len];
        self.src = &self.src[result_len..];
        Some(result)
    }
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {:?})", std::str::from_utf8(self.data).unwrap(), self.kind)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_lexer_next_token() {
        let source = "num1 = 324;\n\t    num2 =    345;\n\n\nnum3=4;".as_bytes();
        let lexer = super::Lexer::new(source);

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
            assert_eq!(expected[i], x.data);
        }
    }
}
