use std::{fmt, process};

#[derive(Debug)]
pub struct LexerIterator<'a> {
    src: &'a str,
}

pub struct Lexer<'a> {
    inner: std::iter::Peekable<LexerIterator<'a>>
}

#[derive(Debug, PartialEq)]
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
    pub data: &'a str,
    pub kind: TokenKind
}



// TODO: needs refactoring
impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self { inner: LexerIterator{src}.peekable() }
    }

    pub fn expect_next(&mut self) -> Token<'a>  {
        self.inner.next().unwrap_or_else(|| {
            eprintln!("ERROR: token was expected but it did not appear");
            process::exit(1);
        })
    }
    
    pub fn expect_peek(&mut self) -> &Token<'a> {
        self.inner.peek().unwrap_or_else(|| {
            eprintln!("ERROR: token was expected but it did not appear");
            process::exit(1);
        })
    }

    pub fn expect_specific_next(&mut self, tk: TokenKind) -> Token<'a> {
        let token = self.expect_next();
        if token.kind != tk {
            eprintln!("ERROR: token `{:?}` was expected, but found `{}`", tk, token.data);
            process::exit(1);
        }

        token
    }

    pub fn peek(&mut self) -> Option<&Token<'a>> {
        self.inner.peek()
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

impl<'a> Iterator for LexerIterator<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.src = self.src.trim_ascii_start();
        if self.src.is_empty() { return None; }

        let mut result = Token { data: &self.src, kind: TokenKind::Name };
        let bytes = self.src.as_bytes();
        let mut result_len = 1;
        match bytes[0] {
            b'=' => result.kind = TokenKind::Eq,
            b'+' => result.kind = TokenKind::Plus,
            b'-' => result.kind = TokenKind::Minus,
            b'*' => result.kind = TokenKind::Star,
            b'/' => result.kind = TokenKind::Slash,
            b';' => result.kind = TokenKind::Semicolon,
            _    => {
                if bytes[0].is_ascii_alphabetic() {
                    result.kind = TokenKind::Name;
                    while bytes[result_len].is_ascii_alphanumeric() { result_len += 1; }
                } else if bytes[0].is_ascii_digit() {
                    result.kind = TokenKind::Num;
                    while bytes[result_len].is_ascii_digit() { result_len += 1; }
                } else if bytes[0] == b'"' {
                    result.kind = TokenKind::StrLit;
                    while bytes[result_len] != b'"' { result_len += 1 }
                    result_len += 1;
                } else {
                    eprintln!("ERROR: undefined token at `{}...`", &self.src[..5]);
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
        write!(f, "({}, {:?})", self.data, self.kind)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_lexer_next_token() {
        let source = "num1 = 324;\n\t    num2 =    345;\n\n\nnum3=4;";
        let lexer = super::Lexer::new(source);

        let expected = [
            "num1",
            "=",
            "324",
            ";",
            "num2",
            "=",
            "345",
            ";",
            "num3",
            "=",
            "4",
            ";",
            "\"Hello world\"",
        ];

        for (i, x) in lexer.enumerate() {
            assert_eq!(expected[i], x.data);
        }
    }
}
