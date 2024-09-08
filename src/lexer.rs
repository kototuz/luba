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
// TODO: we can split `expect` methods and leave just `next()` and `peek()`
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
                    while result_len < bytes.len() && bytes[result_len].is_ascii_alphanumeric() {
                        result_len += 1;
                    }
                } else if bytes[0].is_ascii_digit() {
                    result.kind = TokenKind::Num;
                    while result_len < bytes.len() && bytes[result_len].is_ascii_digit(){
                        result_len += 1;
                    }
                } else if bytes[0] == b'"' {
                    result.kind = TokenKind::StrLit;
                    while result_len < bytes.len() && bytes[result_len] != b'"' {
                        result_len += 1
                    }
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
    use super::*;

    const SOURCE: &str = "
        num1 = 324;\n\t\
        num2 =    345;\
        \n\n\nnum3=4;\n\
        str = \"Hello world\";
    ";

    #[test]
    fn test_next() {
        let lexer = Lexer::new(SOURCE);
        let expected = [
            ("num1",            TokenKind::Name),
            ("=",               TokenKind::Eq),
            ("324",             TokenKind::Num),
            (";",               TokenKind::Semicolon),
            ("num2",            TokenKind::Name),
            ("=",               TokenKind::Eq),
            ("345",             TokenKind::Num),
            (";",               TokenKind::Semicolon),
            ("num3",            TokenKind::Name),
            ("=",               TokenKind::Eq),
            ("4",               TokenKind::Num),
            (";",               TokenKind::Semicolon),
            ("str",             TokenKind::Name),
            ("=",               TokenKind::Eq),
            ("\"Hello world\"", TokenKind::StrLit),
            (";",               TokenKind::Semicolon)
        ];

        for (i, x) in lexer.enumerate() {
            assert_eq!(expected[i].0, x.data);
            assert_eq!(expected[i].1, x.kind);
        }
    }

    #[test]
    fn test_peek() {
        let mut lexer = Lexer::new(SOURCE);

        let t = lexer.peek().unwrap();
        assert_eq!(t.data, "num1");
        assert_eq!(t.kind, TokenKind::Name);

        let t = lexer.next().unwrap();
        assert_eq!(t.data, "num1");
        assert_eq!(t.kind, TokenKind::Name);
    }

    #[test]
    fn test_next_with_one_token() { 
        let source = "adsf";
        let mut lexer = Lexer::new(source);
        assert!(matches!(lexer.next(), Some(_)));
    }
}
