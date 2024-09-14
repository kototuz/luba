use std::process::exit;

#[derive(Clone, Debug, PartialEq)]
pub struct Loc {
    pub row: usize,
    pub col: usize,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    pub cur: usize,
    pub peeked: Option<Token<'a>>,
    pub loc: Loc
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub text: &'a str
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Name,
    StrLit,
    Num,
    Eq,
    Plus,
    Minus,
    Slash,
    Star,
    Semicolon,
    OpenParen,
    CloseParen,
}



impl<'a> Lexer<'a> {
    pub fn new() -> Self {
        Self {
            cur: 0,
            peeked: None,
            loc: Loc { row: 0, col: 0 }
        }
    }

    pub fn peek(&mut self, src: &'a [u8]) -> Option<Token<'a>> {
        if self.peeked.is_some() {
            return self.peeked.clone();
        }

        if self.cur == src.len() { return None; }
        while src[self.cur].is_ascii_whitespace() {
            if src[self.cur] == b'\n' {
                self.loc.row += 1;
                self.loc.col = 0;
            } else {
                self.loc.col += 1;
            }
            self.cur += 1;
            if self.cur == src.len() { return None; }
        }

        let kind: TokenKind;
        let mut end = self.cur;
        match src[end] {
            b'=' => { kind = TokenKind::Eq;         end += 1; },
            b'+' => { kind = TokenKind::Plus;       end += 1; },
            b'-' => { kind = TokenKind::Minus;      end += 1; },
            b'*' => { kind = TokenKind::Star;       end += 1; },
            b'/' => { kind = TokenKind::Slash;      end += 1; },
            b';' => { kind = TokenKind::Semicolon;  end += 1; },
            b'(' => { kind = TokenKind::OpenParen;  end += 1; },
            b')' => { kind = TokenKind::CloseParen; end += 1; },
            s @ _ => {
                end += 1;
                if s.is_ascii_alphabetic() {
                    kind = TokenKind::Name;
                    while end < src.len() && src[end].is_ascii_alphanumeric() {
                        end += 1;
                    }
                } else if s.is_ascii_digit() {
                    kind = TokenKind::Num;
                    while end < src.len() && src[end].is_ascii_digit() {
                        end += 1;
                    }
                } else if s == b'"' {
                    kind = TokenKind::StrLit;
                    while end < src.len() && src[end] != b'"' {
                        end += 1
                    }
                    end += 1;
                } else {
                    eprintln!(
                        "ERROR:{}: undefined token",
                        self.loc
                    );
                    exit(1);
                }
            }
        }

        self.peeked = Some(Token { kind, text: std::str::from_utf8(&src[self.cur..end]).unwrap() });
        self.peeked.clone()
    }

    pub fn next(&mut self, src: &'a [u8]) -> Option<Token<'a>> {
        if self.peeked.is_none() { let _ = self.peek(src); }
        if let Some(t) = &self.peeked {
            self.loc.col += t.text.len();
            self.cur += t.text.len();
            self.peeked.take()
        } else { None }
    }

    pub fn expect_next(&mut self, src: &'a [u8]) -> Token<'a> {
        self.next(src).unwrap_or_else(|| {
            eprintln!(
                "ERROR:{}: token was expected but it did not appear",
                self.loc
            );
            exit(1);
        })
    }

    pub fn expect_peek(&mut self, src: &'a [u8]) -> Token<'a> {
        self.peek(src).unwrap_or_else(|| {
            eprintln!(
                "ERROR:{}: token was expected, but it did not appear",
                self.loc
            );
            exit(1);
        })
    }

    pub fn expect_next_oneof(&mut self, src: &'a [u8], exp_tks: &[TokenKind]) -> Token<'a> {
        let ret = self.peek(src).unwrap_or_else(|| {
            eprintln!(
                "ERROR:{}: one of {:?} was expected, but it did not appear",
                self.loc,
                exp_tks
            );
            exit(1);
        });

        for tk in exp_tks {
            if ret.kind == *tk {
                let _ = self.next(src);
                return ret;
            }
        }

        eprint!("ERROR:{}: one of the [ ", self.loc);
        for tk in exp_tks { eprint!("`{tk}` "); }
        eprint!("] was expected, but found `{}`\n", ret.kind);

        exit(1);
    }

    pub fn expect_next_eq(&mut self, src: &'a [u8], exp_tk: TokenKind) -> Token<'a> {
        let ret = self.peek(src).unwrap_or_else(|| {
            eprintln!(
                "ERROR:{}: token `{}` was expected, but it did not appear",
                self.loc,
                exp_tk,
            );
            exit(1);
        });

        if ret.kind != exp_tk {
            eprintln!(
                "ERROR:{}: token `{}` was expected, but found `{}`",
                self.loc,
                exp_tk,
                ret.kind
            );
            exit(1);
        }

        let _ = self.next(src);
        ret
    }
}

use std::fmt;
impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.row, self.col)
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            TokenKind::Name       => "name",
            TokenKind::StrLit     => "string literal",
            TokenKind::Num        => "number",
            TokenKind::Eq         => "=",
            TokenKind::Plus       => "+",
            TokenKind::Minus      => "-",
            TokenKind::Slash      => "/",
            TokenKind::Star       => "*",
            TokenKind::Semicolon  => ";",
            TokenKind::OpenParen  => "(",
            TokenKind::CloseParen => ")",
        })
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    const SOURCE: &[u8] = "
        num1 = 324;\n\t\
        num2 =    345;\
        \n\n\nnum3=4;\n\
        str = \"Hello world\";
    ".as_bytes();

    #[test]
    fn test_next() {
        let mut lexer = Lexer::new();
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

        let mut i = 0;
        while let Some(tok) = &lexer.next(SOURCE) {
            assert_eq!(expected[i].0, tok.text, "{}", i);
            i += 1;
        }
    }

    #[test]
    fn test_next_with_eof() { 
        let mut lexer = Lexer::new();
        assert!(lexer.next("".as_bytes()).is_none());
    }

    #[test]
    fn test_peek() {
        let mut lexer = Lexer::new();

        let t1 = lexer.peek(SOURCE).unwrap();
        let t2 = lexer.peek(SOURCE).unwrap();
        assert_eq!(t1.text, t2.text);

        let t3 = lexer.next(SOURCE).unwrap();
        assert_eq!(t1.text, t3.text);

        let t3 = lexer.next(SOURCE).unwrap();
        assert_ne!(t1.text, t3.text);
    }
}

// TODO: better error reporting
