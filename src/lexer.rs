use super::Result;

#[derive(Clone, Debug, PartialEq)]
pub struct Loc {
    pub row: usize,
    pub col: usize
}

#[derive(Debug)]
pub struct Lexer<'a> {
    pub loc: Loc,
    src: &'a [u8],
    curr: usize,
    peeked: Option<Token>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub text: &'static str
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
    OpenCurly,
    CloseCurly,
    KeywordFn,
}



impl<'a> Lexer<'a> {
    pub fn new(src: &'a [u8]) -> Self {
        Self {
            src,
            curr: 0,
            peeked: None,
            loc: Loc { row: 1, col: 0 }
        }
    }

    pub fn peek(&mut self) -> Result<Option<Token>> {
        if self.peeked.is_some() {
            return Ok(self.peeked.clone());
        }

        if self.curr == self.src.len() { return Ok(None); }
        while self.src[self.curr].is_ascii_whitespace() {
            if self.src[self.curr] == b'\n' {
                self.loc.row += 1;
                self.loc.col = 0;
            } else {
                self.loc.col += 1;
            }
            self.curr += 1;
            if self.curr == self.src.len() { return Ok(None); }
        }


        let mut res_kind: TokenKind;
        let mut res_text_end = self.curr+1;
        match self.src[self.curr] {
            b'=' => { res_kind = TokenKind::Eq;         },
            b'+' => { res_kind = TokenKind::Plus;       },
            b'-' => { res_kind = TokenKind::Minus;      },
            b'*' => { res_kind = TokenKind::Star;       },
            b'/' => { res_kind = TokenKind::Slash;      },
            b';' => { res_kind = TokenKind::Semicolon   },
            b'(' => { res_kind = TokenKind::OpenParen;  },
            b')' => { res_kind = TokenKind::CloseParen; },
            b'{' => { res_kind = TokenKind::OpenCurly;  },
            b'}' => { res_kind = TokenKind::CloseCurly; },
            s @ _ => {
                if s.is_ascii_alphabetic() {
                    res_kind = TokenKind::Name;
                    while res_text_end < self.src.len() && self.src[res_text_end].is_ascii_alphanumeric() {
                        res_text_end += 1;
                    }
                } else if s.is_ascii_digit() {
                    res_kind = TokenKind::Num;
                    while res_text_end < self.src.len() && self.src[res_text_end].is_ascii_digit() {
                        res_text_end += 1;
                    }
                } else if s == b'"' {
                    res_kind = TokenKind::StrLit;
                    while res_text_end < self.src.len() && self.src[res_text_end] != b'"' {
                        res_text_end += 1
                    }
                    res_text_end += 1;
                } else {
                    eprintln!("ERROR:{}: undefined token", self.loc);
                    return Err(());
                }
            }
        }

        unsafe {
            let text = std::str::from_utf8_unchecked(
                std::slice::from_raw_parts(
                    self.src.as_ptr().add(self.curr),
                    res_text_end-self.curr
                )
            );

            if text == "fn" { res_kind = TokenKind::KeywordFn }
            self.peeked = Some(Token { kind: res_kind, text });
        }

        Ok(self.peeked.clone())
    }

    pub fn next(&mut self) -> Result<Option<Token>> {
        if self.peeked.is_none() { let _ = self.peek()?; }
        if let Some(t) = &self.peeked {
            self.loc.col += t.text.len();
            self.curr += t.text.len();
            Ok(self.peeked.take())
        } else { Ok(None) }
    }

    pub fn expect_next(&mut self, exp_tk: TokenKind) -> Result<Token> {
        match self.peek()? {
            None => {
                eprintln!(
                    "ERROR:{}: token `{}` was expected, but it did not appear",
                    self.loc,
                    exp_tk,
                );
                Err(())
            },
            Some(token) if token.kind != exp_tk => {
                eprintln!(
                    "ERROR:{}: token `{}` was expected, but found `{}`",
                    self.loc,
                    exp_tk,
                    token.kind
                );
                Err(())
            },
            _ => Ok(self.next().unwrap().unwrap())
        }
    }

    pub fn expect_next_oneof(&mut self, exp_tks: &[TokenKind]) -> Result<Token> {
        match self.peek()? {
            None => {
                eprint!("ERROR:{}: one of the [ ", self.loc);
                for tk in exp_tks { eprint!("`{tk}` "); }
                eprint!("] was expected, but it did not appear\n");
                Err(())
            },
            Some(token) if !exp_tks.contains(&token.kind) => {
                eprint!("ERROR:{}: one of the [ ", self.loc);
                for tk in exp_tks { eprint!("`{tk}` "); }
                eprint!("] was expected, but found `{}`\n", token.kind);
                Err(())
            },
            _ => Ok(self.next().unwrap().unwrap())
        }
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
            TokenKind::KeywordFn  => "fn",
            TokenKind::OpenCurly  => "{",
            TokenKind::CloseCurly => "}",
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
        let mut lexer = Lexer::new(SOURCE);
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
        while let Some(tok) = &lexer.next().unwrap() {
            assert_eq!(expected[i].0, tok.text, "{}", i);
            assert_eq!(expected[i].1, tok.kind);
            i += 1;
        }
    }

    #[test]
    fn test_next_with_undefined_token() {
        let mut lexer = Lexer::new(b"$");
        assert!(lexer.next().is_err());
    }

    #[test]
    fn test_next_with_eof() { 
        let mut lexer = Lexer::new("".as_bytes());
        assert!(lexer.next().unwrap().is_none());
    }

    #[test]
    fn test_peek() {
        let mut lexer = Lexer::new(SOURCE);

        let t1 = lexer.peek().unwrap().unwrap();
        let t2 = lexer.peek().unwrap().unwrap();
        assert_eq!(t1.text, t2.text);

        let t3 = lexer.next().unwrap().unwrap();
        assert_eq!(t1.text, t3.text);

        let t3 = lexer.next().unwrap().unwrap();
        assert_ne!(t1.text, t3.text);
    }
}
