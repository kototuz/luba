use super::*;

use std::mem::transmute;



// TODO: think about this way of lexering
//
//lex.expect_keyword(Keyword::If) {}
//lex.expect_punct(Punct::OpenParen) {}
//lex.expect_ident() -> &str {}
//
//lex.get_punct() -> Option<()>
//lex.get_keyword() -> Option<()>
//lex.get_ident() -> Option<&str>
//
//let (token, loc) = lex.expect_any();
//match token {
//    Token::BinOp(op_kind) => ...,
//    Token::CloseParen => ...,
//    Token::Keyword(Keyword::If) => ...,
//    _ => lex.unexpected_token_err(loc, token)
//}



#[derive(Clone, Debug, PartialEq)]
pub struct Loc {
    pub row: usize,
    pub col: usize
}

#[derive(Debug)]
pub struct Lexer<'a> {
    pub loc: Loc,
    src: &'a [u8],
    pos: usize,
    curr_token_len: usize,
    peeked: Option<Token>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinOpKind {
    Add,
    Mod,
    Sub,
    Mul,
    Div,
    Gt,
    Ge,
    Lt,
    Le,
    Eq,
    Ne,
    And,
    Or,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    If,
    Else,
    Fn,
    Return,
    For,
    Int,
    Break,
    Continue,
    Extern,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Punct {
    Semicolon,
    Comma,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    Colon,
    Eq,
    At
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Ident(&'static str),
    StrLit(&'static str),
    Number(i32),
    BinOp(BinOpKind),
    Keyword(Keyword),
    Punct(Punct),
}

impl<'a> Lexer<'a> {
    const KEYWORDS: &'static [(&'static str, Keyword)] = &[
        ("if",       Keyword::If),
        ("fn",       Keyword::Fn),
        ("return",   Keyword::Return),
        ("else",     Keyword::Else),
        ("for",      Keyword::For),
        ("int",      Keyword::Int),
        ("break",    Keyword::Break),
        ("continue", Keyword::Continue),
        ("extern",   Keyword::Extern),
    ];

    pub fn new(src: &'a [u8]) -> Self {
        Self {
            src,
            pos: 0,
            loc: Loc { row: 1, col: 1 },
            curr_token_len: 0,
            peeked: None,
        }
    }


    fn ident(&mut self) -> Option<&'static str> {
        if !self.src[self.pos].is_ascii_alphabetic() { return None; }
        let mut end: usize = self.pos+1;
        while end < self.src.len() &&
            self.src[end].is_ascii_alphanumeric() { end += 1; }
        let text = self.str_from_range(self.pos..end);
        self.curr_token_len = text.len();
        Some(text)
    }

    fn strlit(&mut self) -> Option<&'static str> {
        if self.src[self.pos] != b'"' { return None; }
        let mut end: usize = self.pos+1;
        while self.src[end] != b'"' {
            end += 1;
            if end == self.src.len() {
                lexical_err!(self.loc, "Unclosed string literal");
            }
        }
        let text = self.str_from_range(self.pos+1..end);
        self.curr_token_len = text.len()+2;
        Some(text)
    }

    fn keyword(&mut self) -> Option<Keyword> {
        let mut end: usize = self.pos+1;
        while end < self.src.len() &&
            self.src[end].is_ascii_alphabetic() {
                end += 1;
        }

        let text = self.str_from_range(self.pos..end);
        for (keyword, kind) in Self::KEYWORDS {
            if text == *keyword {
                self.curr_token_len = text.len();
                return Some(kind.clone());
            }
        }

        None
    }

    fn number(&mut self) -> Option<i32> {
        if !self.src[self.pos].is_ascii_digit() { return None; }
        let mut end: usize = self.pos+1;
        while end < self.src.len() &&
            self.src[end].is_ascii_digit() {
                end += 1;
        }

        match self.str_from_range(self.pos..end).parse::<i32>() {
            Ok(num) => {
                self.curr_token_len = end - self.pos;
                Some(num)
            },
            Err(_) => {
                lexical_err!(self.loc, "Invalid 32-bit integer");
            }
        }
    }

    fn punct(&mut self) -> Option<Punct> {
        let punct = match self.src[self.pos] {
            b',' => Punct::Comma,
            b';' => Punct::Semicolon,
            b'=' => Punct::Eq,
            b'(' => Punct::OpenParen,
            b')' => Punct::CloseParen,
            b'{' => Punct::OpenCurly,
            b'}' => Punct::CloseCurly,
            b':' => Punct::Colon,
            b'@' => Punct::At,
            _ => return None
        };
        self.curr_token_len = 1;
        Some(punct)
    }

    fn bin_op(&mut self) -> Option<BinOpKind> {
        let bin_op_kind = match self.src[self.pos] {
            b'+' => { self.curr_token_len = 1; BinOpKind::Add },
            b'-' => { self.curr_token_len = 1; BinOpKind::Sub },
            b'*' => { self.curr_token_len = 1; BinOpKind::Mul },
            b'/' => { self.curr_token_len = 1; BinOpKind::Div },
            b'%' => { self.curr_token_len = 1; BinOpKind::Mod },
            b'=' if self.src[self.pos+1] == b'=' => { self.curr_token_len = 2; BinOpKind::Eq },
            b'!' if self.src[self.pos+1] == b'=' => { self.curr_token_len = 2; BinOpKind::Ne },
            b'&' if self.src[self.pos+1] == b'&' => { self.curr_token_len = 2; BinOpKind::And },
            b'|' if self.src[self.pos+1] == b'|' => { self.curr_token_len = 2; BinOpKind::Or },
            b'>' => {
                if self.src[self.pos+1] == b'=' {
                    self.curr_token_len = 2;
                    BinOpKind::Ge
                } else {
                    self.curr_token_len = 1;
                    BinOpKind::Gt
                }
            },
            b'<' => {
                if self.src[self.pos+1] == b'=' {
                    self.curr_token_len = 2;
                    BinOpKind::Le
                } else {
                    self.curr_token_len = 1;
                    BinOpKind::Lt
                }
            },
            _ => return None
        };

        Some(bin_op_kind)
    }

    pub fn expect_ident(&mut self) -> &'static str {
        self.pos += self.curr_token_len;
        self.loc.col += self.curr_token_len;
        if !self.skip_whitespace() {
            if let Some(text) = self.ident() {
                return text;
            }
        }

        syntax_err!(self.loc, "Identifier was expected, but it did not appear");
    }

    pub fn expect_punct(&mut self, expected: Punct) {
        let p = self.expect_any();
        if p != Token::Punct(expected.clone()) {
            syntax_err!(self.loc, "Punctuator `{expected}` was expected, but found `{p}`");
        }
    }

    pub fn next_any(&mut self) -> Option<Token> {
        let result = self.peek_any();
        self.peeked = None;
        result
    }

    pub fn peek_any(&mut self) -> Option<Token> {
        if self.peeked.is_some() {
            return self.peeked.clone();
        }
        
        self.pos += self.curr_token_len;
        self.loc.col += self.curr_token_len;
        if self.skip_whitespace() {
            self.curr_token_len = 0;
            return None;
        }

        let result =
            self.bin_op().map(|op| Token::BinOp(op))
            .or_else(|| self.strlit().map(|p| Token::StrLit(p)))
            .or_else(|| self.punct().map(|p| Token::Punct(p)))
            .or_else(|| self.keyword().map(|k| Token::Keyword(k)))
            .or_else(|| self.number().map(|n| Token::Number(n)))
            .or_else(|| self.ident().map(|i| Token::Ident(i)));

        if result.is_none() {
            lexical_err!(self.loc, "Undefined token");
        }

        self.peeked = result.clone();

        result
    }

    pub fn expect_peek_any(&mut self) -> Token {
        self.peek_any().unwrap_or_else(|| {
            syntax_err!(self.loc, "Token was expected, but reached the end");
        })
    }

    pub fn expect_any(&mut self) -> Token {
        self.next_any().unwrap_or_else(|| {
            syntax_err!(self.loc, "Token was expected, but reached the end");
        })
    }

    fn str_from_range(&self, range: std::ops::Range<usize>) -> &'static str {
        unsafe {
            transmute::<&str, &'static str>(
                std::str::from_utf8(&self.src[range])
                    .unwrap_or_else(|_| {
                        lexical_err!(self.loc, "Invalid UTF-8");
                    })
            )
        }
    }

    // returns true if the end is reached
    fn skip_whitespace(&mut self) -> bool {
        if self.pos >= self.src.len() { return true; }
        while self.src[self.pos].is_ascii_whitespace() {
            if self.src[self.pos] == b'\n' {
                self.loc.row += 1;
                self.loc.col = 1;
            } else {
                self.loc.col += 1;
            }
            self.pos += 1;
            if self.pos == self.src.len() { return true; }
        }
        false
    }
}

use std::fmt;
impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.row, self.col)
    }
}

impl fmt::Display for Punct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
         match self {
            Punct::Comma      => write!(f, ","),
            Punct::Semicolon  => write!(f, ";"),
            Punct::Colon      => write!(f, ":"),
            Punct::Eq         => write!(f, "="),
            Punct::OpenParen  => write!(f, "("),
            Punct::CloseParen => write!(f, ")"),
            Punct::OpenCurly  => write!(f, "{{"),
            Punct::CloseCurly => write!(f, "}}"),
            Punct::At         => write!(f, "@"),
        }
    }
}

impl fmt::Display for BinOpKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinOpKind::Add => write!(f, "+"),
            BinOpKind::Sub => write!(f, "-"),
            BinOpKind::Mul => write!(f, "*"),
            BinOpKind::Div => write!(f, "/"),
            BinOpKind::Mod => write!(f, "%"),
            BinOpKind::Eq  => write!(f, "=="),
            BinOpKind::Ne  => write!(f, "!="),
            BinOpKind::Gt  => write!(f, ">"),
            BinOpKind::Ge  => write!(f, ">="),
            BinOpKind::Lt  => write!(f, "<"),
            BinOpKind::Le  => write!(f, "<="),
            BinOpKind::And => write!(f, "&&"),
            BinOpKind::Or  => write!(f, "||"),
        }
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Keyword::If       => write!(f, "if"),
            Keyword::Else     => write!(f, "else"),
            Keyword::Fn       => write!(f, "fn"),
            Keyword::Return   => write!(f, "return"),
            Keyword::For      => write!(f, "for"),
            Keyword::Int      => write!(f, "int"),
            Keyword::Break    => write!(f, "break"),
            Keyword::Continue => write!(f, "continue"),
            Keyword::Extern   => write!(f, "extern"),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Ident(text)   => write!(f, "identifier `{text}`"),
            Token::StrLit(text)  => write!(f, "string literal `{text}`"),
            Token::Number(num)   => write!(f, "number `{num}`"),
            Token::BinOp(kind)   => write!(f, "binary operation `{kind}`"),
            Token::Keyword(kind) => write!(f, "keyword `{kind}`"),
            Token::Punct(kind)   => write!(f, "punctuator `{kind}`"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const SOURCE: &[u8] = "
        num1 = 324;\n\t\
        num2 =    345;\
        \n\n\nnum3=4;\n\
        num3 = num1 == num2;\n\
        fn some() \"HELLO, WORLD\"
    ".as_bytes();

    #[test]
    #[should_panic]
    fn unexpected_token() {
        let mut lexer = Lexer::new(SOURCE);
        match lexer.expect_any() {
            Token::Ident(_) => {},
            t @ _ => { unexpected_token_err!(lexer.loc, t); }
        }
        match lexer.expect_any() {
            Token::Punct(Punct::Semicolon) => {},
            t @ _ => { unexpected_token_err!(lexer.loc, t); }
        }
    }

    #[test]
    #[should_panic]
    fn expect_punct() {
        let mut lexer = Lexer::new(SOURCE);
        lexer.expect_punct(Punct::Eq);
    }

    #[test]
    #[should_panic]
    fn expect_ident() {
        let mut lexer = Lexer::new(SOURCE);
        let _ = lexer.expect_ident();
        let _ = lexer.expect_ident();
    }

    #[test]
    #[should_panic]
    fn illegal_int() {
        let mut lexer = Lexer::new(b"123412341234123412341234123412341234");
        let _ = lexer.expect_any();
    }

    #[test]
    #[should_panic]
    fn illegal_utf8() {
        let mut lexer = Lexer::new(b"\xE0");
        let _ = lexer.expect_any();
    }

    #[test]
    fn peek() {
        let mut lexer = Lexer::new(SOURCE);
        let t1 = lexer.peek_any();
        let t2 = lexer.peek_any();
        let t3 = lexer.next_any();
        let t4 = lexer.next_any();
        assert_eq!(t1, t2);
        assert_eq!(t1, t3);
        assert_ne!(t1, t4);
    }

    #[test]
    fn test_next() {
        let mut lexer = Lexer::new(SOURCE);
        let expected = [
            Token::Ident("num1"),
            Token::Punct(Punct::Eq),
            Token::Number(324),
            Token::Punct(Punct::Semicolon),
            Token::Ident("num2"),
            Token::Punct(Punct::Eq),
            Token::Number(345),
            Token::Punct(Punct::Semicolon),
            Token::Ident("num3"),
            Token::Punct(Punct::Eq),
            Token::Number(4),
            Token::Punct(Punct::Semicolon),
            Token::Ident("num3"),
            Token::Punct(Punct::Eq),
            Token::Ident("num1"),
            Token::BinOp(BinOpKind::Eq),
            Token::Ident("num2"),
            Token::Punct(Punct::Semicolon),
            Token::Keyword(Keyword::Fn),
            Token::Ident("some"),
            Token::Punct(Punct::OpenParen),
            Token::Punct(Punct::CloseParen),
            Token::StrLit("HELLO, WORLD"),
        ];

        for (i, x) in expected.iter().enumerate() {
            let token = lexer.expect_any();
            assert_eq!(token, x.clone(), "{i}");
        }
    }
}


// TODO: comments
// TODO: more convinient way to declare and handle token kinds
