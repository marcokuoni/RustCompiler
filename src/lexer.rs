use std::borrow::Cow;
use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token<'a> {
    pub span: Span,
    pub kind: TokenKind<'a>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenKind<'a> {
    Fix(TokenTag),
    Identifier(&'a str),
    Integer(i32),
    StringLiteral(Cow<'a, str>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TokenTag {
    // keywords
    Class,
    Else,
    Extends,
    If,
    Implements,
    Instanceof,
    Interface,
    New,
    Return,
    While,
    // operators
    And,
    Assign,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Plus,
    Minus,
    Times,
    Divide,
    Modulo,
    Not,
    Or,
    // punctuation
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    OpenParen,
    CloseParen,
    Comma,
    Period,
    Semicolon,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Diagnostic {
    pub span: Span,
    pub kind: LexErrorKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LexErrorKind {
    UnterminatedString,
    UnknownEscape(u8),
    IntOverflow,
    Unexpected(u8),
    UnclosedComment,
}

#[derive(Default, Debug)]
pub struct Diagnostics {
    inner: Vec<Diagnostic>,
}
impl Diagnostics {
    pub fn push(&mut self, span: Span, kind: LexErrorKind) {
        self.inner.push(Diagnostic { span, kind });
    }
    pub fn as_slice(&self) -> &[Diagnostic] {
        &self.inner
    }
}

pub struct Lexer<'a> {
    input: &'a [u8],
    pos: usize,
    diags: Diagnostics,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            input: src.as_bytes(),
            pos: 0,
            diags: Diagnostics::default(),
        }
    }
    pub fn diagnostics(&self) -> &[Diagnostic] {
        self.diags.as_slice()
    }
    pub fn into_diagnostics(self) -> Diagnostics {
        self.diags
    }

    #[inline]
    fn peek(&self) -> Option<u8> {
        self.input.get(self.pos).copied()
    }
    #[inline]
    fn peek2(&self) -> Option<u8> {
        self.input.get(self.pos + 1).copied()
    }
    #[inline]
    fn bump(&mut self) -> Option<u8> {
        let b = self.peek()?;
        self.pos += 1;
        Some(b)
    }
    #[inline]
    fn eat(&mut self, b: u8) -> bool {
        if self.peek() == Some(b) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    #[inline]
    fn skip_ws_and_comments(&mut self) {
        loop {
            // skip ASCII whitespace
            while matches!(self.peek(), Some(b) if b == b' ' || b == b'\n' || b == b'\r' || b == b'\t' || b == b'\x0C')
            {
                self.pos += 1;
            }
            // line comment //
            if self.peek() == Some(b'/') && self.peek2() == Some(b'/') {
                self.pos += 2;
                if let Some(off) = memchr::memchr(b'\n', &self.input[self.pos..]) {
                    self.pos += off + 1;
                } else {
                    self.pos = self.input.len();
                }
                continue;
            }
            // block comment /* ... */ (no nesting)
            if self.peek() == Some(b'/') && self.peek2() == Some(b'*') {
                let start = self.pos;
                self.pos += 2;
                while let Some(b) = self.peek() {
                    if b == b'*' && self.peek2() == Some(b'/') {
                        self.pos += 2;
                        break;
                    }
                    self.pos += 1;
                }
                if self.peek().is_none() {
                    self.diags.push(
                        Span {
                            start,
                            end: self.pos,
                        },
                        LexErrorKind::UnclosedComment,
                    );
                }
                continue;
            }
            break;
        }
    }

    #[inline]
    fn span_from(&self, start: usize) -> Span {
        Span {
            start,
            end: self.pos,
        }
    }

    fn lex_identifier<'b>(&'b mut self, _first: u8) -> Token<'a>
    where
        'a: 'b,
    {
        let start = self.pos - 1; // first already consumed
        while let Some(b) = self.peek() {
            if is_ident_continue(b) {
                self.pos += 1;
            } else {
                break;
            }
        }
        let s = unsafe { std::str::from_utf8_unchecked(&self.input[start..self.pos]) };
        Token {
            span: self.span_from(start),
            kind: TokenKind::Identifier(s),
        }
    }

    fn lex_number(&mut self, first: u8) -> Token<'a> {
        let start = self.pos - 1;
        let mut val: i32 = (first - b'0') as i32;
        let mut overflow = false;
        while let Some(b) = self.peek() {
            if b.is_ascii_digit() {
                match val
                    .checked_mul(10)
                    .and_then(|v| v.checked_add((b - b'0') as i32))
                {
                    Some(v) => val = v,
                    None => {
                        overflow = true;
                        // keep consuming digits to finish the token
                    }
                }
                self.pos += 1;
            } else {
                break;
            }
        }
        if overflow {
            val = i32::MAX;
            self.diags.push(
                Span {
                    start,
                    end: self.pos,
                },
                LexErrorKind::IntOverflow,
            );
        }
        Token {
            span: self.span_from(start),
            kind: TokenKind::Integer(val),
        }
    }

    fn lex_string(&mut self) -> Token<'a> {
        let start = self.pos - 1; // opening quote consumed by caller
        let mut owned: Option<String> = None;
        while let Some(b) = self.bump() {
            match b {
                b'"' => {
                    let end = self.pos; // points after closing quote
                    if let Some(s) = owned {
                        return Token {
                            span: Span { start, end },
                            kind: TokenKind::StringLiteral(Cow::Owned(s)),
                        };
                    }
                    let raw = unsafe {
                        std::str::from_utf8_unchecked(&self.input[(start + 1)..(end - 1)])
                    };
                    return Token {
                        span: Span { start, end },
                        kind: TokenKind::StringLiteral(Cow::Borrowed(raw)),
                    };
                }
                b'\\' => {
                    let mut s = owned.take().unwrap_or_else(|| {
                        let head = unsafe {
                            std::str::from_utf8_unchecked(&self.input[(start + 1)..(self.pos - 1)])
                        };
                        head.to_string()
                    });
                    let esc = match self.bump() {
                        Some(e) => e,
                        None => {
                            self.diags.push(
                                Span {
                                    start,
                                    end: self.pos,
                                },
                                LexErrorKind::UnterminatedString,
                            );
                            break;
                        }
                    };
                    match esc {
                        b'"' => s.push('"'),
                        b'\\' => s.push('\\'),
                        b'n' => s.push('\n'),
                        b't' => s.push('\t'),
                        b'r' => s.push('\r'),
                        b'0' => s.push('\0'),
                        other => {
                            self.diags.push(
                                Span {
                                    start: self.pos - 1,
                                    end: self.pos,
                                },
                                LexErrorKind::UnknownEscape(other),
                            );
                            s.push(other as char);
                        }
                    }
                    owned = Some(s);
                }
                ch => {
                    if let Some(ref mut s) = owned {
                        s.push(ch as char);
                    }
                }
            }
        }
        // EOF without closing quote
        self.diags.push(
            Span {
                start,
                end: self.pos,
            },
            LexErrorKind::UnterminatedString,
        );
        Token {
            span: self.span_from(start),
            kind: TokenKind::StringLiteral(Cow::Owned(owned.unwrap_or_default())),
        }
    }

    fn keyword_or_ident(token: Token<'a>) -> Token<'a> {
        if let TokenKind::Identifier(s) = token.kind {
            let tag = match s {
                "class" => Some(TokenTag::Class),
                "else" => Some(TokenTag::Else),
                "extends" => Some(TokenTag::Extends),
                "if" => Some(TokenTag::If),
                "implements" => Some(TokenTag::Implements),
                "instanceof" => Some(TokenTag::Instanceof),
                "interface" => Some(TokenTag::Interface),
                "new" => Some(TokenTag::New),
                "return" => Some(TokenTag::Return),
                "while" => Some(TokenTag::While),
                _ => None,
            };
            if let Some(t) = tag {
                return Token {
                    span: token.span,
                    kind: TokenKind::Fix(t),
                };
            }
            return Token {
                span: token.span,
                kind: TokenKind::Identifier(s),
            };
        }
        token
    }

    fn fixed_or_error(&mut self) -> Option<Token<'a>> {
        use TokenTag::*;
        let start = self.pos;
        let c = self.bump()?;
        // two-char ops
        let two = match (c, self.peek()) {
            (b'=', Some(b'=')) => Some(Equal),
            (b'!', Some(b'=')) => Some(NotEqual),
            (b'<', Some(b'=')) => Some(LessEqual),
            (b'>', Some(b'=')) => Some(GreaterEqual),
            (b'&', Some(b'&')) => Some(And),
            (b'|', Some(b'|')) => Some(Or),
            _ => None,
        };
        if let Some(tag) = two {
            self.pos += 1;
            return Some(Token {
                span: Span {
                    start,
                    end: self.pos,
                },
                kind: TokenKind::Fix(tag),
            });
        }

        let tag = match c {
            b'!' => Some(Not),
            b'=' => Some(Assign),
            b'>' => Some(Greater),
            b'<' => Some(Less),
            b'+' => Some(Plus),
            b'-' => Some(Minus),
            b'*' => Some(Times),
            b'%' => Some(Modulo),
            b'{' => Some(OpenBrace),
            b'}' => Some(CloseBrace),
            b'[' => Some(OpenBracket),
            b']' => Some(CloseBracket),
            b'(' => Some(OpenParen),
            b')' => Some(CloseParen),
            b',' => Some(Comma),
            b'.' => Some(Period),
            b';' => Some(Semicolon),
            b'/' => Some(Divide),
            _ => None,
        };
        match tag {
            Some(t) => Some(Token {
                span: Span {
                    start,
                    end: self.pos,
                },
                kind: TokenKind::Fix(t),
            }),
            None => {
                self.diags.push(
                    Span {
                        start,
                        end: self.pos,
                    },
                    LexErrorKind::Unexpected(c),
                );
                // Skip unknown and continue lexing
                self.next()
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        self.skip_ws_and_comments();
        let c = self.bump()?;
        let tok = match c {
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => Self::keyword_or_ident(self.lex_identifier(c)),
            b'0'..=b'9' => self.lex_number(c),
            b'"' => self.lex_string(),
            b'/' => {
                // If it were a comment, skip_ws_and_comments would have eaten it.
                Token {
                    span: Span {
                        start: self.pos - 1,
                        end: self.pos,
                    },
                    kind: TokenKind::Fix(TokenTag::Divide),
                }
            }
            _ => match self.fixed_or_error() {
                Some(t) => t,
                None => return None,
            },
        };
        Some(tok)
    }
}

#[inline]
fn is_ident_continue(b: u8) -> bool {
    #[cfg(feature = "unicode")]
    {
        if b < 0x80 {
            (b as char == '_' || (b as char).is_ascii_alphanumeric())
        } else {
            true
        }
    }

    #[cfg(not(feature = "unicode"))]
    {
        b == b'_' || (b as char).is_ascii_alphanumeric()
    }
}
