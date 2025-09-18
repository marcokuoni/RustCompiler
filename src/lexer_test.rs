use pretty_assertions::assert_eq;
use rust_lexer::*;

#[test]
fn class_and_braces() {
    let src = "class A { }";
    let mut lex = Lexer::new(src);
    let kinds: Vec<_> = (&mut lex).map(|t| t.kind).collect();
    assert_eq!(kinds.len(), 4);
}

#[test]
fn strings_escape_and_diag() {
    let src = "\"hi\\n\" \"unterminated";
    let mut lex = Lexer::new(src);
    let ts: Vec<_> = (&mut lex).collect();
    assert!(matches!(ts[0].kind, TokenKind::StringLiteral(_)));
    assert!(!lex.diagnostics().is_empty());
}

#[test]
fn integers_and_overflow() {
    let src = "0 42 2147483648"; // last overflows i32
    let mut lex = Lexer::new(src);
    let ints: Vec<_> = (&mut lex)
        .filter_map(|t| {
            if let TokenKind::Integer(v) = t.kind {
                Some(v)
            } else {
                None
            }
        })
        .collect();
    assert_eq!(ints[0], 0);
    assert_eq!(ints[1], 42);
    assert_eq!(ints[2], i32::MAX);
    assert!(!lex.diagnostics().is_empty());
}
