use rust_compiler::{Lexer, TokenKind, TokenTag};

fn main() {
    let src = r#"
class /* comment */ A {
let x = 42;
if (x >= 10 && x != 13) {
print("hi\n"); // line comment
}
}
"#;

    let mut lex = Lexer::new(src);
    for tok in &mut lex {
        match &tok.kind {
            TokenKind::Fix(t) => println!("{:?} @ {:?}", t, tok.span),
            TokenKind::Identifier(id) => println!("IDENT({}) @ {:?}", id, tok.span),
            TokenKind::Integer(i) => println!("INT({}) @ {:?}", i, tok.span),
            TokenKind::StringLiteral(s) => println!("STR(\"{}\") @ {:?}", s, tok.span),
        }
    }

    if !lex.diagnostics().is_empty() {
        eprintln!("\nDiagnostics:");
        for d in lex.diagnostics() {
            eprintln!("- {:?} at {:?}", d.kind, d.span);
        }
    }
}
