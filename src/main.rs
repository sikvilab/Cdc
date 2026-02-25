use std::env;
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, PartialEq, Eq)]
enum TokenKind {
    KwInt,
    KwMain,
    KwReturn,
    Number(i64),
    LParen,
    RParen,
    LBrace,
    RBrace,
    Plus,
    Minus,
    Star,
    Slash,
    Semicolon,
    Eof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Token {
    kind: TokenKind,
    lexeme: String,
    line: usize,
    col: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CompileError {
    line: usize,
    col: usize,
    message: String,
}

impl CompileError {
    fn new(line: usize, col: usize, message: impl Into<String>) -> Self {
        Self {
            line,
            col,
            message: message.into(),
        }
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}: {}", self.line, self.col, self.message)
    }
}

impl std::error::Error for CompileError {}

struct Lexer<'a> {
    chars: Vec<char>,
    pos: usize,
    line: usize,
    col: usize,
    _src: &'a str,
}

impl<'a> Lexer<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            chars: src.chars().collect(),
            pos: 0,
            line: 1,
            col: 1,
            _src: src,
        }
    }

    fn tokenize(&mut self) -> Result<Vec<Token>, CompileError> {
        let mut tokens = Vec::new();

        while let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                self.bump();
                continue;
            }

            if self.starts_with("//") {
                self.skip_line_comment();
                continue;
            }

            if self.starts_with("/*") {
                self.skip_block_comment()?;
                continue;
            }

            let (line, col) = (self.line, self.col);

            if ch.is_ascii_alphabetic() || ch == '_' {
                let ident = self.consume_while(|c| c.is_ascii_alphanumeric() || c == '_');
                let kind = match ident.as_str() {
                    "int" => TokenKind::KwInt,
                    "main" => TokenKind::KwMain,
                    "return" => TokenKind::KwReturn,
                    _ => {
                        return Err(CompileError::new(
                            line,
                            col,
                            format!("unsupported identifier `{ident}`"),
                        ))
                    }
                };
                tokens.push(Token {
                    kind,
                    lexeme: ident,
                    line,
                    col,
                });
                continue;
            }

            if ch.is_ascii_digit() {
                let digits = self.consume_while(|c| c.is_ascii_digit());
                let value = digits.parse::<i64>().map_err(|_| {
                    CompileError::new(line, col, format!("integer literal out of range: {digits}"))
                })?;
                tokens.push(Token {
                    kind: TokenKind::Number(value),
                    lexeme: digits,
                    line,
                    col,
                });
                continue;
            }

            let kind = match ch {
                '(' => TokenKind::LParen,
                ')' => TokenKind::RParen,
                '{' => TokenKind::LBrace,
                '}' => TokenKind::RBrace,
                '+' => TokenKind::Plus,
                '-' => TokenKind::Minus,
                '*' => TokenKind::Star,
                '/' => TokenKind::Slash,
                ';' => TokenKind::Semicolon,
                _ => {
                    return Err(CompileError::new(
                        line,
                        col,
                        format!("unexpected character `{ch}`"),
                    ))
                }
            };

            self.bump();
            tokens.push(Token {
                kind,
                lexeme: ch.to_string(),
                line,
                col,
            });
        }

        tokens.push(Token {
            kind: TokenKind::Eof,
            lexeme: String::new(),
            line: self.line,
            col: self.col,
        });

        Ok(tokens)
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn peek_next(&self) -> Option<char> {
        self.chars.get(self.pos + 1).copied()
    }

    fn starts_with(&self, s: &str) -> bool {
        let mut i = self.pos;
        for expected in s.chars() {
            if self.chars.get(i).copied() != Some(expected) {
                return false;
            }
            i += 1;
        }
        true
    }

    fn bump(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.pos += 1;
        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(ch)
    }

    fn consume_while<F: Fn(char) -> bool>(&mut self, pred: F) -> String {
        let mut out = String::new();
        while let Some(ch) = self.peek() {
            if !pred(ch) {
                break;
            }
            out.push(ch);
            self.bump();
        }
        out
    }

    fn skip_line_comment(&mut self) {
        self.bump();
        self.bump();
        while let Some(ch) = self.peek() {
            self.bump();
            if ch == '\n' {
                break;
            }
        }
    }

    fn skip_block_comment(&mut self) -> Result<(), CompileError> {
        let start_line = self.line;
        let start_col = self.col;
        self.bump();
        self.bump();

        while let Some(ch) = self.peek() {
            if ch == '*' && self.peek_next() == Some('/') {
                self.bump();
                self.bump();
                return Ok(());
            }
            self.bump();
        }

        Err(CompileError::new(
            start_line,
            start_col,
            "unterminated block comment",
        ))
    }
}

struct Parser {
    tokens: Vec<Token>,
    i: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, i: 0 }
    }

    fn parse_program(&mut self) -> Result<String, CompileError> {
        self.expect_keyword(TokenKind::KwInt)?;
        self.expect_keyword(TokenKind::KwMain)?;
        self.expect_symbol(TokenKind::LParen)?;
        self.expect_symbol(TokenKind::RParen)?;
        self.expect_symbol(TokenKind::LBrace)?;
        self.expect_keyword(TokenKind::KwReturn)?;

        let asm = self.parse_expr()?;

        self.expect_symbol(TokenKind::Semicolon)?;
        self.expect_symbol(TokenKind::RBrace)?;
        self.expect_eof()?;

        Ok(asm)
    }

    fn parse_expr(&mut self) -> Result<String, CompileError> {
        let mut left = self.parse_term()?;

        while matches!(self.current().kind, TokenKind::Plus | TokenKind::Minus) {
            let op = self.current().kind.clone();
            self.i += 1;
            let right = self.parse_term()?;
            left.push_str(&right);
            match op {
                TokenKind::Plus => {
                    left.push_str(
                        "    pop %rdi\n    pop %rax\n    add %rdi, %rax\n    push %rax\n",
                    );
                }
                TokenKind::Minus => {
                    left.push_str(
                        "    pop %rdi\n    pop %rax\n    sub %rdi, %rax\n    push %rax\n",
                    );
                }
                _ => unreachable!(),
            }
        }

        Ok(left)
    }

    fn parse_term(&mut self) -> Result<String, CompileError> {
        let mut left = self.parse_factor()?;

        while matches!(self.current().kind, TokenKind::Star | TokenKind::Slash) {
            let op = self.current().kind.clone();
            self.i += 1;
            let right = self.parse_factor()?;
            left.push_str(&right);
            match op {
                TokenKind::Star => {
                    left.push_str(
                        "    pop %rdi\n    pop %rax\n    imul %rdi, %rax\n    push %rax\n",
                    );
                }
                TokenKind::Slash => {
                    left.push_str(
                        "    pop %rdi\n    pop %rax\n    cqo\n    idiv %rdi\n    push %rax\n",
                    );
                }
                _ => unreachable!(),
            }
        }

        Ok(left)
    }

    fn parse_factor(&mut self) -> Result<String, CompileError> {
        match self.current().kind.clone() {
            TokenKind::LParen => {
                self.i += 1;
                let code = self.parse_expr()?;
                self.expect_symbol(TokenKind::RParen)?;
                Ok(code)
            }
            TokenKind::Plus => {
                self.i += 1;
                self.parse_factor()
            }
            TokenKind::Minus => {
                self.i += 1;
                let mut code = self.parse_factor()?;
                code.push_str("    pop %rax\n    neg %rax\n    push %rax\n");
                Ok(code)
            }
            TokenKind::Number(v) => {
                self.i += 1;
                Ok(format!("    push ${v}\n"))
            }
            _ => {
                let t = self.current();
                Err(CompileError::new(
                    t.line,
                    t.col,
                    format!("expected number or `(`, got `{}`", t.lexeme),
                ))
            }
        }
    }

    fn current(&self) -> &Token {
        &self.tokens[self.i]
    }

    fn expect_keyword(&mut self, expected: TokenKind) -> Result<(), CompileError> {
        if self.current().kind == expected {
            self.i += 1;
            Ok(())
        } else {
            self.expected_error(expected)
        }
    }

    fn expect_symbol(&mut self, expected: TokenKind) -> Result<(), CompileError> {
        if self.current().kind == expected {
            self.i += 1;
            Ok(())
        } else {
            self.expected_error(expected)
        }
    }

    fn expect_eof(&self) -> Result<(), CompileError> {
        if self.current().kind == TokenKind::Eof {
            Ok(())
        } else {
            let t = self.current();
            Err(CompileError::new(
                t.line,
                t.col,
                format!("unexpected trailing token `{}`", t.lexeme),
            ))
        }
    }

    fn expected_error(&self, expected: TokenKind) -> Result<(), CompileError> {
        let t = self.current();
        Err(CompileError::new(
            t.line,
            t.col,
            format!(
                "expected {}, got `{}`",
                token_kind_name(&expected),
                display_token(t)
            ),
        ))
    }
}

fn token_kind_name(kind: &TokenKind) -> &'static str {
    match kind {
        TokenKind::KwInt => "`int`",
        TokenKind::KwMain => "`main`",
        TokenKind::KwReturn => "`return`",
        TokenKind::Number(_) => "number",
        TokenKind::LParen => "`(`",
        TokenKind::RParen => "`)`",
        TokenKind::LBrace => "`{`",
        TokenKind::RBrace => "`}`",
        TokenKind::Plus => "`+`",
        TokenKind::Minus => "`-`",
        TokenKind::Star => "`*`",
        TokenKind::Slash => "`/`",
        TokenKind::Semicolon => "`;`",
        TokenKind::Eof => "end of file",
    }
}

fn display_token(token: &Token) -> String {
    if token.kind == TokenKind::Eof {
        "<eof>".to_string()
    } else {
        token.lexeme.clone()
    }
}

fn compile_source(source: &str) -> Result<String, CompileError> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize()?;
    let mut parser = Parser::new(tokens);
    let body = parser.parse_program()?;

    Ok(format!(
        ".text\n.globl main\nmain:\n{}    pop %rax\n    ret\n.section .note.GNU-stack,\"\",@progbits\n",
        body
    ))
}

fn parse_args() -> Result<(PathBuf, PathBuf), String> {
    let mut args = env::args().skip(1);
    let input = args
        .next()
        .map(PathBuf::from)
        .ok_or_else(|| "usage: cdc <input.c> [-o <output.s>]".to_string())?;

    let mut output: Option<PathBuf> = None;
    while let Some(arg) = args.next() {
        if arg == "-o" || arg == "--output" {
            let val = args
                .next()
                .ok_or_else(|| "missing path after -o/--output".to_string())?;
            output = Some(PathBuf::from(val));
        } else {
            return Err(format!("unknown argument: {arg}"));
        }
    }

    let out = output.unwrap_or_else(|| input.with_extension("s"));
    Ok((input, out))
}

fn run() -> Result<(), String> {
    let (input, output) = parse_args()?;
    let source = fs::read_to_string(&input)
        .map_err(|e| format!("failed to read {}: {e}", path_display(&input)))?;

    let asm = compile_source(&source).map_err(|e| format!("compile error: {e}"))?;

    fs::write(&output, asm)
        .map_err(|e| format!("failed to write {}: {e}", path_display(&output)))?;
    Ok(())
}

fn path_display(path: &Path) -> String {
    path.to_string_lossy().to_string()
}

fn main() {
    if let Err(err) = run() {
        eprintln!("{err}");
        std::process::exit(1);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexer_skips_comments_and_tracks_location() {
        let src = "int main(){/*x*/return 40+2;//tail\n}";
        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize().expect("tokenize");

        assert!(tokens.iter().any(|t| t.kind == TokenKind::KwReturn));
        let plus = tokens
            .iter()
            .find(|t| t.kind == TokenKind::Plus)
            .expect("plus token");
        assert_eq!(plus.line, 1);
    }

    #[test]
    fn compile_expression() {
        let asm = compile_source("int main(){ return (2+3)*4-5; }").expect("compile");
        assert!(asm.contains("imul %rdi, %rax"));
        assert!(asm.contains("sub %rdi, %rax"));
    }

    #[test]
    fn reject_unknown_identifier() {
        let err = compile_source("int main(){ return foo; }").expect_err("should fail");
        assert!(err.message.contains("unsupported identifier"));
    }
}
