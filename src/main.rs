use std::collections::HashMap;
use std::env;
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, PartialEq, Eq)]
enum TokenKind {
    KwInt,
    KwReturn,
    KwIf,
    KwElse,
    KwWhile,
    Ident(String),
    Number(i64),
    LParen,
    RParen,
    LBrace,
    RBrace,
    Semicolon,
    Comma,
    Plus,
    Minus,
    Star,
    Slash,
    Assign,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
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

struct Lexer {
    chars: Vec<char>,
    pos: usize,
    line: usize,
    col: usize,
}

impl Lexer {
    fn new(src: &str) -> Self {
        Self {
            chars: src.chars().collect(),
            pos: 0,
            line: 1,
            col: 1,
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
                    "return" => TokenKind::KwReturn,
                    "if" => TokenKind::KwIf,
                    "else" => TokenKind::KwElse,
                    "while" => TokenKind::KwWhile,
                    _ => TokenKind::Ident(ident.clone()),
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

            let (kind, width) = if self.starts_with("==") {
                (TokenKind::Eq, 2)
            } else if self.starts_with("!=") {
                (TokenKind::Ne, 2)
            } else if self.starts_with("<=") {
                (TokenKind::Le, 2)
            } else if self.starts_with(">=") {
                (TokenKind::Ge, 2)
            } else {
                match ch {
                    '(' => (TokenKind::LParen, 1),
                    ')' => (TokenKind::RParen, 1),
                    '{' => (TokenKind::LBrace, 1),
                    '}' => (TokenKind::RBrace, 1),
                    ';' => (TokenKind::Semicolon, 1),
                    ',' => (TokenKind::Comma, 1),
                    '+' => (TokenKind::Plus, 1),
                    '-' => (TokenKind::Minus, 1),
                    '*' => (TokenKind::Star, 1),
                    '/' => (TokenKind::Slash, 1),
                    '=' => (TokenKind::Assign, 1),
                    '<' => (TokenKind::Lt, 1),
                    '>' => (TokenKind::Gt, 1),
                    _ => {
                        return Err(CompileError::new(
                            line,
                            col,
                            format!("unexpected character `{ch}`"),
                        ));
                    }
                }
            };

            for _ in 0..width {
                self.bump();
            }

            let lexeme = self.chars[self.pos - width..self.pos]
                .iter()
                .collect::<String>();

            tokens.push(Token {
                kind,
                lexeme,
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
        s.chars()
            .enumerate()
            .all(|(i, c)| self.chars.get(self.pos + i).copied() == Some(c))
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
        let (line, col) = (self.line, self.col);
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
        Err(CompileError::new(line, col, "unterminated block comment"))
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Number(i64),
    Var(String),
    Assign(String, Box<Expr>),
    UnaryNeg(Box<Expr>),
    UnaryPlus(Box<Expr>),
    Binary(BinOp, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, Copy)]
enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone)]
enum Stmt {
    Return(Expr),
    Expr(Expr),
    Decl(String, Option<Expr>),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
}

struct Parser {
    tokens: Vec<Token>,
    i: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, i: 0 }
    }

    fn parse_program(&mut self) -> Result<Stmt, CompileError> {
        self.expect(TokenKind::KwInt)?;
        self.expect_ident("main")?;
        self.expect(TokenKind::LParen)?;
        self.expect(TokenKind::RParen)?;
        let block = self.parse_block()?;
        self.expect(TokenKind::Eof)?;
        Ok(block)
    }

    fn parse_block(&mut self) -> Result<Stmt, CompileError> {
        self.expect(TokenKind::LBrace)?;
        let mut stmts = Vec::new();
        while !self.check(&TokenKind::RBrace) {
            stmts.push(self.parse_stmt()?);
        }
        self.expect(TokenKind::RBrace)?;
        Ok(Stmt::Block(stmts))
    }

    fn parse_stmt(&mut self) -> Result<Stmt, CompileError> {
        if self.check(&TokenKind::KwReturn) {
            self.advance();
            let expr = self.parse_expr()?;
            self.expect(TokenKind::Semicolon)?;
            return Ok(Stmt::Return(expr));
        }

        if self.check(&TokenKind::KwInt) {
            self.advance();
            let name = self.expect_any_ident()?;
            let init = if self.check(&TokenKind::Assign) {
                self.advance();
                Some(self.parse_expr()?)
            } else {
                None
            };
            self.expect(TokenKind::Semicolon)?;
            return Ok(Stmt::Decl(name, init));
        }

        if self.check(&TokenKind::KwIf) {
            self.advance();
            self.expect(TokenKind::LParen)?;
            let cond = self.parse_expr()?;
            self.expect(TokenKind::RParen)?;
            let then_stmt = Box::new(self.parse_stmt()?);
            let else_stmt = if self.check(&TokenKind::KwElse) {
                self.advance();
                Some(Box::new(self.parse_stmt()?))
            } else {
                None
            };
            return Ok(Stmt::If(cond, then_stmt, else_stmt));
        }

        if self.check(&TokenKind::KwWhile) {
            self.advance();
            self.expect(TokenKind::LParen)?;
            let cond = self.parse_expr()?;
            self.expect(TokenKind::RParen)?;
            let body = Box::new(self.parse_stmt()?);
            return Ok(Stmt::While(cond, body));
        }

        if self.check(&TokenKind::LBrace) {
            return self.parse_block();
        }

        let expr = self.parse_expr()?;
        self.expect(TokenKind::Semicolon)?;
        Ok(Stmt::Expr(expr))
    }

    fn parse_expr(&mut self) -> Result<Expr, CompileError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expr, CompileError> {
        let left = self.parse_equality()?;
        if self.check(&TokenKind::Assign) {
            self.advance();
            let right = self.parse_assignment()?;
            if let Expr::Var(name) = left {
                return Ok(Expr::Assign(name, Box::new(right)));
            }
            let t = self.current();
            return Err(CompileError::new(
                t.line,
                t.col,
                "left side of assignment must be variable",
            ));
        }
        Ok(left)
    }

    fn parse_equality(&mut self) -> Result<Expr, CompileError> {
        let mut left = self.parse_relational()?;
        while self.check(&TokenKind::Eq) || self.check(&TokenKind::Ne) {
            let op = if self.check(&TokenKind::Eq) {
                BinOp::Eq
            } else {
                BinOp::Ne
            };
            self.advance();
            let right = self.parse_relational()?;
            left = Expr::Binary(op, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_relational(&mut self) -> Result<Expr, CompileError> {
        let mut left = self.parse_additive()?;
        while self.check(&TokenKind::Lt)
            || self.check(&TokenKind::Le)
            || self.check(&TokenKind::Gt)
            || self.check(&TokenKind::Ge)
        {
            let op = match self.current().kind {
                TokenKind::Lt => BinOp::Lt,
                TokenKind::Le => BinOp::Le,
                TokenKind::Gt => BinOp::Gt,
                TokenKind::Ge => BinOp::Ge,
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_additive()?;
            left = Expr::Binary(op, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_additive(&mut self) -> Result<Expr, CompileError> {
        let mut left = self.parse_term()?;
        while self.check(&TokenKind::Plus) || self.check(&TokenKind::Minus) {
            let op = if self.check(&TokenKind::Plus) {
                BinOp::Add
            } else {
                BinOp::Sub
            };
            self.advance();
            let right = self.parse_term()?;
            left = Expr::Binary(op, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_term(&mut self) -> Result<Expr, CompileError> {
        let mut left = self.parse_unary()?;
        while self.check(&TokenKind::Star) || self.check(&TokenKind::Slash) {
            let op = if self.check(&TokenKind::Star) {
                BinOp::Mul
            } else {
                BinOp::Div
            };
            self.advance();
            let right = self.parse_unary()?;
            left = Expr::Binary(op, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expr, CompileError> {
        if self.check(&TokenKind::Minus) {
            self.advance();
            return Ok(Expr::UnaryNeg(Box::new(self.parse_unary()?)));
        }
        if self.check(&TokenKind::Plus) {
            self.advance();
            return Ok(Expr::UnaryPlus(Box::new(self.parse_unary()?)));
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Expr, CompileError> {
        match &self.current().kind {
            TokenKind::Number(v) => {
                let out = Expr::Number(*v);
                self.advance();
                Ok(out)
            }
            TokenKind::Ident(name) => {
                let out = Expr::Var(name.clone());
                self.advance();
                Ok(out)
            }
            TokenKind::LParen => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(TokenKind::RParen)?;
                Ok(expr)
            }
            _ => {
                let t = self.current();
                Err(CompileError::new(
                    t.line,
                    t.col,
                    format!("expected expression, got `{}`", t.lexeme),
                ))
            }
        }
    }

    fn check(&self, kind: &TokenKind) -> bool {
        std::mem::discriminant(&self.current().kind) == std::mem::discriminant(kind)
    }

    fn advance(&mut self) {
        self.i += 1;
    }

    fn current(&self) -> &Token {
        &self.tokens[self.i]
    }

    fn expect(&mut self, expected: TokenKind) -> Result<(), CompileError> {
        if self.check(&expected) {
            self.advance();
            Ok(())
        } else {
            let t = self.current();
            Err(CompileError::new(
                t.line,
                t.col,
                format!("expected {}, got `{}`", token_name(&expected), t.lexeme),
            ))
        }
    }

    fn expect_ident(&mut self, expected: &str) -> Result<(), CompileError> {
        match &self.current().kind {
            TokenKind::Ident(s) if s == expected => {
                self.advance();
                Ok(())
            }
            _ => {
                let t = self.current();
                Err(CompileError::new(
                    t.line,
                    t.col,
                    format!("expected identifier `{expected}`, got `{}`", t.lexeme),
                ))
            }
        }
    }

    fn expect_any_ident(&mut self) -> Result<String, CompileError> {
        match &self.current().kind {
            TokenKind::Ident(s) => {
                let out = s.clone();
                self.advance();
                Ok(out)
            }
            _ => {
                let t = self.current();
                Err(CompileError::new(t.line, t.col, "expected identifier"))
            }
        }
    }
}

fn token_name(kind: &TokenKind) -> &'static str {
    match kind {
        TokenKind::KwInt => "`int`",
        TokenKind::KwReturn => "`return`",
        TokenKind::KwIf => "`if`",
        TokenKind::KwElse => "`else`",
        TokenKind::KwWhile => "`while`",
        TokenKind::Ident(_) => "identifier",
        TokenKind::Number(_) => "number",
        TokenKind::LParen => "`(`",
        TokenKind::RParen => "`)`",
        TokenKind::LBrace => "`{`",
        TokenKind::RBrace => "`}`",
        TokenKind::Semicolon => "`;`",
        TokenKind::Comma => "`,`",
        TokenKind::Plus => "`+`",
        TokenKind::Minus => "`-`",
        TokenKind::Star => "`*`",
        TokenKind::Slash => "`/`",
        TokenKind::Assign => "`=`",
        TokenKind::Eq => "`==`",
        TokenKind::Ne => "`!=`",
        TokenKind::Lt => "`<`",
        TokenKind::Le => "`<=`",
        TokenKind::Gt => "`>`",
        TokenKind::Ge => "`>=`",
        TokenKind::Eof => "end of file",
    }
}

struct Codegen {
    asm: String,
    vars: HashMap<String, i64>,
    stack_size: i64,
    next_label: usize,
}

impl Codegen {
    fn new() -> Self {
        Self {
            asm: String::new(),
            vars: HashMap::new(),
            stack_size: 0,
            next_label: 0,
        }
    }

    fn emit(&mut self, s: &str) {
        self.asm.push_str(s);
    }

    fn fresh_label(&mut self, prefix: &str) -> String {
        let label = format!(".L{}_{}", prefix, self.next_label);
        self.next_label += 1;
        label
    }

    fn compile_program(mut self, prog: Stmt) -> Result<String, CompileError> {
        self.emit(".text\n.globl main\nmain:\n    push %rbp\n    mov %rsp, %rbp\n");
        self.emit("    sub $4096, %rsp\n");
        self.stack_size = 4096;
        self.compile_stmt(&prog)?;
        self.emit("    mov $0, %rax\n    leave\n    ret\n");
        self.emit(".section .note.GNU-stack,\"\",@progbits\n");
        Ok(self.asm)
    }

    fn ensure_var(&mut self, name: &str) -> i64 {
        if let Some(off) = self.vars.get(name) {
            *off
        } else {
            let idx = self.vars.len() as i64 + 1;
            let off = idx * 8;
            self.vars.insert(name.to_string(), off);
            off
        }
    }

    fn lookup_var(&self, name: &str) -> Option<i64> {
        self.vars.get(name).copied()
    }

    fn compile_stmt(&mut self, stmt: &Stmt) -> Result<(), CompileError> {
        match stmt {
            Stmt::Return(expr) => {
                self.compile_expr(expr)?;
                self.emit("    pop %rax\n    leave\n    ret\n");
            }
            Stmt::Expr(expr) => {
                self.compile_expr(expr)?;
                self.emit("    add $8, %rsp\n");
            }
            Stmt::Decl(name, init) => {
                let off = self.ensure_var(name);
                if off > self.stack_size {
                    return Err(CompileError::new(1, 1, "too many local variables"));
                }
                if let Some(expr) = init {
                    self.compile_expr(expr)?;
                    self.emit("    pop %rax\n");
                    self.emit(&format!("    mov %rax, -{}(%rbp)\n", off));
                }
            }
            Stmt::Block(stmts) => {
                for s in stmts {
                    self.compile_stmt(s)?;
                }
            }
            Stmt::If(cond, then_stmt, else_stmt) => {
                let else_l = self.fresh_label("else");
                let end_l = self.fresh_label("endif");
                self.compile_expr(cond)?;
                self.emit("    pop %rax\n    cmp $0, %rax\n");
                self.emit(&format!("    je {}\n", else_l));
                self.compile_stmt(then_stmt)?;
                self.emit(&format!("    jmp {}\n{}:\n", end_l, else_l));
                if let Some(s) = else_stmt {
                    self.compile_stmt(s)?;
                }
                self.emit(&format!("{}:\n", end_l));
            }
            Stmt::While(cond, body) => {
                let start = self.fresh_label("while_start");
                let end = self.fresh_label("while_end");
                self.emit(&format!("{}:\n", start));
                self.compile_expr(cond)?;
                self.emit("    pop %rax\n    cmp $0, %rax\n");
                self.emit(&format!("    je {}\n", end));
                self.compile_stmt(body)?;
                self.emit(&format!("    jmp {}\n{}:\n", start, end));
            }
        }
        Ok(())
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<(), CompileError> {
        match expr {
            Expr::Number(v) => self.emit(&format!("    push ${}\n", v)),
            Expr::UnaryPlus(e) => self.compile_expr(e)?,
            Expr::UnaryNeg(e) => {
                self.compile_expr(e)?;
                self.emit("    pop %rax\n    neg %rax\n    push %rax\n");
            }
            Expr::Var(name) => {
                let off = self.lookup_var(name).ok_or_else(|| {
                    CompileError::new(1, 1, format!("undefined variable `{name}`"))
                })?;
                self.emit(&format!("    mov -{}(%rbp), %rax\n    push %rax\n", off));
            }
            Expr::Assign(name, e) => {
                self.compile_expr(e)?;
                let off = self.lookup_var(name).ok_or_else(|| {
                    CompileError::new(1, 1, format!("undefined variable `{name}`"))
                })?;
                self.emit("    pop %rax\n");
                self.emit(&format!("    mov %rax, -{}(%rbp)\n    push %rax\n", off));
            }
            Expr::Binary(op, l, r) => {
                self.compile_expr(l)?;
                self.compile_expr(r)?;
                self.emit("    pop %rdi\n    pop %rax\n");
                match op {
                    BinOp::Add => self.emit("    add %rdi, %rax\n"),
                    BinOp::Sub => self.emit("    sub %rdi, %rax\n"),
                    BinOp::Mul => self.emit("    imul %rdi, %rax\n"),
                    BinOp::Div => self.emit("    cqo\n    idiv %rdi\n"),
                    BinOp::Eq => {
                        self.emit("    cmp %rdi, %rax\n    sete %al\n    movzb %al, %rax\n")
                    }
                    BinOp::Ne => {
                        self.emit("    cmp %rdi, %rax\n    setne %al\n    movzb %al, %rax\n")
                    }
                    BinOp::Lt => {
                        self.emit("    cmp %rdi, %rax\n    setl %al\n    movzb %al, %rax\n")
                    }
                    BinOp::Le => {
                        self.emit("    cmp %rdi, %rax\n    setle %al\n    movzb %al, %rax\n")
                    }
                    BinOp::Gt => {
                        self.emit("    cmp %rdi, %rax\n    setg %al\n    movzb %al, %rax\n")
                    }
                    BinOp::Ge => {
                        self.emit("    cmp %rdi, %rax\n    setge %al\n    movzb %al, %rax\n")
                    }
                }
                self.emit("    push %rax\n");
            }
        }
        Ok(())
    }
}

fn compile_source(source: &str) -> Result<String, CompileError> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize()?;
    let mut parser = Parser::new(tokens);
    let prog = parser.parse_program()?;
    Codegen::new().compile_program(prog)
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
    Ok((
        input.clone(),
        output.unwrap_or_else(|| input.with_extension("s")),
    ))
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
    fn lexer_comments_and_ops() {
        let mut lex = Lexer::new("int main(){/*x*/int a=1; // y\n return a==1;}");
        let t = lex.tokenize().expect("tokenize");
        assert!(t.iter().any(|x| matches!(x.kind, TokenKind::Eq)));
    }

    #[test]
    fn compile_control_flow_and_vars() {
        let src = r#"
            int main() {
                int i = 0;
                int s = 0;
                while (i < 5) {
                    s = s + i;
                    i = i + 1;
                }
                if (s == 10) {
                    return 42;
                } else {
                    return 1;
                }
            }
        "#;
        let asm = compile_source(src).expect("compile");
        assert!(asm.contains("while_start"));
        assert!(asm.contains("sete %al"));
    }

    #[test]
    fn undefined_var_rejected() {
        let err = compile_source("int main(){ return x; }").expect_err("must fail");
        assert!(err.message.contains("undefined variable"));
    }
}
