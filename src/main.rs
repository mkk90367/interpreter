use std::collections::HashMap;
use std::io::{self, Write};

type Matrix = Vec<Vec<f64>>;

// =========================
// TOKENS
// =========================

#[derive(Debug, Clone)]
enum Token {
    Identifier(String),
    Number(f64),

    Equals,
    Plus,
    Minus,

    LBracket,
    RBracket,
    Comma,
}

// =========================
// AST
// =========================

#[derive(Debug, Clone)]
enum Expr {
    Number(f64),
    Variable(String),
    Matrix(Vec<Vec<Expr>>),

    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
enum Statement {
    Assignment {
        name: String,
        value: Expr,
    },

    Expression(Expr),
}

// =========================
// LEXER
// =========================

fn tokenize(input: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&ch) = chars.peek() {
        match ch {
            ' ' | '\t' | '\n' | '\r' => {
                chars.next();
            }

            '=' => {
                chars.next();
                tokens.push(Token::Equals);
            }

            '+' => {
                chars.next();
                tokens.push(Token::Plus);
            }

            '-' => {
                chars.next();
                tokens.push(Token::Minus);
            }

            '[' => {
                chars.next();
                tokens.push(Token::LBracket);
            }

            ']' => {
                chars.next();
                tokens.push(Token::RBracket);
            }

            ',' => {
                chars.next();
                tokens.push(Token::Comma);
            }

            '0'..='9' | '.' => {
                let mut number = String::new();

                while let Some(&c) = chars.peek() {
                    if c.is_ascii_digit() || c == '.' {
                        number.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }

                let value = number
                    .parse::<f64>()
                    .map_err(|_| format!("Invalid number: {}", number))?;

                tokens.push(Token::Number(value));
            }

            'a'..='z' | 'A'..='Z' | '_' => {
                let mut identifier = String::new();

                while let Some(&c) = chars.peek() {
                    if c.is_ascii_alphanumeric() || c == '_' {
                        identifier.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }

                tokens.push(Token::Identifier(identifier));
            }

            _ => {
                return Err(format!("Unexpected character: '{}'", ch));
            }
        }
    }

    Ok(tokens)
}

// =========================
// PARSER
// =========================

struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    fn current(&self) -> Option<&Token> {
        self.tokens.get(self.position)
    }

    fn advance(&mut self) {
        self.position += 1;
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        if let Some(Token::Identifier(name)) = self.current() {
            if matches!(
                self.tokens.get(self.position + 1),
                Some(Token::Equals)
            ) {
                let name = name.clone();

                self.advance();
                self.advance();

                let value = self.parse_expression()?;

                return Ok(Statement::Assignment { name, value });
            }
        }

        let expression = self.parse_expression()?;

        Ok(Statement::Expression(expression))
    }

    fn parse_expression(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_primary()?;

        loop {
            match self.current() {
                Some(Token::Plus) => {
                    self.advance();

                    let right = self.parse_primary()?;

                    left = Expr::Add(
                        Box::new(left),
                        Box::new(right),
                    );
                }

                Some(Token::Minus) => {
                    self.advance();

                    let right = self.parse_primary()?;

                    left = Expr::Sub(
                        Box::new(left),
                        Box::new(right),
                    );
                }

                _ => break,
            }
        }

        Ok(left)
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        match self.current() {
            Some(Token::Number(value)) => {
                let value = *value;
                self.advance();

                Ok(Expr::Number(value))
            }

            Some(Token::Identifier(name)) => {
                let name = name.clone();
                self.advance();

                Ok(Expr::Variable(name))
            }

            Some(Token::LBracket) => {
                self.parse_matrix()
            }

            _ => {
                Err("Expected number, variable or matrix".to_string())
            }
        }
    }

    fn parse_matrix(&mut self) -> Result<Expr, String> {
        self.advance(); // [

        let mut rows = Vec::new();

        loop {
            match self.current() {
                Some(Token::LBracket) => {
                    self.advance();

                    let mut row = Vec::new();

                    loop {
                        let value = self.parse_expression()?;
                        row.push(value);

                        match self.current() {
                            Some(Token::Comma) => {
                                self.advance();
                            }

                            Some(Token::RBracket) => {
                                self.advance();
                                break;
                            }

                            _ => {
                                return Err(
                                    "Expected ',' or ']' in matrix row"
                                        .to_string()
                                );
                            }
                        }
                    }

                    rows.push(row);

                    match self.current() {
                        Some(Token::Comma) => {
                            self.advance();
                        }

                        Some(Token::RBracket) => {
                            self.advance();
                            break;
                        }

                        _ => {
                            return Err(
                                "Expected ',' or ']' after matrix row"
                                    .to_string()
                            );
                        }
                    }
                }

                _ => {
                    return Err(
                        "Expected '[' at beginning of matrix row"
                            .to_string()
                    );
                }
            }
        }

        Ok(Expr::Matrix(rows))
    }
}

// =========================
// EVALUATOR
// =========================

struct Interpreter {
    variables: HashMap<String, Matrix>,
}

impl Interpreter {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    fn execute(&mut self, statement: Statement) -> Result<Matrix, String> {
        match statement {
            Statement::Assignment { name, value } => {
                let matrix = self.evaluate(value)?;

                self.variables.insert(name, matrix.clone());

                Ok(matrix)
            }

            Statement::Expression(expr) => {
                self.evaluate(expr)
            }
        }
    }

    fn evaluate(&self, expr: Expr) -> Result<Matrix, String> {
        match expr {
            Expr::Number(value) => {
                Ok(vec![vec![value]])
            }

            Expr::Variable(name) => {
                self.variables
                    .get(&name)
                    .cloned()
                    .ok_or_else(|| {
                        format!("Undefined variable: {}", name)
                    })
            }

            Expr::Matrix(rows) => {
                let mut matrix = Vec::new();

                for row in rows {
                    let mut values = Vec::new();

                    for expr in row {
                        let value = self.evaluate(expr)?;

                        if value.len() != 1 || value[0].len() != 1 {
                            return Err(
                                "Matrix elements must be scalar values"
                                    .to_string()
                            );
                        }

                        values.push(value[0][0]);
                    }

                    matrix.push(values);
                }

                validate_matrix(&matrix)?;

                Ok(matrix)
            }

            Expr::Add(left, right) => {
                let a = self.evaluate(*left)?;
                let b = self.evaluate(*right)?;

                add_matrices(&a, &b)
            }

            Expr::Sub(left, right) => {
                let a = self.evaluate(*left)?;
                let b = self.evaluate(*right)?;

                subtract_matrices(&a, &b)
            }
        }
    }
}

// =========================
// MATRIX OPERATIONS
// =========================

fn validate_matrix(matrix: &Matrix) -> Result<(), String> {
    if matrix.is_empty() {
        return Err("Matrix cannot be empty".to_string());
    }

    let columns = matrix[0].len();

    if columns == 0 {
        return Err("Matrix cannot have empty rows".to_string());
    }

    for row in matrix {
        if row.len() != columns {
            return Err("Matrix rows must have the same length".to_string());
        }
    }

    Ok(())
}

fn add_matrices(a: &Matrix, b: &Matrix) -> Result<Matrix, String> {
    if a.len() != b.len() || a[0].len() != b[0].len() {
        return Err(
            "Matrices must have the same dimensions".to_string()
        );
    }

    let mut result = Vec::new();

    for i in 0..a.len() {
        let mut row = Vec::new();

        for j in 0..a[i].len() {
            row.push(a[i][j] + b[i][j]);
        }

        result.push(row);
    }

    Ok(result)
}

fn subtract_matrices(a: &Matrix, b: &Matrix) -> Result<Matrix, String> {
    if a.len() != b.len() || a[0].len() != b[0].len() {
        return Err(
            "Matrices must have the same dimensions".to_string()
        );
    }

    let mut result = Vec::new();

    for i in 0..a.len() {
        let mut row = Vec::new();

        for j in 0..a[i].len() {
            row.push(a[i][j] - b[i][j]);
        }

        result.push(row);
    }

    Ok(result)
}

// =========================
// PRINT MATRIX
// =========================

fn print_matrix(matrix: &Matrix) {
    for row in matrix {
        print!("[ ");

        for value in row {
            print!("{:8.3} ", value);
        }

        println!("]");
    }
}

// =========================
// MAIN
// =========================

fn main() {
    println!("MatLang interpreter v0.1");
    println!("Type 'help' for help.");

    let mut interpreter = Interpreter::new();

    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();

        io::stdin()
            .read_line(&mut input)
            .unwrap();

        let input = input.trim();

        match input {
            "help" => {
                println!("Available commands:");
                println!("  help");
                println!("  exit");
                println!();
                println!("Examples:");
                println!("  A = [[1, 2], [3, 4]]");
                println!("  B = [[5, 6], [7, 8]]");
                println!("  A + B");
                println!("  A - B");
            }

            "exit" => {
                println!("Goodbye!");
                break;
            }

            "" => {}

            _ => {
                match tokenize(input) {
                    Ok(tokens) => {
                        let mut parser = Parser::new(tokens);

                        match parser.parse_statement() {
                            Ok(statement) => {
                                match interpreter.execute(statement) {
                                    Ok(matrix) => {
                                        print_matrix(&matrix);
                                    }

                                    Err(error) => {
                                        println!("Error: {}", error);
                                    }
                                }
                            }

                            Err(error) => {
                                println!("Parser error: {}", error);
                            }
                        }
                    }

                    Err(error) => {
                        println!("Lexer error: {}", error);
                    }
                }
            }
        }
    }
}