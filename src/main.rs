use eframe::egui;
use std::collections::HashMap;
use std::fs;
use std::process::Command;
use std::time::Instant;

// ==================== TOKENS ====================
#[derive(Debug, PartialEq, Clone)]
enum Token {
    Let,
    Identifier(String),
    Number(f64),
    Assign,
    Plus,
    Minus,
    Multiply,
    Divide,
    LParen,
    RParen,
    Semicolon,
    Eof,
}

// ==================== AST NODES ====================
#[derive(Debug, Clone)]
enum Expr {
    Number(f64),
    Identifier(String),
    Binary(Box<Expr>, Token, Box<Expr>),
}

#[derive(Debug, Clone)]
enum Stmt {
    Let(String, Expr),
    ExprStmt(Expr),
}

// ==================== LEXER ====================
struct Lexer {
    input: Vec<char>,
    position: usize,
}

impl Lexer {
    fn new(input: &str) -> Self {
        Lexer {
            input: input.chars().collect(),
            position: 0,
        }
    }

    fn peek(&self) -> char {
        if self.position >= self.input.len() { '\0' } else { self.input[self.position] }
    }

    fn advance(&mut self) -> char {
        let ch = self.peek();
        self.position += 1;
        ch
    }

    fn next_token(&mut self) -> Token {
        while self.peek().is_whitespace() {
            self.advance();
        }
        let ch = self.peek();
        if ch == '\0' { return Token::Eof; }

        if ch.is_ascii_digit() {
            let mut num_str = String::new();
            while self.peek().is_ascii_digit() || self.peek() == '.' {
                num_str.push(self.advance());
            }
            return Token::Number(num_str.parse().unwrap());
        }

        if ch.is_ascii_alphabetic() || ch == '_' {
            let mut ident = String::new();
            while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
                ident.push(self.advance());
            }
            return match ident.as_str() {
                "let" => Token::Let,
                _ => Token::Identifier(ident),
            };
        }

        self.advance();
        match ch {
            '=' => Token::Assign,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Multiply,
            '/' => Token::Divide,
            '(' => Token::LParen,
            ')' => Token::RParen,
            ';' => Token::Semicolon,
            other => panic!("Unexpected character: {}", other),
        }
    }
}

// ==================== PARSER ====================
struct Parser {
    lexer: Lexer,
    current_token: Token,
}

impl Parser {
    fn new(lexer: Lexer) -> Self {
        let mut parser = Parser { lexer, current_token: Token::Eof };
        parser.current_token = parser.lexer.next_token();
        parser
    }

    fn eat(&mut self, expected: Token) {
        if std::mem::discriminant(&self.current_token) == std::mem::discriminant(&expected) {
            self.current_token = self.lexer.next_token();
        } else {
            panic!("Expected token {:?}, got {:?}", expected, self.current_token);
        }
    }

    fn parse_primary(&mut self) -> Expr {
        match self.current_token.clone() {
            Token::Number(n) => { self.eat(Token::Number(0.0)); Expr::Number(n) }
            Token::Identifier(name) => { self.eat(Token::Identifier("".to_string())); Expr::Identifier(name) }
            Token::LParen => {
                self.eat(Token::LParen);
                let expr = self.parse_expression();
                self.eat(Token::RParen);
                expr
            }
            other => panic!("Unexpected token: {:?}", other),
        }
    }

    fn parse_factor(&mut self) -> Expr {
        let mut left = self.parse_primary();
        while self.current_token == Token::Multiply || self.current_token == Token::Divide {
            let op = self.current_token.clone();
            self.eat(op.clone());
            let right = self.parse_primary();
            left = Expr::Binary(Box::new(left), op, Box::new(right));
        }
        left
    }

    fn parse_expression(&mut self) -> Expr {
        let mut left = self.parse_factor();
        while self.current_token == Token::Plus || self.current_token == Token::Minus {
            let op = self.current_token.clone();
            self.eat(op.clone());
            let right = self.parse_factor();
            left = Expr::Binary(Box::new(left), op, Box::new(right));
        }
        left
    }

    fn parse_statement(&mut self) -> Stmt {
        if self.current_token == Token::Let {
            self.eat(Token::Let);
            let name = match self.current_token.clone() {
                Token::Identifier(n) => { self.eat(Token::Identifier("".to_string())); n }
                other => panic!("Expected identifier, got {:?}", other),
            };
            self.eat(Token::Assign);
            let expr = self.parse_expression();
            self.eat(Token::Semicolon);
            Stmt::Let(name, expr)
        } else {
            let expr = self.parse_expression();
            self.eat(Token::Semicolon);
            Stmt::ExprStmt(expr)
        }
    }

    fn parse_program(&mut self) -> Vec<Stmt> {
        let mut statements = Vec::new();
        while self.current_token != Token::Eof {
            statements.push(self.parse_statement());
        }
        statements
    }
}

// ==================== EVALUATOR ====================
struct Evaluator {
    env: HashMap<String, f64>,
}

impl Evaluator {
    fn new() -> Self { Evaluator { env: HashMap::new() } }

    fn eval_expr(&self, expr: &Expr) -> f64 {
        match expr {
            Expr::Number(n) => *n,
            Expr::Identifier(name) => *self.env.get(name).unwrap_or_else(|| panic!("Undefined variable: '{}'", name)),
            Expr::Binary(left, op, right) => {
                let l = self.eval_expr(left);
                let r = self.eval_expr(right);
                match op {
                    Token::Plus => l + r,
                    Token::Minus => l - r,
                    Token::Multiply => l * r,
                    Token::Divide => { if r == 0.0 { panic!("Division by zero!"); } l / r }
                    _ => panic!("Unsupported operator"),
                }
            }
        }
    }

    fn eval_program(&mut self, program: &[Stmt]) -> (String, HashMap<String, f64>) {
        let mut output = String::new();
        for stmt in program {
            match stmt {
                Stmt::Let(name, expr) => {
                    let val = self.eval_expr(expr);
                    output.push_str(&format!("let {} = {}\n", name, val));
                    self.env.insert(name.clone(), val);
                }
                Stmt::ExprStmt(expr) => {
                    let val = self.eval_expr(expr);
                    output.push_str(&format!("=> {}\n", val));
                }
            }
        }
        (output, self.env.clone())
    }
}

// ==================== MULTI-LANGUAGE IDE STATE ====================
#[derive(PartialEq, Clone, Debug)]
enum LanguageMode {
    RustXCompiler,
    Python,
    JavaScript,
}

struct MultiLangIdeApp {
    code_input: String,
    terminal_output: String,
    memory_registry: HashMap<String, f64>,
    selected_lang: LanguageMode,
    is_dark_mode: bool,
    show_explorer: bool,
    show_debugger: bool,
    status_text: String,
    execution_time_ms: u128,
}

impl Default for MultiLangIdeApp {
    fn default() -> Self {
        Self {
            code_input: String::from("let x = 10 + 5 * 2;\nlet y = (x - 5) / 5;"),
            terminal_output: String::from("Ready. Select language and click '▶ Run' or press Ctrl+Enter."),
            memory_registry: HashMap::new(),
            selected_lang: LanguageMode::RustXCompiler,
            is_dark_mode: true,
            show_explorer: true,
            show_debugger: true,
            status_text: String::from("Ready | Multi-Lang Mode"),
            execution_time_ms: 0,
        }
    }
}

impl MultiLangIdeApp {
    fn run_code(&mut self) {
        let start_time = Instant::now();

        match self.selected_lang {
            LanguageMode::RustXCompiler => {
                let lexer = Lexer::new(&self.code_input);
                let mut parser = Parser::new(lexer);

                let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    let program = parser.parse_program();
                    let mut evaluator = Evaluator::new();
                    evaluator.eval_program(&program)
                }));

                self.execution_time_ms = start_time.elapsed().as_micros();

                match result {
                    Ok((output, memory)) => {
                        self.terminal_output = output;
                        self.memory_registry = memory;
                        self.status_text = format!("RustX Build Success ({} µs) ✓", self.execution_time_ms);
                    }
                    Err(_) => {
                        self.terminal_output = String::from("❌ RustX Compilation Error.");
                        self.status_text = String::from("Build Failed ✗");
                    }
                }
            }
            LanguageMode::Python => {
                let file_path = "temp_script.py";
                let _ = fs::write(file_path, &self.code_input);

                let output = Command::new("python")
                    .arg(file_path)
                    .output();

                self.execution_time_ms = start_time.elapsed().as_millis();

                match output {
                    Ok(res) => {
                        let stdout = String::from_utf8_lossy(&res.stdout);
                        let stderr = String::from_utf8_lossy(&res.stderr);
                        if res.status.success() {
                            self.terminal_output = stdout.to_string();
                            self.status_text = format!("Python Executed ({} ms) ✓", self.execution_time_ms);
                        } else {
                            self.terminal_output = format!("Python Error:\n{}", stderr);
                            self.status_text = String::from("Python Execution Failed ✗");
                        }
                    }
                    Err(e) => {
                        self.terminal_output = format!("Failed to launch Python interpreter.\nError: {}", e);
                        self.status_text = String::from("Python Error ✗");
                    }
                }
                let _ = fs::remove_file(file_path);
            }
            LanguageMode::JavaScript => {
                let file_path = "temp_script.js";
                let _ = fs::write(file_path, &self.code_input);

                let output = Command::new("node")
                    .arg(file_path)
                    .output();

                self.execution_time_ms = start_time.elapsed().as_millis();

                match output {
                    Ok(res) => {
                        let stdout = String::from_utf8_lossy(&res.stdout);
                        let stderr = String::from_utf8_lossy(&res.stderr);
                        if res.status.success() {
                            self.terminal_output = stdout.to_string();
                            self.status_text = format!("Node.js Executed ({} ms) ✓", self.execution_time_ms);
                        } else {
                            self.terminal_output = format!("Node.js Error:\n{}", stderr);
                            self.status_text = String::from("Node.js Execution Failed ✗");
                        }
                    }
                    Err(e) => {
                        self.terminal_output = format!("Failed to launch Node.js.\nError: {}", e);
                        self.status_text = String::from("Node.js Error ✗");
                    }
                }
                let _ = fs::remove_file(file_path);
            }
        }
    }
}

impl eframe::App for MultiLangIdeApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        if self.is_dark_mode {
            ctx.set_visuals(egui::Visuals::dark());
        } else {
            ctx.set_visuals(egui::Visuals::light());
        }

        if ctx.input(|i| i.modifiers.ctrl && i.key_pressed(egui::Key::Enter)) {
            self.run_code();
        }

        // 1. LEFT ACTIVITY BAR
        egui::SidePanel::left("activity_bar")
            .exact_width(48.0)
            .show(ctx, |ui| {
                ui.add_space(8.0);
                ui.vertical_centered(|ui| {
                    if ui.button("📁").on_hover_text("Explorer").clicked() { self.show_explorer = !self.show_explorer; }
                    ui.add_space(8.0);
                    if ui.button("🐛").on_hover_text("Variables").clicked() { self.show_debugger = !self.show_debugger; }
                    ui.add_space(8.0);
                    if ui.button("⚙️").on_hover_text("Theme Toggle").clicked() { self.is_dark_mode = !self.is_dark_mode; }
                });
            });

        // 2. EXPLORER SIDEBAR
        if self.show_explorer {
            egui::SidePanel::left("explorer_sidebar")
                .min_width(170.0)
                .max_width(220.0)
                .show(ctx, |ui| {
                    ui.add_space(6.0);
                    ui.heading("EXPLORER");
                    ui.separator();
                    ui.collapsing("📁 multi_lang_studio", |ui| {
                        if ui.selectable_label(self.selected_lang == LanguageMode::RustXCompiler, "📄 main.rustx").clicked() {
                            self.selected_lang = LanguageMode::RustXCompiler;
                            self.code_input = String::from("let x = 10 + 5 * 2;\nlet y = (x - 5) / 5;");
                        }
                        if ui.selectable_label(self.selected_lang == LanguageMode::Python, "📄 script.py").clicked() {
                            self.selected_lang = LanguageMode::Python;
                            self.code_input = String::from("print('Hello from Python!')\nfor i in range(3):\n    print(f'Count: {i}')");
                        }
                        if ui.selectable_label(self.selected_lang == LanguageMode::JavaScript, "📄 script.js").clicked() {
                            self.selected_lang = LanguageMode::JavaScript;
                            self.code_input = String::from("console.log('Hello from Node.js JavaScript!');\nconst total = 50 + 50;\nconsole.log('Total:', total);");
                        }
                    });
                });
        }

        // 3. VARIABLE INSPECTOR
        if self.show_debugger {
            egui::SidePanel::right("debugger_panel")
                .min_width(180.0)
                .max_width(240.0)
                .show(ctx, |ui| {
                    ui.add_space(6.0);
                    ui.heading("🔍 Variables");
                    ui.separator();
                    if self.selected_lang != LanguageMode::RustXCompiler {
                        ui.label("Variable inspector active\nfor RustX Custom Compiler.");
                    } else if self.memory_registry.is_empty() {
                        ui.label("No active variables.");
                    } else {
                        egui::Grid::new("vars_grid").striped(true).show(ui, |ui| {
                            ui.label("Name"); ui.label("Value"); ui.end_row();
                            for (k, v) in &self.memory_registry {
                                ui.label(k);
                                ui.label(format!("{:.2}", v));
                                ui.end_row();
                            }
                        });
                    }
                });
        }

        // 4. BOTTOM STATUS BAR
        egui::TopBottomPanel::bottom("status_bar").show(ctx, |ui| {
            ui.horizontal(|ui| {
                ui.label(&self.status_text);
                ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                    if ui.button(if self.is_dark_mode { "☀️ Light" } else { "🌙 Dark" }).clicked() {
                        self.is_dark_mode = !self.is_dark_mode;
                    }
                    if ui.button("▶ Run Code (Ctrl+Enter)").clicked() {
                        self.run_code();
                    }
                    ui.label(format!("Mode: {:?} | Time: {}ms", self.selected_lang, self.execution_time_ms));
                });
            });
        });

        // 5. CENTRAL PANEL
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.horizontal(|ui| {
                ui.label("Language Selector:");
                ui.selectable_value(&mut self.selected_lang, LanguageMode::RustXCompiler, "🛠️ RustX Compiler");
                ui.selectable_value(&mut self.selected_lang, LanguageMode::Python, "🐍 Python");
                ui.selectable_value(&mut self.selected_lang, LanguageMode::JavaScript, "🌐 JavaScript (Node)");
            });
            ui.separator();

            ui.vertical(|ui| {
                ui.label("📝 Source Code Editor");
                ui.add(
                    egui::TextEdit::multiline(&mut self.code_input)
                        .font(egui::TextStyle::Monospace)
                        .code_editor()
                        .desired_width(f32::INFINITY)
                        .desired_rows(16)
                );

                ui.add_space(6.0);
                ui.separator();
                ui.label("💻 Terminal & Execution Output");
                ui.add(
                    egui::TextEdit::multiline(&mut self.terminal_output)
                        .font(egui::TextStyle::Monospace)
                        .desired_width(f32::INFINITY)
                        .desired_rows(10)
                        .interactive(false)
                );
            });
        });
    }
}

// ==================== MAIN LAUNCHER ====================
fn main() -> Result<(), eframe::Error> {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_inner_size([1200.0, 750.0])
            .with_min_inner_size([850.0, 500.0]),
        ..Default::default()
    };
    eframe::run_native(
        "RustX Multi-Language Studio",
        options,
        Box::new(|_cc| Box::new(MultiLangIdeApp::default())),
    )
}