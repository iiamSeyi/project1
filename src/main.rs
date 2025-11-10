//! # lolcompiler
//!
//! **Author:** Oluwaseyi Salisu  
//! **Course:** COSC 455 – Programming Languages: Design and Implementation  
//! **Term:** Fall 2025
//!
//! ## Overview
//! `lolcompiler` is a mini compiler written in Rust that translates a simplified
//! **LOLCODE-Markdown hybrid language** into valid, well-formatted HTML.
//!
//! It performs three major compilation stages:
//! 1. **Lexical Analysis** – Tokenizes directives such as `#HAI`, `#GIMMEH`, `#MKAY`.
//! 2. **Parsing** – Builds an Abstract Syntax Tree (AST) for program structure.
//! 3. **Code Generation** – Outputs styled HTML with proper semantics and formatting.
//!
//! Additional features include variable scoping, syntax and semantic error detection,
//! and automatic browser preview of the compiled HTML file.
//!
//! ## Usage
//! ```text
//! lolcompiler <input.lol>
//! ```
//! Example:
//! ```text
//! > lolcompiler.exe Test3.lol
//! ✅ Compiled to Test3.html
//! 🌐 Opening in Chrome: file:///C:/Users/osalisu1/Desktop/COSC_455/project1/test/Test3.html
//! ```
//!
//! ## Example Source
//! ```text
//! #HAI
//! #MAEK PARAGRAF
//!   Hello #GIMMEH BOLD world #MKAY!
//! #OIC
//! #KTHXBYE
//! ```
//!
//! ## Example Output
//! ```html
//! <!doctype html>
//! <html>
//! <head><title></title></head>
//! <body><p>Hello <strong>world</strong>!</p></body>
//! </html>
//! ```
//!
//! ## Compilation Phases Summary
//! - **Lexer:** Scans `.lol` source → `Vec<Tok>` tokens  
//! - **Parser:** Consumes tokens → `Node` (AST)  
//! - **Emitter:** Generates HTML output and opens it in Chrome  
//!
//! ## Platform Support
//! - Windows 10/11 (x86_64-pc-windows-msvc)  
//! - macOS (arm64)  
//! - Uses Google Chrome for preview  

use std::env;
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::collections::{HashMap, VecDeque};
use std::ffi::OsStr;

//
// ===========================================================
//  LOLCODE MARKDOWN COMPILER
//  Author: Oluwaseyi Salisu
//  Description:
//    This program implements a mini compiler for a LOLCODE-like
//    markdown language that converts annotated syntax (#HAI ...)
//    into valid HTML output.
//
//    The compiler has three main stages:
//      1. Lexical Analysis  → Converts source into tokens.
//      2. Parsing           → Builds an AST (Abstract Syntax Tree).
//      3. Code Generation   → Outputs structured HTML.
//
//    It also includes error checking, variable scoping, and
//    browser preview functionality.
// ===========================================================
//

//
// ================== TRAITS ==================
//

// Compiler trait defines the high-level structure of any compiler.
pub trait Compiler {
    fn compile(&mut self, source: &str);
    fn next_token(&mut self) -> String;
    fn parse(&mut self);
    fn current_token(&self) -> String;
    fn set_current_token(&mut self, tok: String);
}

// LexicalAnalyzer trait defines required operations
// for reading characters and identifying lexemes.
pub trait LexicalAnalyzer {
    fn get_char(&mut self) -> char;
    fn add_char(&mut self, c: char);
    fn lookup(&self, s: &str) -> bool;
}

//
// ================== TOKEN DEFINITIONS ==================
//

/// Enum `Tok` represents all possible token types recognized by the lexer.
#[derive(Debug, Clone, PartialEq)]
enum Tok {
    // Core structure
    HAI, KTHXBYE, OBTW, TLDR, MAEK, OIC, GIMMEH, MKAY,
    // HTML-related structures
    HEAD, TITLE, PARAGRAF, BOLD, ITALICS, LIST, ITEM,
    NEWLINE, SOUNDZ, VIDZ,
    // Variable definitions and IO
    IHAZ, ITIZ, LEMMESEE,
    // Data and values
    VARDEF(String), ADDRESS(String), TEXT(String),
    // End of file marker
    EOF,
}

//
// ================== HELPER FUNCTIONS ==================
//

// Opens generated HTML in a browser (supports Windows, macOS, Linux)
fn open_in_browser(p: &Path) -> io::Result<()> {
    use std::process::Command;

    if !p.exists() {
        eprintln!("⚠️ Output file not found: {}", p.display());
        return Ok(());
    }

    // Canonicalize file path and clean prefix
    let abs = fs::canonicalize(p)?;
    let mut path_str = abs.display().to_string();
    if path_str.starts_with(r"\\?\") {
        path_str = path_str.trim_start_matches(r"\\?\").to_string();
    }

    // Build proper file URL
    let url = if cfg!(windows) {
        format!("file:///{}", path_str.replace('\\', "/"))
    } else {
        format!("file://{}", path_str)
    };

    println!("🌐 Opening in Chrome: {}", url);

    // Platform-specific browser launching
    #[cfg(target_os = "windows")]
    {
        let chrome_paths = [
            r"C:\Program Files\Google\Chrome\Application\chrome.exe",
            r"C:\Program Files (x86)\Google\Chrome\Application\chrome.exe",
        ];
        let mut opened = false;
        for chrome in &chrome_paths {
            if Path::new(chrome).exists() {
                Command::new(chrome)
                    .args(["--new-window", &url])
                    .spawn()?;
                opened = true;
                break;
            }
        }
        if !opened {
            Command::new("cmd").args(["/C", "start", "", &url]).spawn()?;
        }
    }

    #[cfg(target_os = "macos")]
    {
        let status = Command::new("open")
            .args(["-a", "Google Chrome", &url])
            .status();

        if !status.as_ref().map(|s| s.success()).unwrap_or(false) {
            Command::new("open").arg(&url).status()?;
        }
    }

    #[cfg(all(not(target_os = "windows"), not(target_os = "macos")))]
    {
        let _ = Command::new("xdg-open").arg(&url).spawn();
    }

    Ok(())
}

// Fixes spacing issues before punctuation.
fn fix_spacing(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut it = s.chars().peekable();
    while let Some(c) = it.next() {
        if c == ' ' {
            if let Some(&next) = it.peek() {
                if matches!(next, '.' | ',' | '!' | '?' | ':' | ';') {
                    continue; // drop space before punctuation
                }
            }
        }
        out.push(c);
    }
    out
}

fn upper(s: &str) -> String { s.chars().flat_map(|c| c.to_uppercase()).collect() }
fn is_letter(c: char) -> bool { c.is_ascii_alphabetic() }
fn is_address_char(c: char) -> bool { c != ' ' && c != '\n' && c != '\r' && c != '\t' }

///
/// ================== LEXER ==================
///
/// Converts the input source code into a stream of tokens.
/// Handles keywords, variables, directives (#HAI, #GIMMEH, etc.), and text.
///

struct Lexer {
    src: String,
    pos: usize,
    line: usize,
    col: usize,
    lookahead: VecDeque<Tok>,
    expect_keyword: bool,
}

impl Lexer {
    /// Creates a new `Lexer` instance from the provided source string.
    fn new(input: &str) -> Self {
        Self {
            src: input.to_string(),
            pos: 0,
            line: 1,
            col: 1,
            lookahead: VecDeque::new(),
            expect_keyword: false,
        }
    }

    fn peek(&self) -> Option<char> { self.src[self.pos..].chars().next() }

    fn bump(&mut self) -> Option<char> {
        // Advances one character and updates position
        if self.pos >= self.src.len() { return None; }
        let mut iter = self.src[self.pos..].char_indices();
        if let Some((_, c)) = iter.next() {
            if let Some((next, _)) = iter.next() {
                self.pos += next;
            } else {
                self.pos = self.src.len();
            }
            if c == '\n' { self.line += 1; self.col = 1; } else { self.col += 1; }
            Some(c)
        } else { None }
    }

    fn skip_ws(&mut self) {
        // Skips whitespace between tokens
        while let Some(c) = self.peek() {
            if c.is_whitespace() { self.bump(); } else { break; }
        }
    }

    fn error(&self, msg: &str) -> ! {
        eprintln!("Lexer Error at line {}, col {}: {}", self.line, self.col, msg);
        std::process::exit(1);
    }

    // Reads a sequence of letters (used for keywords/identifiers)
    fn read_word(&mut self) -> String {
        let mut s = String::new();
        while let Some(c) = self.peek() {
            if is_letter(c) { s.push(c); self.bump(); } else { break; }
        }
        s
    }

    // Reads a URL or address literal
    fn read_address(&mut self) -> String {
        let mut s = String::new();
        while let Some(c) = self.peek() {
            if is_address_char(c) { s.push(c); self.bump(); } else { break; }
        }
        if s.is_empty() { self.error("Expected address but found none"); }
        s
    }

    // Reads plain text until next #
    fn read_plain_text(&mut self) -> String {
        let mut s = String::new();
        while let Some(c) = self.peek() {
            if c == '#' { break; }
            s.push(c);
            self.bump();
        }
        s.trim().to_string()
    }

    // Reads # directives like #HAI, #GIMMEH, #OBTW
    fn read_hash_directive(&mut self) -> Tok {
        self.skip_ws();
        let mut word = String::new();
        while let Some(c) = self.peek() {
            if is_letter(c) { word.push(c); self.bump(); } else { break; }
        }

        match upper(&word).as_str() {
            "HAI" => Tok::HAI,
            "KTHXBYE" => Tok::KTHXBYE,
            "OBTW" => {
                // Multi-line comment until #TLDR
                let mut comment = String::new();
                self.skip_ws();
                while self.pos < self.src.len() {
                    if self.src[self.pos..].starts_with("#TLDR") { break; }
                    if let Some(c) = self.bump() { comment.push(c); } else { break; }
                }
                if !comment.trim().is_empty() {
                    self.lookahead.push_back(Tok::TEXT(comment.trim().to_string()));
                }
                Tok::OBTW
            }
            "TLDR" => Tok::TLDR,
            "MAEK" => { self.expect_keyword = true; Tok::MAEK },
            "OIC" => Tok::OIC,
            "GIMMEH" => { self.expect_keyword = true; Tok::GIMMEH },
            "MKAY" => Tok::MKAY,
            "I" => {
                self.skip_ws();
                let w2 = self.read_word();
                if upper(&w2) == "HAZ" { Tok::IHAZ } else { self.error("Expected HAZ after #I"); }
            }
            "IT" => {
                self.skip_ws();
                let w2 = self.read_word();
                if upper(&w2) == "IZ" { Tok::ITIZ } else { self.error("Expected IZ after #IT"); }
            }
            "LEMME" => {
                self.skip_ws();
                let w2 = self.read_word();
                if upper(&w2) == "SEE" { Tok::LEMMESEE } else { self.error("Expected SEE after #LEMME"); }
            }
            other => self.error(&format!("Unknown directive #{other}")),
        }
    }

    // Main token extraction logic
    /// Returns the next token from the source or `Tok::EOF` at the end of file.
    fn next_token_internal(&mut self) -> Tok {
        if let Some(tok) = self.lookahead.pop_front() { return tok; }
        self.skip_ws();

        if self.peek() == Some('#') { self.bump(); return self.read_hash_directive(); }

        // URLs become ADDRESS tokens
        let rest = &self.src[self.pos..];
        if rest.starts_with("http://") || rest.starts_with("https://") {
            let s = self.read_address();
            return Tok::ADDRESS(s);
        }

        // Identify letters or keywords
        if let Some(c) = self.peek() {
            if is_letter(c) {
                let w = self.read_word();
                if self.expect_keyword {
                    self.expect_keyword = false;
                    return match upper(&w).as_str() {
                        "HEAD"     => Tok::HEAD,
                        "TITLE"    => Tok::TITLE,
                        "PARAGRAF" => Tok::PARAGRAF,
                        "BOLD"     => Tok::BOLD,
                        "ITALICS"  => Tok::ITALICS,
                        "LIST"     => Tok::LIST,
                        "ITEM"     => Tok::ITEM,
                        "NEWLINE"  => Tok::NEWLINE,
                        "SOUNDZ"   => Tok::SOUNDZ,
                        "VIDZ"     => Tok::VIDZ,
                        _ => Tok::VARDEF(w),
                    };
                } else {
                    return Tok::VARDEF(w);
                }
            }
        }

        // Default to TEXT if none match
        let s = self.read_plain_text();
        if s.is_empty() { Tok::EOF } else { Tok::TEXT(s) }
    }
}

//
// ================== PARSER AND AST ==================
//
// The Parser consumes tokens and constructs an abstract syntax tree (AST)
// that represents the logical structure of the program.
//

/// Represents nodes in the Abstract Syntax Tree (AST).
#[derive(Debug, Clone)]
enum Node {
    Program { head: Option<Box<Node>>, body: Vec<Box<Node>>, comments: Vec<Box<Node>> },
    Head { title: Option<String> },
    Paragraph(Vec<Box<Node>>),
    Bold(Vec<Box<Node>>),
    Italic(Vec<Box<Node>>),
    List(Vec<Box<Node>>),
    ListItem(Vec<Box<Node>>),
    Audio(String),
    Video(String),
    Newline,
    Text(String),
    Comment(String),
    VarDef {},
}

/// The `Parser` builds an AST from tokens and generates HTML output.
struct Parser {
    tokens: Vec<Tok>,
    i: usize,
    scopes: Vec<HashMap<String, String>>, // Stack of scopes for variable management
}

impl Parser {
    /// Entry point for parsing a full LOLCODE program.
    ///
    /// # Panics
    /// Panics if the program does not begin with `#HAI` or end with `#KTHXBYE`.
    fn new(tokens: Vec<Tok>) -> Self {
        Self { tokens, i: 0, scopes: vec![HashMap::new()] }
    }

    // --- Scope management helpers ---
    fn push_scope(&mut self) { self.scopes.push(HashMap::new()); }
    fn pop_scope(&mut self) { self.scopes.pop(); }

    fn define_var(&mut self, name: &str, value: &str) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), value.to_string());
        }
    }

    fn lookup_var(&self, name: &str) -> Option<String> {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.get(name) {
                return Some(v.clone());
            }
        }
        None
    }

    // --- Token utilities ---
    fn cur(&self) -> &Tok { self.tokens.get(self.i).unwrap_or(&Tok::EOF) }
    fn next(&self) -> &Tok { self.tokens.get(self.i + 1).unwrap_or(&Tok::EOF) }
    fn bump(&mut self) { if self.i < self.tokens.len() { self.i += 1; } }

    //
    // ========== TOP-LEVEL PARSING ==========
    //

    pub fn parse_lolcode(&mut self) -> Box<Node> {
        if *self.cur() != Tok::HAI { panic!("Program must start with #HAI"); }
        self.bump();

        let comments = self.parse_comments();
        let head = self.parse_head();
        let body = self.parse_body();

        if *self.cur() != Tok::KTHXBYE {
            panic!("Program must end with #KTHXBYE");
        }
        self.bump();

        Box::new(Node::Program { head, body, comments })
    }

    //
    // ========== COMMENT PARSING ==========
    //

    fn parse_comments(&mut self) -> Vec<Box<Node>> {
        let mut out = vec![];
        while let Some(c) = self.parse_comment() { out.push(Box::new(c)); }
        out
    }

    fn parse_comment(&mut self) -> Option<Node> {
        if *self.cur() != Tok::OBTW { return None; }
        self.bump();

        let mut buf = String::new();
        while *self.cur() != Tok::TLDR && *self.cur() != Tok::EOF {
            if let Tok::TEXT(s) = self.cur().clone() {
                if !buf.is_empty() { buf.push(' '); }
                buf.push_str(&s);
            }
            self.bump();
        }

        if *self.cur() == Tok::TLDR { self.bump(); }

        Some(Node::Comment(buf.trim().to_string()))
    }

    //
    // ========== HEAD (TITLE) PARSING ==========
    //

    fn parse_head(&mut self) -> Option<Box<Node>> {
        if !(*self.cur() == Tok::MAEK && *self.next() == Tok::HEAD) {
            return None;
        }

        self.bump(); // MAEK
        self.bump(); // HEAD

        let mut title: Option<String> = None;

        if *self.cur() == Tok::GIMMEH && *self.next() == Tok::TITLE {
            self.bump(); // GIMMEH
            self.bump(); // TITLE

            // Accumulate title text until #MKAY
            let mut acc = String::new();
            loop {
                match self.cur().clone() {
                    Tok::TEXT(s) | Tok::VARDEF(s) => {
                        if !acc.is_empty() { acc.push(' '); }
                        acc.push_str(&s);
                        self.bump();
                    }
                    _ => break,
                }
            }

            if !acc.trim().is_empty() {
                title = Some(acc.trim().to_string());
            }

            if *self.cur() == Tok::MKAY { self.bump(); }
        }

        // Consume until OIC closes the head
        while *self.cur() != Tok::OIC && *self.cur() != Tok::EOF { self.bump(); }
        if *self.cur() == Tok::OIC { self.bump(); }

        Some(Box::new(Node::Head { title }))
    }

    //
    // ========== BODY PARSING ==========
    //

    fn parse_body(&mut self) -> Vec<Box<Node>> {
        let mut out = vec![];

        while *self.cur() != Tok::KTHXBYE && *self.cur() != Tok::EOF {
            if let Some(x) = self.parse_comment() { out.push(Box::new(x)); continue; }
            if let Some(x) = self.parse_paragraph() { out.push(Box::new(x)); continue; }
            if let Some(x) = self.parse_bold() { out.push(Box::new(x)); continue; }
            if let Some(x) = self.parse_italics() { out.push(Box::new(x)); continue; }
            if let Some(x) = self.parse_list() { out.push(Box::new(x)); continue; }
            if let Some(x) = self.parse_audio() { out.push(Box::new(x)); continue; }
            if let Some(x) = self.parse_video() { out.push(Box::new(x)); continue; }
            if let Some(x) = self.parse_newline() { out.push(Box::new(x)); continue; }
            if let Some(x) = self.parse_variable_define() { out.push(Box::new(x)); continue; }
            if let Some(x) = self.parse_variable_use() { out.push(Box::new(x)); continue; }

            // Inline paragraph parsing
            if matches!(self.cur(), Tok::TEXT(_) | Tok::VARDEF(_) | Tok::GIMMEH) {
                let mut items = vec![];

                while *self.cur() != Tok::EOF && !matches!(self.cur(), Tok::MAEK | Tok::KTHXBYE | Tok::OIC) {
                    if let Some(x) = self.parse_bold() { items.push(Box::new(x)); continue; }
                    if let Some(x) = self.parse_italics() { items.push(Box::new(x)); continue; }

                    match self.cur().clone() {
                        Tok::TEXT(s) | Tok::VARDEF(s) => {
                            items.push(Box::new(Node::Text(s)));
                            self.bump();
                        }
                        _ => break,
                    }
                }

                if !items.is_empty() {
                    out.push(Box::new(Node::Paragraph(items)));
                }
                continue;
            } else {
                self.bump();
            }
        }

        out
    }

    //
    // ========== PARAGRAPH PARSING ==========
    //

    fn parse_paragraph(&mut self) -> Option<Node> {
        if !(*self.cur() == Tok::MAEK && *self.next() == Tok::PARAGRAF) {
            return None;
        }

        self.bump();
        self.bump();

        // Each paragraph introduces its own scope
        self.push_scope();
        let mut items: Vec<Box<Node>> = vec![];

        while *self.cur() != Tok::OIC && *self.cur() != Tok::EOF {
            if let Some(x) = self.parse_variable_define() { items.push(Box::new(x)); continue; }
            if let Some(x) = self.parse_variable_use() { items.push(Box::new(x)); continue; }
            if let Some(x) = self.parse_bold() { items.push(Box::new(x)); continue; }
            if let Some(x) = self.parse_italics() { items.push(Box::new(x)); continue; }
            if let Some(x) = self.parse_list() { items.push(Box::new(x)); continue; }
            if let Some(x) = self.parse_audio() { items.push(Box::new(x)); continue; }
            if let Some(x) = self.parse_video() { items.push(Box::new(x)); continue; }
            if let Some(x) = self.parse_newline() { items.push(Box::new(x)); continue; }

            // Combine plain text sequences
            match self.cur().clone() {
                Tok::TEXT(first) | Tok::VARDEF(first) | Tok::ADDRESS(first) => {
                    let mut buf = first;
                    self.bump();
                    while matches!(self.cur(), Tok::TEXT(_) | Tok::VARDEF(_) | Tok::ADDRESS(_)) {
                        match self.cur().clone() {
                            Tok::TEXT(next) | Tok::VARDEF(next) | Tok::ADDRESS(next) => {
                                if !buf.is_empty() { buf.push(' '); }
                                buf.push_str(&next);
                            }
                            _ => {}
                        }
                        self.bump();
                    }
                    items.push(Box::new(Node::Text(buf)));
                }
                unexpected => {
                    panic!("Syntax error: unexpected token {:?} inside PARAGRAF at index {}", unexpected, self.i);
                }
            }
        }

        if *self.cur() == Tok::OIC { self.bump(); } else {
            panic!("Syntax error: expected #OIC to close PARAGRAF");
        }

        self.pop_scope();
        Some(Node::Paragraph(items))
    }

    //
    // ========== INLINE ELEMENTS ==========
    //

    fn parse_bold(&mut self) -> Option<Node> {
        if !(*self.cur() == Tok::GIMMEH && *self.next() == Tok::BOLD) { return None; }
        self.bump(); self.bump();

        let mut items: Vec<Box<Node>> = vec![];

        while !matches!(self.cur(), Tok::MKAY | Tok::EOF) {
            if let Some(x) = self.parse_italics() { items.push(Box::new(x)); continue; }
            if let Some(x) = self.parse_variable_use() { items.push(Box::new(x)); continue; }
            if let Some(x) = self.parse_newline() { items.push(Box::new(x)); continue; }

            match self.cur().clone() {
                Tok::TEXT(s) | Tok::VARDEF(s) | Tok::ADDRESS(s) => {
                    items.push(Box::new(Node::Text(s)));
                    self.bump();
                }
                _ => break,
            }
        }

        if *self.cur() == Tok::MKAY { self.bump(); } else {
            panic!("Syntax error: missing #MKAY after BOLD block");
        }
        Some(Node::Bold(items))
    }

    fn parse_italics(&mut self) -> Option<Node> {
        if !(*self.cur() == Tok::GIMMEH && *self.next() == Tok::ITALICS) { return None; }
        self.bump(); self.bump();

        let mut items: Vec<Box<Node>> = vec![];

        loop {
            match self.cur().clone() {
                Tok::MKAY => { self.bump(); break; }
                _ if self.parse_variable_use().is_some() => {
                    if let Some(x) = self.parse_variable_use() { items.push(Box::new(x)); }
                }
                Tok::TEXT(s) | Tok::VARDEF(s) | Tok::ADDRESS(s) => {
                    items.push(Box::new(Node::Text(s)));
                    self.bump();
                }
                Tok::EOF => panic!("Syntax error: unterminated ITALICS block"),
                other => panic!("Syntax error: expected text inside ITALICS until #MKAY, found {:?}", other),
            }
        }
        Some(Node::Italic(items))
    }

    //
    // ========== LISTS / MEDIA / NEWLINE ==========
    //

    fn parse_list(&mut self) -> Option<Node> {
        if !(*self.cur() == Tok::MAEK && *self.next() == Tok::LIST) { return None; }
        self.bump(); self.bump();

        let mut items = vec![];
        while *self.cur() == Tok::GIMMEH && *self.next() == Tok::ITEM {
            self.bump(); self.bump();
            let mut inner = vec![];

            if let Some(x) = self.parse_variable_use() { inner.push(Box::new(x)); }
            else if let Some(x) = self.parse_bold() { inner.push(Box::new(x)); }
            else if let Some(x) = self.parse_italics() { inner.push(Box::new(x)); }
            else if let Tok::TEXT(t) = self.cur().clone() { inner.push(Box::new(Node::Text(t))); self.bump(); }
            else if let Tok::VARDEF(t) = self.cur().clone() { inner.push(Box::new(Node::Text(t))); self.bump(); }

            if *self.cur() == Tok::MKAY { self.bump(); }
            items.push(Box::new(Node::ListItem(inner)));
        }

        if *self.cur() == Tok::OIC { self.bump(); }
        Some(Node::List(items))
    }

    fn parse_audio(&mut self) -> Option<Node> {
        if !(*self.cur() == Tok::GIMMEH && *self.next() == Tok::SOUNDZ) { return None; }
        self.bump(); self.bump();
        let addr = match self.cur().clone() {
            Tok::ADDRESS(a) => { self.bump(); a }
            _ => panic!("Expected ADDRESS after SOUNDZ"),
        };
        if *self.cur() == Tok::MKAY { self.bump(); }
        Some(Node::Audio(addr))
    }

    fn parse_video(&mut self) -> Option<Node> {
        if !(*self.cur() == Tok::GIMMEH && *self.next() == Tok::VIDZ) { return None; }
        self.bump(); self.bump();
        let addr = match self.cur().clone() {
            Tok::ADDRESS(a) => { self.bump(); a }
            _ => panic!("Expected ADDRESS after VIDZ"),
        };
        if *self.cur() == Tok::MKAY { self.bump(); }
        Some(Node::Video(addr))
    }

    fn parse_newline(&mut self) -> Option<Node> {
        if !(*self.cur() == Tok::GIMMEH && *self.next() == Tok::NEWLINE) { return None; }
        self.bump(); self.bump();
        Some(Node::Newline)
    }

    //
    // ========== VARIABLE DEFINITION & USE ==========
    //

    fn parse_variable_define(&mut self) -> Option<Node> {
        if *self.cur() != Tok::IHAZ { return None; }
        self.bump();

        let name = match self.cur().clone() {
            Tok::VARDEF(n) => { self.bump(); n }
            _ => panic!("Expected variable name after #I HAZ"),
        };

        if *self.cur() != Tok::ITIZ { panic!("Expected #IT IZ"); }
        self.bump();

        let value = match self.cur().clone() {
            Tok::TEXT(v) | Tok::VARDEF(v) => { self.bump(); v }
            _ => panic!("Expected TEXT or VARDEF as value after #IT IZ"),
        };

        if *self.cur() == Tok::MKAY { self.bump(); }

        self.define_var(&name, &value);
        Some(Node::VarDef {})
    }

    fn parse_variable_use(&mut self) -> Option<Node> {
        if *self.cur() != Tok::LEMMESEE { return None; }
        self.bump();

        let name = match self.cur().clone() {
            Tok::VARDEF(n) => { self.bump(); n }
            _ => panic!("Expected variable name after #LEMME SEE"),
        };

        if *self.cur() == Tok::MKAY { self.bump(); }

        if let Some(v) = self.lookup_var(&name) {
            Some(Node::Text(v))
        } else {
            eprintln!("Semantic Error: Undefined variable '{}'", name);
            std::process::exit(1);
        }
    }

    //
    // ========== HTML GENERATION ==========
    //
    /// Recursively converts the AST into an HTML string.
    pub fn to_html(&self, n: &Node) -> String {
        match n {
            Node::Program { head, body, comments } => {
                let mut head_html = String::new();
                if let Some(h) = head { head_html.push_str(&self.to_html(h)); }
                let mut body_html = String::new();
                for c in comments { body_html.push_str(&self.to_html(c)); }
                for b in body { body_html.push_str(&self.to_html(b)); }
                format!(
                    "<!doctype html>\n<html>\n{}\n<body>\n{}</body>\n</html>",
                    head_html, body_html
                )
            }
            Node::Head { title } => {
                let t = title.clone().unwrap_or_default();
                format!("<head><title>{}</title></head>", html_escape(&t))
            }
            Node::Paragraph(items) => {
                let parts = items.iter().map(|x| self.to_html(x)).filter(|s| !s.is_empty()).collect::<Vec<_>>();
                let joined = parts.join(" ").trim().to_string();
                format!("<p>{}</p>", fix_spacing(&joined))
            }
            Node::Bold(items) => {
                let parts = items.iter().map(|x| self.to_html(x)).collect::<Vec<_>>();
                format!("<strong>{}</strong>", fix_spacing(&parts.join(" ")))
            }
            Node::Italic(items) => {
                let parts = items.iter().map(|x| self.to_html(x)).collect::<Vec<_>>();
                format!("<em>{}</em>", fix_spacing(&parts.join(" ")))
            }
            Node::List(items) => format!("<ul>{}</ul>", items.iter().map(|x| self.to_html(x)).collect::<String>()),
            Node::ListItem(inner) => format!("<li>{}</li>", inner.iter().map(|x| self.to_html(x)).collect::<String>()),
            Node::Audio(a) => format!("<audio controls src=\"{}\"></audio>", html_attr(a)),
            Node::Video(v) => format!("<video controls src=\"{}\"></video>", html_attr(v)),
            Node::Newline => "<br/>".to_string(),
            Node::Text(s) => html_escape(s),
            Node::Comment(c) => format!("<!-- {} -->", c),
            Node::VarDef { .. } => String::new(),
        }
    }
}

// Escapes HTML special characters to prevent rendering issues.
fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

// Escapes attributes in tags like src="..."
fn html_attr(s: &str) -> String {
    html_escape(s).replace('\'', "&#39;")
}

//
// ================== DRIVER / ENTRY POINT ==================
//

// Reads the entire input file as a string.
fn read_entire_file(p: &Path) -> io::Result<String> {
    let mut f = fs::File::open(p)?;
    let mut s = String::new();
    f.read_to_string(&mut s)?;
    Ok(s)
}

// Main function orchestrates the compilation pipeline.
fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: lolcompiler <input.lol>");
        std::process::exit(1);
    }

    let path = PathBuf::from(&args[1]);

    // Ensure file has .lol extension
    let is_lol = path
        .extension()
        .and_then(OsStr::to_str)
        .map(|ext| ext.eq_ignore_ascii_case("lol"))
        .unwrap_or(false);

    if !is_lol {
        eprintln!("Error: input file must have a .lol extension.\nGot: {}", path.display());
        std::process::exit(1);
    }

    let src = read_entire_file(&path).expect("Could not read file");

    // Lexical Analysis
    let mut lex = Lexer::new(&src);
    let mut tokens = vec![];
    loop {
        let t = lex.next_token_internal();
        tokens.push(t.clone());
        if t == Tok::EOF { break; }
    }

    // Parsing
    let mut p = Parser::new(tokens);
    let ast = p.parse_lolcode();

    // HTML generation
    let html = p.to_html(&ast);

    // Write and open output
    let out = path.with_extension("html");
    fs::write(&out, html).unwrap();
    println!("✅ Compiled to {}", out.display());

    if let Err(e) = open_in_browser(&out) {
        eprintln!("Note: could not open browser automatically: {}", e);
    }
}
