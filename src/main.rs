use std::env;
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::collections::{HashMap, VecDeque};
use std::ffi::OsStr;

//
// ========== TRAITS ==========
//
pub trait Compiler {
    fn compile(&mut self, source: &str);
    fn next_token(&mut self) -> String;
    fn parse(&mut self);
    fn current_token(&self) -> String;
    fn set_current_token(&mut self, tok: String);
}

pub trait LexicalAnalyzer {
    fn get_char(&mut self) -> char;
    fn add_char(&mut self, c: char);
    fn lookup(&self, s: &str) -> bool;
}

//
// ========== TOKEN DEFINITIONS ==========
//
#[derive(Debug, Clone, PartialEq)]
enum Tok {
    HAI, KTHXBYE, OBTW, TLDR, MAEK, OIC, GIMMEH, MKAY,
    HEAD, TITLE, PARAGRAF, BOLD, ITALICS, LIST, ITEM,
    NEWLINE, SOUNDZ, VIDZ, IHAZ, ITIZ, LEMMESEE,
    VARDEF(String), ADDRESS(String), TEXT(String), EOF,
}

//
// ========== HELPERS ==========
//
fn upper(s: &str) -> String { s.chars().flat_map(|c| c.to_uppercase()).collect() }
fn is_letter(c: char) -> bool { c.is_ascii_alphabetic() }
fn is_allowed_text_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || matches!(c, ' ' | ',' | '.' | '"' | '?' | '_' | ':' | '/')
}
fn is_address_char(c: char) -> bool { c != ' ' && c != '\n' && c != '\r' && c != '\t' }

//
// ========== LEXER ==========
//
struct Lexer {
    src: String,
    pos: usize,
    line: usize,
    col: usize,
    lookahead: VecDeque<Tok>,
}

impl Lexer {
    fn new(input: &str) -> Self {
        Self { src: input.to_string(), pos: 0, line: 1, col: 1, lookahead: VecDeque::new() }
    }

    fn peek(&self) -> Option<char> { self.src[self.pos..].chars().next() }

    fn bump(&mut self) -> Option<char> {
        if self.pos >= self.src.len() { return None; }
        let mut iter = self.src[self.pos..].char_indices();
        if let Some((_, c)) = iter.next() {
            if let Some((next, _)) = iter.next() { self.pos += next; } else { self.pos = self.src.len(); }
            if c == '\n' { self.line += 1; self.col = 1; } else { self.col += 1; }
            Some(c)
        } else { None }
    }

    fn skip_ws(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() { self.bump(); } else { break; }
        }
    }

    fn error(&self, msg: &str) -> ! {
        eprintln!("Lexer Error at line {}, col {}: {}", self.line, self.col, msg);
        std::process::exit(1);
    }

    fn read_word(&mut self) -> String {
        let mut s = String::new();
        while let Some(c) = self.peek() {
            if is_letter(c) { s.push(c); self.bump(); } else { break; }
        }
        s
    }

    fn read_address(&mut self) -> String {
        let mut s = String::new();
        while let Some(c) = self.peek() {
            if is_address_char(c) { s.push(c); self.bump(); } else { break; }
        }
        if s.is_empty() { self.error("Expected address but found none"); }
        s
    }

    fn read_plain_text(&mut self) -> String {
        let mut s = String::new();
        while let Some(c) = self.peek() {
            if c == '#'  { 
                break; 
            }
            s.push(c); 
            self.bump();
        }
        s.trim().to_string()
    }

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
            "MAEK" => Tok::MAEK,
            "OIC" => Tok::OIC,
            "GIMMEH" => Tok::GIMMEH,
            "MKAY" => Tok::MKAY,
            "I" => { self.skip_ws(); let w2 = self.read_word(); if upper(&w2) == "HAZ" { Tok::IHAZ } else { self.error("Expected HAZ after #I"); } }
            "IT" => { self.skip_ws(); let w2 = self.read_word(); if upper(&w2) == "IZ" { Tok::ITIZ } else { self.error("Expected IZ after #IT"); } }
            "LEMME" => { self.skip_ws(); let w2 = self.read_word(); if upper(&w2) == "SEE" { Tok::LEMMESEE } else { self.error("Expected SEE after #LEMME"); } }
            other => self.error(&format!("Unknown directive #{other}")),
        }
    }

    fn next_token_internal(&mut self) -> Tok {
        if let Some(tok) = self.lookahead.pop_front() { return tok; }
        self.skip_ws();
        let Some(c) = self.peek() else { return Tok::EOF; };
        if c == '#' { self.bump(); return self.read_hash_directive(); }
        if is_letter(c) {
            let w = self.read_word();
            match upper(&w).as_str() {
                "HEAD" => Tok::HEAD,
                "TITLE" => Tok::TITLE,
                "PARAGRAF" => Tok::PARAGRAF,
                "BOLD" => Tok::BOLD,
                "ITALICS" => Tok::ITALICS,
                "LIST" => Tok::LIST,
                "ITEM" => Tok::ITEM,
                "NEWLINE" => Tok::NEWLINE,
                "SOUNDZ" => Tok::SOUNDZ,
                "VIDZ" => Tok::VIDZ,
                _ => Tok::VARDEF(w),
            }
        } else if is_address_char(c) {
            let s = self.read_address();
            if s.contains('/') || s.contains(':') { Tok::ADDRESS(s) } else { Tok::TEXT(s) }
        } else if is_allowed_text_char(c) {
            let s = self.read_plain_text();
            Tok::TEXT(s)
        } else {
            self.error(&format!("Illegal character '{}'", c));
        }
    }
}

//
// ========== AST + PARSER (BOXED NODE FIX) ==========
//
#[derive(Debug, Clone)]
enum Node {
    Program { head: Option<Box<Node>>, body: Vec<Box<Node>>, comments: Vec<Box<Node>> },
    Head { title: Option<String> },
    Paragraph(Vec<Box<Node>>),
    Bold(String),
    Italic(String),
    List(Vec<Box<Node>>),
    ListItem(Vec<Box<Node>>),
    Audio(String),
    Video(String),
    Newline,
    Text(String),
    Comment(String),
    VarDef {  },
    VarUse(String),
}

struct Parser {
    tokens: Vec<Tok>,
    i: usize,
    vars: HashMap<String, String>,
}

impl Parser {
    fn new(tokens: Vec<Tok>) -> Self {
        Self { tokens, i: 0, vars: HashMap::new() }
    }

    fn cur(&self) -> &Tok { self.tokens.get(self.i).unwrap_or(&Tok::EOF) }
    fn next(&self) -> &Tok { self.tokens.get(self.i + 1).unwrap_or(&Tok::EOF) }
    fn bump(&mut self) { if self.i < self.tokens.len() { self.i += 1; } }

    pub fn parse_lolcode(&mut self) -> Box<Node> {
        if *self.cur() != Tok::HAI { panic!("Program must start with #HAI"); }
        self.bump();
        let comments = self.parse_comments();
        let head = self.parse_head();
        let body = self.parse_body();
        if *self.cur() != Tok::KTHXBYE { panic!("Program must end with #KTHXBYE"); }
        self.bump();
        Box::new(Node::Program { head, body, comments })
    }

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

        // Accumulate title from TEXT or VARDEF until #MKAY
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

    // Consume until OIC
    while *self.cur() != Tok::OIC && *self.cur() != Tok::EOF { self.bump(); }
    if *self.cur() == Tok::OIC { self.bump(); }

    Some(Box::new(Node::Head { title }))
}

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

// Accumulate inline paragraph content including bold/italic
if matches!(self.cur(), Tok::TEXT(_) | Tok::VARDEF(_) | Tok::GIMMEH) {
    let mut items = vec![];

    while *self.cur() != Tok::EOF && !matches!(self.cur(), Tok::MAEK | Tok::KTHXBYE | Tok::OIC) {
        if let Some(x) = self.parse_bold() {
            items.push(Box::new(x));
            continue;
        }
        if let Some(x) = self.parse_italics() {
            items.push(Box::new(x));
            continue;
        }
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

    if *self.cur() == Tok::KTHXBYE {
        while let Tok::TEXT(t) = self.cur().clone() {
    if t.trim().is_empty() {
        self.bump();
    } else {
        break;
    }
}
    }

    out
}


    fn parse_paragraph(&mut self) -> Option<Node> {
    if !(*self.cur() == Tok::MAEK && *self.next() == Tok::PARAGRAF) { return None; }
    self.bump(); // MAEK
    self.bump(); // PARAGRAF

    let mut items: Vec<Box<Node>> = vec![];

    // Optional leading variable define
    if let Some(vd) = self.parse_variable_define() {
        items.push(Box::new(vd));
    }

    while *self.cur() != Tok::OIC && *self.cur() != Tok::EOF {
        // Stray #MKAY is illegal in a paragraph
        if *self.cur() == Tok::MKAY {
            panic!("Syntax error: #MKAY outside of a #GIMMEH block inside PARAGRAF");
        }

        if let Some(x) = self.parse_variable_use()   { items.push(Box::new(x)); continue; }
        if let Some(x) = self.parse_bold()           { items.push(Box::new(x)); continue; }
        if let Some(x) = self.parse_italics()        { items.push(Box::new(x)); continue; }
        if let Some(x) = self.parse_list()           { items.push(Box::new(x)); continue; }
        if let Some(x) = self.parse_audio()          { items.push(Box::new(x)); continue; }
        if let Some(x) = self.parse_video()          { items.push(Box::new(x)); continue; }
        if let Some(x) = self.parse_newline()        { items.push(Box::new(x)); continue; }

        // Coalesce consecutive TEXT / VARDEF tokens into one text node with spaces
        match self.cur().clone() {
            Tok::TEXT(first) | Tok::VARDEF(first) => {
                let mut buf = first;
                self.bump();
                while matches!(self.cur(), Tok::TEXT(_) | Tok::VARDEF(_)) {
                    match self.cur().clone() {
                        Tok::TEXT(next) | Tok::VARDEF(next) => {
                            if !buf.is_empty() { buf.push(' '); }
                            buf.push_str(&next);
                        }
                        _ => {}
                    }
                    self.bump();
                }
                items.push(Box::new(Node::Text(buf)));
            }
            // Anything else is an unexpected token in a paragraph
            unexpected => {
                panic!("Syntax error: unexpected token inside PARAGRAF: {:?}", unexpected);
            }
        }
    }

    if *self.cur() == Tok::OIC { self.bump(); } else {
        panic!("Syntax error: expected #OIC to close PARAGRAF");
    }

    Some(Node::Paragraph(items))
}


    fn parse_bold(&mut self) -> Option<Node> {
    if !(*self.cur() == Tok::GIMMEH && *self.next() == Tok::BOLD) { return None; }
    self.bump(); // GIMMEH
    self.bump(); // BOLD

    let mut buf = String::new();
    while !matches!(self.cur(), Tok::MKAY | Tok::EOF) {
        match self.cur().clone() {
            Tok::TEXT(s) | Tok::VARDEF(s) => {
                if !buf.is_empty() { buf.push(' '); }
                buf.push_str(&s);
                self.bump();
            }
            _ => break,
        }
    }

    if *self.cur() == Tok::MKAY { self.bump(); } else {
        panic!("Syntax error: missing #MKAY after BOLD block");
    }
    Some(Node::Bold(buf.trim().to_string()))
}


    fn parse_italics(&mut self) -> Option<Node> {
    if !(*self.cur() == Tok::GIMMEH && *self.next() == Tok::ITALICS) { return None; }
    self.bump(); // GIMMEH
    self.bump(); // ITALICS

    let mut buf = String::new();
    while !matches!(self.cur(), Tok::MKAY | Tok::EOF) {
        match self.cur().clone() {
            Tok::TEXT(s) | Tok::VARDEF(s) => {
                if !buf.is_empty() { buf.push(' '); }
                buf.push_str(&s);
                self.bump();
            }
            _ => break, // do not swallow non text tokens here
        }
    }

    if *self.cur() == Tok::MKAY { self.bump(); } else {
        panic!("Syntax error: missing #MKAY after ITALICS block");
    }
    Some(Node::Italic(buf.trim().to_string()))
}

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

    fn parse_variable_define(&mut self) -> Option<Node> {
        if *self.cur() != Tok::IHAZ { return None; }
        self.bump();
        let name = match self.cur().clone() {
            Tok::VARDEF(n) => { self.bump(); n }
            _ => panic!("Expected variable name"),
        };
        if *self.cur() != Tok::ITIZ { panic!("Expected #IT IZ"); }
        self.bump();
        let value = match self.cur().clone() {
            Tok::TEXT(v) => { self.bump(); v }
            _ => panic!("Expected TEXT value"),
        };
        if *self.cur() == Tok::MKAY { self.bump(); }
        self.vars.insert(name.clone(), value.clone());
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
        Some(Node::VarUse(name))
    }

    pub fn to_html(&self, n: &Node) -> String {
        match n {
            Node::Program { head, body, comments } => {
                let mut head_html = String::new();
                if let Some(h) = head { head_html.push_str(&self.to_html(h)); }
                let mut body_html = String::new();
                for c in comments { body_html.push_str(&self.to_html(c)); }
                for b in body { body_html.push_str(&self.to_html(b)); }
                format!("<!doctype html>\n<html>\n{}\n<body>\n{}</body>\n</html>", head_html, body_html)
            }
            Node::Head { title } => {
                let t = title.clone().unwrap_or_default();
                format!("<head><title>{}</title></head>", html_escape(&t))
            }
            Node::Paragraph(items) => {
    let parts = items
        .iter()
        .map(|x| self.to_html(x))
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>();
    format!("<p>{}</p>", parts.join(" ").trim())
},

            Node::Bold(s) => format!("<strong>{}</strong>", html_escape(s)),
            Node::Italic(s) => format!("<em>{}</em>", html_escape(s)),
            Node::List(items) => format!("<ul>{}</ul>", items.iter().map(|x| self.to_html(x)).collect::<String>()),
            Node::ListItem(inner) => format!("<li>{}</li>", inner.iter().map(|x| self.to_html(x)).collect::<String>()),
            Node::Audio(a) => format!("<audio controls src=\"{}\"></audio>", html_attr(a)),
            Node::Video(v) => format!("<video controls src=\"{}\"></video>", html_attr(v)),
            Node::Newline => "<br/>".to_string(),
            Node::Text(s) => html_escape(s),
            Node::Comment(c) => format!("<!-- {} -->", c),
            Node::VarDef { .. } =>String::new(), // variable definitions do not produce output
            Node::VarUse(name) => {
                if let Some(v) = self.vars.get(name) { html_escape(v) }
                else { format!("{{{{{}}}}}", html_escape(name)) }
            }
        }
    }
}

fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;").replace('<', "&lt;").replace('>', "&gt;").replace('"', "&quot;")
}
fn html_attr(s: &str) -> String {
    html_escape(s).replace('\'', "&#39;")
}

//
// ========== DRIVER ==========
//
fn read_entire_file(p: &Path) -> io::Result<String> {
    let mut f = fs::File::open(p)?;
    let mut s = String::new();
    f.read_to_string(&mut s)?;
    Ok(s)
}

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: cargo run -- <input.lol>");
        std::process::exit(1);
    }

    let path = PathBuf::from(&args[1]);

    // accept only .lol files
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

    let mut lex = Lexer::new(&src);
    let mut tokens = vec![];
    loop {
        let t = lex.next_token_internal();
        tokens.push(t.clone());
        if t == Tok::EOF { break; }
    }

    let mut p = Parser::new(tokens);
    let ast = p.parse_lolcode();
    let html = p.to_html(&ast);

    let out = path.with_extension("html");
    fs::write(&out, html).unwrap();
    println!("✅ Compiled to {}", out.display());
}
