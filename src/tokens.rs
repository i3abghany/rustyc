#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenType {
    Identifier,
    Type,
    Keyword,
    Equals,
    Plus,
    SemiColon,
    IntegerLiteral,
    Eof,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub value: String,
    pub token_type: TokenType,
    pub pos: usize,
}

pub static EOF: char = '\0';

// TODO update these vectors to represent the specification.
pub static STR_ALLOWED_SYMBOLS: [char; 2] = ['_', '$'];

pub static TYPES: [&str; 1] = ["int"];

pub static KEYWORDS: [&str; 4] = ["if", "while", "do", "return"];
