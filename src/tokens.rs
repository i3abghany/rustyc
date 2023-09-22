use phf::phf_map;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenType {
    Identifier,
    Type,
    Return,
    While,
    If,
    Do,
    Equals,
    Plus,
    Minus,
    Star,
    Slash,
    Bar,
    And,
    BarBar,
    AndAnd,
    Caret,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
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
pub static EOF_POS: usize = usize::MAX;

// TODO update these vectors to represent the specification.
pub static STR_ALLOWED_SYMBOLS: [char; 2] = ['_', '$'];

pub const TYPES: phf::Map<&'static str, TokenType> = phf_map! {
    "int" => TokenType::Type,
};

pub const KEYWORDS: phf::Map<&'static str, TokenType> = phf_map! {
    "if" => TokenType::If,
    "while" => TokenType::While,
    "do" => TokenType::Do,
    "return" => TokenType::Return,
};

pub const SINGLE_CHAR_TOKENS: phf::Map<char, TokenType> = phf_map! {
    '+' => TokenType::Plus,
    '-' => TokenType::Minus,
    '*' => TokenType::Star,
    '/' => TokenType::Slash,
    '=' => TokenType::Equals,
    ';' => TokenType::SemiColon,
    '(' => TokenType::OpenParen,
    ')' => TokenType::CloseParen,
    '{' => TokenType::OpenCurly,
    '}' => TokenType::CloseCurly,
    '^' => TokenType::Caret,
    '|' => TokenType::Bar,
    '&' => TokenType::And,
};
