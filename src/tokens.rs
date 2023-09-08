use phf::phf_map;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenType {
    Identifier,
    Type,
    ReturnKeyword,
    WhileKeyword,
    IfKeyword,
    DoKeyword,
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

pub const TYPES: phf::Map<&'static str, TokenType> = phf_map! {
    "int" => TokenType::Type,
};

pub const KEYWORDS: phf::Map<&'static str, TokenType> = phf_map! {
    "if" => TokenType::IfKeyword,
    "while" => TokenType::WhileKeyword,
    "do" => TokenType::DoKeyword,
    "return" => TokenType::ReturnKeyword,
};

pub const SINGLE_CHAR_TOKENS: phf::Map<char, TokenType> = phf_map! {
    '+' => TokenType::Plus,
    '=' => TokenType::Equals,
    ';' => TokenType::SemiColon,
};
