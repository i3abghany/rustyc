pub struct Lexer {
    src: String,
    pos: usize,
}
#[derive(Debug, PartialEq, Eq)]
enum TokenType {
    Identifier,
    Type,
    Keyword,
    Equals,
    Plus,
    SemiColon,
    IntegerLiteral,
    Eof,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    value: String,
    token_type: TokenType,
    pos: usize,
}

static EOF: char = '\0';

// TODO update these vectors to represent the specification.
static STR_ALLOWED_SYMBOLS: [char; 2] = ['_', '$'];

static TYPES: [&str; 1] = ["int"];

static KEYWORDS: [&str; 4] = ["if", "while", "do", "return"];

impl Lexer {
    pub fn new(src: String) -> Self {
        Self { src, pos: 0 }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut result = Vec::new();
        loop {
            let c = self.current();
            if c == EOF {
                break;
            }
            if c.is_digit(10) {
                result.push(self.lex_integer());
            } else if c.is_whitespace() {
                self.advance();
            } else if let Some(token_type) = self.get_single_char_token(c) {
                result.push(Token { value: String::from(c), token_type, pos: self.pos});
                self.advance();
            } else {
                result.push(self.lex_string());
            }
        }
        result
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn consume(&mut self) -> char {
        let curr = self.current();
        self.advance();
        curr
    }

    fn current(&self) -> char {
        if self.pos >= self.src.len() {
            return EOF;
        }

        self.src.chars().nth(self.pos).unwrap()
    }

    fn lex_integer(&mut self) -> Token {
        let mut value = String::new();
        let start = self.pos;
        let mut c = self.current();

        while c.is_digit(10) {
            value.push(self.consume());
            c = self.current();
        }

        Token {
            value,
            token_type: TokenType::IntegerLiteral,
            pos: start,
        }
    }

    fn lex_string(&mut self) -> Token {
        let mut value = String::new();
        let start = self.pos;
        let mut c = self.current();

        while c.is_alphanumeric() || STR_ALLOWED_SYMBOLS.contains(&c) {
            value.push(self.consume());
            c = self.current();
        }

        let token_type: TokenType = self.get_str_token_type(value.as_str());
        Token {
            value,
            token_type,
            pos: start,
        }
    }

    fn get_str_token_type(&self, string: &str) -> TokenType {
        return if TYPES.contains(&string) {
            TokenType::Type
        } else if KEYWORDS.contains(&string) {
            TokenType::Keyword
        } else {
            TokenType::Identifier
        }
    }

    fn get_single_char_token(&self, c: char) -> Option<TokenType> {
        return match c {
            '+' => Some(TokenType::Plus),
            '=' => Some(TokenType::Equals),
            ';' => Some(TokenType::SemiColon),
            _ => None,
        };
    }
}

#[cfg(test)]
mod tests {
    use std::iter::zip;
    use super::*;

    #[rstest::rstest]
    #[case("0")]
    #[case("1")]
    #[case("123")]
    fn test_lex_integer(#[case] test_case: String) {
        let tokens = Lexer::new(test_case.clone()).lex();
        assert_eq!(1, tokens.len());
        assert_eq!(test_case, tokens[0].value);
        assert_eq!(TokenType::IntegerLiteral, tokens[0].token_type);
    }

    #[rstest::rstest]
    #[case("+", TokenType::Plus)]
    #[case("=", TokenType::Equals)]
    #[case(";", TokenType::SemiColon)]
    #[case("int", TokenType::Type)]
    #[case("while", TokenType::Keyword)]
    #[case("hello", TokenType::Identifier)]
    #[case("Pa$5W_rd", TokenType::Identifier)]
    fn test_lex_string(#[case] test_case: String, #[case] expected_type: TokenType) {
        let tokens = Lexer::new(test_case.clone()).lex();
        assert_eq!(1, tokens.len());
        assert_eq!(test_case, tokens[0].value);
        assert_eq!(expected_type, tokens[0].token_type);
    }

    #[rstest::rstest]
    #[case("int x = 55;     ", vec![
        Token{value: "int".to_string(), token_type: TokenType::Type, pos: 0},
        Token{value: "x".to_string(), token_type: TokenType::Identifier, pos: 4},
        Token{value: "=".to_string(), token_type: TokenType::Equals, pos: 6},
        Token{value: "55".to_string(), token_type: TokenType::IntegerLiteral, pos: 8},
        Token{value: ";".to_string(), token_type: TokenType::SemiColon, pos: 10},
    ])]
    fn test_lex_multiple_tokens(#[case] test_case: String, #[case] expected: Vec<Token>) {
        let result = Lexer::new(test_case.clone()).lex();
        assert_eq!(expected.len(), result.len());
        for (x, y) in zip(expected, result) {
            assert_eq!(x, y);
        }
    }

}
