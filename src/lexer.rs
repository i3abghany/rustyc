use crate::tokens::*;

pub struct Lexer {
    src: String,
    pos: usize,
}

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
            if c.is_ascii_digit() {
                result.push(self.lex_integer());
            } else if c.is_whitespace() {
                self.advance();
            } else if let Some(token_type) = self.get_single_char_token(c) {
                result.push(Token {
                    value: String::from(c),
                    token_type,
                    pos: self.pos,
                });
                self.advance();
            } else {
                result.push(self.lex_string());
            }
        }
        result.push(Token {
            value: EOF.to_string(),
            token_type: TokenType::Eof,
            pos: EOF_POS,
        });
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

        while c.is_ascii_digit() {
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

    fn get_keyword_token_type(&self, string: &str) -> TokenType {
        KEYWORDS.get(string).unwrap().clone()
    }

    fn get_str_token_type(&self, string: &str) -> TokenType {
        if TYPES.contains_key(string) {
            TokenType::Type
        } else if KEYWORDS.contains_key(string) {
            self.get_keyword_token_type(string)
        } else {
            TokenType::Identifier
        }
    }

    fn get_single_char_token(&self, c: char) -> Option<TokenType> {
        SINGLE_CHAR_TOKENS.get(&c).cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::iter::zip;

    #[rstest::rstest]
    #[case("0")]
    #[case("1")]
    #[case("123")]
    fn test_lex_integer(#[case] test_case: String) {
        let tokens = Lexer::new(test_case.clone()).lex();
        assert_eq!(2, tokens.len());
        assert_eq!(test_case, tokens[0].value);
        assert_eq!(TokenType::IntegerLiteral, tokens[0].token_type);
        assert_eq!(TokenType::Eof, tokens[1].token_type);
    }

    #[rstest::rstest]
    #[case("+", TokenType::Plus)]
    #[case("=", TokenType::Equals)]
    #[case("*", TokenType::Star)]
    #[case("-", TokenType::Minus)]
    #[case("/", TokenType::Slash)]
    #[case(";", TokenType::SemiColon)]
    #[case("int", TokenType::Type)]
    #[case("while", TokenType::While)]
    #[case("hello", TokenType::Identifier)]
    #[case("Pa$5W_rd", TokenType::Identifier)]
    fn test_lex_string(#[case] test_case: String, #[case] expected_type: TokenType) {
        let tokens = Lexer::new(test_case.clone()).lex();
        assert_eq!(2, tokens.len());
        assert_eq!(test_case, tokens[0].value);
        assert_eq!(expected_type, tokens[0].token_type);
        assert_eq!(TokenType::Eof, tokens[1].token_type);
    }

    #[rstest::rstest]
    #[case("int x = 55;     ", vec![
        Token{value: "int".to_string(), token_type: TokenType::Type, pos: 0},
        Token{value: "x".to_string(), token_type: TokenType::Identifier, pos: 4},
        Token{value: "=".to_string(), token_type: TokenType::Equals, pos: 6},
        Token{value: "55".to_string(), token_type: TokenType::IntegerLiteral, pos: 8},
        Token{value: ";".to_string(), token_type: TokenType::SemiColon, pos: 10},
        Token{value: EOF.to_string(), token_type: TokenType::Eof, pos: EOF_POS},
    ])]
    #[case("return x;", vec![
        Token{value: "return".to_string(), token_type: TokenType::Return, pos: 0},
        Token{value: "x".to_string(), token_type: TokenType::Identifier, pos: 7},
        Token{value: ";".to_string(), token_type: TokenType::SemiColon, pos: 8},
        Token{value: EOF.to_string(), token_type: TokenType::Eof, pos: EOF_POS},
    ])]
    #[case("return 1234;", vec![
        Token{value: "return".to_string(), token_type: TokenType::Return, pos: 0},
        Token{value: "1234".to_string(), token_type: TokenType::IntegerLiteral, pos: 7},
        Token{value: ";".to_string(), token_type: TokenType::SemiColon, pos: 11},
        Token{value: EOF.to_string(), token_type: TokenType::Eof, pos: EOF_POS},
    ])]
    fn test_lex_multiple_tokens(#[case] test_case: String, #[case] expected: Vec<Token>) {
        let result = Lexer::new(test_case.clone()).lex();
        assert_eq!(expected.len(), result.len());
        for (x, y) in zip(expected, result) {
            assert_eq!(x, y);
        }
    }
}
