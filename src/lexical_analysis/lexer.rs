use crate::lexical_analysis::tokens::*;

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
            } else if let Some(mut token_type) = self.get_single_char_token(c) {
                let pos = self.pos;
                let mut value = String::from(c);
                macro_rules! match_double_char_token {
                    ($next:expr, $token_type:expr) => {
                        if self.peek(1) == $next {
                            value = format!("{}{}", c, $next);
                            token_type = $token_type;
                            self.advance();
                        }
                    };
                }

                match c {
                    '|' => match_double_char_token!('|', TokenType::BarBar),
                    '&' => match_double_char_token!('&', TokenType::AndAnd),
                    '=' => match_double_char_token!('=', TokenType::EqualsEquals),
                    '!' => match_double_char_token!('=', TokenType::NotEquals),
                    '>' => match_double_char_token!('=', TokenType::GreaterThanEquals),
                    '<' => match_double_char_token!('=', TokenType::LessThanEquals),
                    '/' => {
                        if self.peek(1) == '/' {
                            while self.current() != '\n' && self.current() != EOF {
                                self.advance();
                            }
                            continue;
                        }
                    }
                    _ => {}
                }

                result.push(Token {
                    value,
                    token_type,
                    pos,
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

    fn peek(&self, offset: usize) -> char {
        let index = self.pos + offset;

        if index >= self.src.len() {
            return EOF;
        }

        self.src.chars().nth(index).unwrap()
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
        self.peek(0)
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
        if TYPES.contains(string) {
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
    #[case("^", TokenType::Caret)]
    #[case("&", TokenType::And)]
    #[case("&&", TokenType::AndAnd)]
    #[case("|", TokenType::Bar)]
    #[case("||", TokenType::BarBar)]
    #[case("==", TokenType::EqualsEquals)]
    #[case("!=", TokenType::NotEquals)]
    #[case(">", TokenType::GreaterThan)]
    #[case(">=", TokenType::GreaterThanEquals)]
    #[case("<", TokenType::LessThan)]
    #[case("<=", TokenType::LessThanEquals)]
    #[case("(", TokenType::OpenParen)]
    #[case(")", TokenType::CloseParen)]
    #[case("{", TokenType::OpenCurly)]
    #[case("}", TokenType::CloseCurly)]
    #[case("!", TokenType::Bang)]
    #[case("int", TokenType::Type)]
    #[case("true", TokenType::Identifier)]
    #[case("false", TokenType::Identifier)]
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
    #[case("// This is a comment ending with EOF")]
    #[case("// This is a comment ending with a new line\n")]
    fn test_comment(#[case] test_case: String) {
        let tokens = Lexer::new(test_case.clone()).lex();
        assert_eq!(1, tokens.len());
        assert_eq!(TokenType::Eof, tokens[0].token_type);
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
