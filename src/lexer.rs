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

#[derive(Debug)]
pub struct Token {
    value: String,
    token_type: TokenType,
    pos: usize,
}

static EOF: char = '\0';
impl Lexer {

    pub fn new(src: String) -> Self {
        Self { src, pos: 0 }
    }
    fn consume(&mut self) -> char {
        let curr = self.current();
        self.pos += 1;
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
        let mut c = self.consume();
        while c.is_digit(10) {
            value.push(c);
            c = self.consume();
        }

        Token { value, token_type: TokenType::IntegerLiteral, pos: start }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut result = Vec::new();
        loop {
            let c = self.current();
            if c == EOF { break; }
            if c.is_digit(10) {
                result.push(self.lex_integer());
            }
        }
        result
    }
}

#[cfg(test)]
mod tests {
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
}