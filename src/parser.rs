use crate::ast::*;
use crate::tokens::*;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn parse(&mut self) -> ASTNode {
        let mut result = Vec::new();
        loop {
            let c = self.current();
            if c.token_type == TokenType::Eof {
                break;
            }
            result.push(self.parse_statement());
        }
        ASTNode::Program(result)
    }

    fn parse_statement(&mut self) -> ASTNode {
        match self.current().token_type {
            TokenType::Type => self.parse_declaration(),
            TokenType::Return => self.parse_return_statement(),
            _ => panic!("Parser: Unexpected token {:?}", self.current()),
        }
    }

    fn peak(&self, offset: usize) -> &Token {
        if (self.pos + offset) >= self.tokens.len() {
            self.EOF_token()
        } else {
            &self.tokens[self.pos + offset]
        }
    }

    fn current(&self) -> &Token {
        self.peak(0)
    }

    fn consume(&mut self) -> Token {
        let result = self.current().clone();
        self.advance();
        result
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn parse_int_literal(&mut self) -> ASTNode {
        ASTNode::IntegerLiteralExpression(self.consume())
    }

    fn parse_declaration(&mut self) -> ASTNode {
        static EXPECTED_SEQUENCE: [TokenType; 5] = [
            TokenType::Type,
            TokenType::Identifier,
            TokenType::Equals,
            TokenType::IntegerLiteral,
            TokenType::SemiColon,
        ];

        self.expect_token_sequence(&EXPECTED_SEQUENCE);

        let type_token = self.consume();
        let identifier = self.consume();
        self.advance(); // Equals
        let literal = self.parse_int_literal();
        self.advance(); // SemiColon

        ASTNode::Declaration(type_token, identifier, Box::new(literal))
    }

    fn parse_return_statement(&mut self) -> ASTNode {
        static EXPECTED_SEQUENCE: [TokenType; 3] = [
            TokenType::Return,
            TokenType::IntegerLiteral,
            TokenType::SemiColon,
        ];

        self.expect_token_sequence(&EXPECTED_SEQUENCE);

        let return_keyword = self.consume();
        let literal = self.parse_int_literal();
        self.advance(); // SemiColon

        ASTNode::ReturnStatement(return_keyword, Box::new(literal))
    }

    fn expect_token_sequence(&self, types: &[TokenType]) {
        for (i, token_type) in types.iter().enumerate() {
            if self.peak(i).token_type != *token_type {
                panic!(
                    "Parser: Expected {:?}, got {:?}",
                    token_type,
                    self.peak(i).token_type
                )
            }
        }
    }

    fn EOF_token(&self) -> &Token {
        return self.tokens.last().unwrap();
    }
}

#[cfg(test)]
mod tests {

    use super::{ASTNode, Parser, Token, TokenType};
    use crate::lexer::Lexer;

    #[rstest::rstest]
    #[case("int x = 55;", ASTNode::Program(
        vec![ASTNode::Declaration(
            Token{value: "int".to_string(), token_type: TokenType::Type, pos: 0},
            Token{value: "x".to_string(), token_type: TokenType::Identifier, pos: 4},
            Box::new(ASTNode::IntegerLiteralExpression(
                Token{value: "55".to_string(), token_type: TokenType::IntegerLiteral, pos: 8}
            ))
        )])
    )]
    fn test_parse_basic_declaration(#[case] test_case: String, #[case] expected: ASTNode) {
        let tokens = Lexer::new(test_case.clone()).lex();
        let result = Parser::new(tokens).parse();
        assert_eq!(expected, result);
    }

    #[rstest::rstest]
    #[case("return 123;", ASTNode::Program(
        vec![ASTNode::ReturnStatement(
            Token{value: "return".to_string(), token_type: TokenType::Return, pos: 0},
            Box::new(ASTNode::IntegerLiteralExpression(
                    Token{value: "123".to_string(), token_type: TokenType::IntegerLiteral, pos: 7}
            ))
        )])
    )]
    fn test_parse_return_statement(#[case] test_case: String, #[case] expected: ASTNode) {
        let tokens = Lexer::new(test_case).lex();
        let result = Parser::new(tokens).parse();
        assert_eq!(expected, result);
    }
}
