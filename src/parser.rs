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
            TokenType::Identifier => self.parse_assignment(),
            _ => panic!("Parser: Unexpected token {:?}", self.current()),
        }
    }

    fn peak(&self, offset: usize) -> &Token {
        if (self.pos + offset) >= self.tokens.len() {
            return self.tokens.last().unwrap();
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

    fn parse_expression(&mut self) -> ASTNode {
        match self.current().token_type {
            TokenType::Identifier => {
                ASTNode::ExpressionNode(Expression::VariableExpression(self.consume()))
            }
            TokenType::IntegerLiteral => {
                ASTNode::ExpressionNode(Expression::IntegerLiteralExpression(self.consume()))
            }
            _ => panic!("Unexpected token: {:?}", self.current()),
        }
    }

    fn parse_assignment(&mut self) -> ASTNode {
        let identifier_token = self.try_consume(TokenType::Identifier);
        self.try_consume(TokenType::Equals);
        let expression = self.parse_expression();
        self.try_consume(TokenType::SemiColon);
        ASTNode::Assignment(identifier_token, Box::new(expression))
    }

    fn parse_int_literal(&mut self) -> ASTNode {
        ASTNode::ExpressionNode(Expression::IntegerLiteralExpression(self.consume()))
    }

    fn parse_declaration(&mut self) -> ASTNode {
        let type_token = self.try_consume(TokenType::Type);
        let identifier = self.try_consume(TokenType::Identifier);
        if self.current().token_type == TokenType::Equals {
            // We have a declaration followed by an assignment.
            // The declaration ends here, and we need to parse
            // again starting from the identifier.
            self.pos -= 1;
        } else {
            self.try_consume(TokenType::SemiColon);
        }

        ASTNode::Declaration(type_token, identifier)
    }

    fn try_consume(&mut self, token_type: TokenType) -> Token {
        if self.current().token_type == token_type {
            self.consume()
        } else {
            panic!(
                "Expected {:?}, found: {:?}",
                token_type,
                self.current().token_type
            )
        }
    }

    fn parse_return_statement(&mut self) -> ASTNode {
        let return_keyword = self.try_consume(TokenType::Return);
        let expression = self.parse_expression();
        self.try_consume(TokenType::SemiColon);

        ASTNode::ReturnStatement(return_keyword, Box::new(expression))
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::lexer::Lexer;

    #[rstest::rstest]
    #[case("int x = 55;", ASTNode::Program(
        vec![
            ASTNode::Declaration(
                Token{value: "int".to_string(), token_type: TokenType::Type, pos: 0},
                Token{value: "x".to_string(), token_type: TokenType::Identifier, pos: 4}
            ),
            ASTNode::Assignment(
                Token{value: "x".to_string(), token_type: TokenType::Identifier, pos: 4},
                Box::new(ASTNode::ExpressionNode(
                    Expression::IntegerLiteralExpression(
                    Token{value: "55".to_string(), token_type: TokenType::IntegerLiteral, pos: 8}))
                )
            )
        ])
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
            Box::new(ASTNode::ExpressionNode(Expression::IntegerLiteralExpression(
                    Token{value: "123".to_string(), token_type: TokenType::IntegerLiteral, pos: 7}
            ))
        ))])
    )]
    fn test_parse_return_statement(#[case] test_case: String, #[case] expected: ASTNode) {
        let tokens = Lexer::new(test_case).lex();
        let result = Parser::new(tokens).parse();
        assert_eq!(expected, result);
    }

    #[rstest::rstest]
    #[case("return 123")]
    #[case("int x")]
    #[case("int x = 123")]
    #[should_panic]
    fn test_missing_simicolon(#[case] test_case: String) {
        let tokens = Lexer::new(test_case).lex();
        let _result = Parser::new(tokens).parse();
    }

    #[rstest::rstest]
    #[case("return;")]
    #[case("int x =;")]
    #[should_panic]
    fn test_missing_expression(#[case] test_case: String) {
        let tokens = Lexer::new(test_case).lex();
        let _result = Parser::new(tokens).parse();
    }
}
