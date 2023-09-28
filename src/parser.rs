use crate::ast::ASTNode::ExpressionNode;
use crate::ast::*;
use crate::tokens::*;
use phf::phf_map;
use std::thread::current;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

fn operator_precedence(token_type: TokenType) -> u8 {
    match token_type {
        TokenType::BarBar => 1,
        TokenType::AndAnd => 2,
        TokenType::Bar => 3,
        TokenType::Caret => 4,
        TokenType::And => 5,
        TokenType::Plus | TokenType::Minus => 6,
        TokenType::Star | TokenType::Slash => 7,
        _ => 0,
    }
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn parse_unit(&mut self) -> ASTNode {
        let c = self.current();
        if c.token_type == TokenType::OpenCurly {
            self.parse_scope()
        } else {
            self.parse_statement()
        }
    }

    pub fn parse(&mut self) -> ASTNode {
        let mut result = Vec::new();
        loop {
            let c = self.current();
            if c.token_type == TokenType::Eof {
                break;
            }
            result.push(self.parse_unit());
        }
        ASTNode::Program(result)
    }

    fn parse_scope(&mut self) -> ASTNode {
        let mut result = Vec::new();
        self.try_consume(TokenType::OpenCurly);
        while self.current().token_type != TokenType::CloseCurly
            && self.current().token_type != TokenType::Eof
        {
            result.push(self.parse_unit());
        }
        self.try_consume(TokenType::CloseCurly);
        ASTNode::Scope(result)
    }

    fn parse_statement(&mut self) -> ASTNode {
        match self.current().token_type {
            TokenType::Type => self.parse_declaration(),
            TokenType::Return => self.parse_return_statement(),
            TokenType::Identifier => self.parse_assignment(),
            TokenType::If => self.parse_if(),
            TokenType::While => self.parse_while(),
            TokenType::Do => self.parse_do_while(),
            _ => panic!("Unexpected token: {:?}", self.current()),
        }
    }

    fn parse_do_while(&mut self) -> ASTNode {
        let do_token = self.try_consume(TokenType::Do);
        let body = self.parse_scope_or_single_statement();
        let while_token = self.try_consume(TokenType::While);
        let condition = self.parse_parenthesized_expression();
        self.try_consume(TokenType::SemiColon);
        ASTNode::DoWhile(
            do_token,
            Box::new(body),
            while_token,
            Box::new(ExpressionNode(condition)),
        )
    }

    fn parse_while(&mut self) -> ASTNode {
        let while_token = self.try_consume(TokenType::While);
        let condition = self.parse_parenthesized_expression();
        let body = self.parse_scope_or_single_statement();
        ASTNode::While(
            while_token,
            Box::new(ExpressionNode(condition)),
            Box::new(body),
        )
    }

    fn parse_scope_or_single_statement(&mut self) -> ASTNode {
        if self.current().token_type == TokenType::OpenCurly {
            self.parse_scope()
        } else {
            ASTNode::Scope(vec![self.parse_statement()])
        }
    }

    fn parse_if(&mut self) -> ASTNode {
        let if_token = self.try_consume(TokenType::If);
        let condition = self.parse_parenthesized_expression();
        let body = self.parse_scope_or_single_statement();

        let mut else_body: Option<Box<ASTNode>> = None;

        if self.current().token_type == TokenType::Else {
            self.advance();
            let body = self.parse_unit();

            else_body = if let ASTNode::Scope(_) = body {
                Some(Box::new(body))
            } else {
                Some(Box::new(ASTNode::Scope(vec![body])))
            };
        }

        ASTNode::If(
            if_token,
            Box::new(ASTNode::ExpressionNode(condition)),
            Box::new(body),
            else_body,
        )
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

    fn parse_expression(&mut self) -> Expression {
        self.parse_expression_internal(0)
    }

    fn parse_expression_internal(&mut self, parent_precedence: u8) -> Expression {
        let mut left = self.parse_primary_expression();
        loop {
            let operator_token = self.current().clone();
            let operator_precedence = operator_precedence(operator_token.token_type.clone());
            if parent_precedence >= operator_precedence {
                //  Because parent_precedence >= 0, this
                // condition is satisfied too if the current
                // token is not an operator, e.g. ; or ).
                break;
            }
            self.advance();
            let right = self.parse_expression_internal(operator_precedence);
            left = Expression::Binary(operator_token, Box::new(left), Box::new(right))
        }
        left
    }

    fn parse_parenthesized_expression(&mut self) -> Expression {
        self.try_consume(TokenType::OpenParen);
        let expr = self.parse_expression();
        self.try_consume(TokenType::CloseParen);
        expr
    }

    fn parse_primary_expression(&mut self) -> Expression {
        match self.current().token_type {
            TokenType::Identifier => Expression::Variable(self.consume()),
            TokenType::IntegerLiteral => Expression::IntegerLiteral(self.consume()),
            TokenType::OpenParen => self.parse_parenthesized_expression(),
            _ => panic!("Unexpected token: {:?}", self.current()),
        }
    }

    fn parse_assignment(&mut self) -> ASTNode {
        let identifier_token = self.try_consume(TokenType::Identifier);
        self.try_consume(TokenType::Equals);
        let expression = self.parse_expression();
        self.try_consume(TokenType::SemiColon);
        ASTNode::Assignment(identifier_token, Box::new(ExpressionNode(expression)))
    }

    fn parse_int_literal(&mut self) -> ASTNode {
        ASTNode::ExpressionNode(Expression::IntegerLiteral(self.consume()))
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

        ASTNode::ReturnStatement(return_keyword, Box::new(ExpressionNode(expression)))
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::ast::ASTNode::*;
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
                    Expression::IntegerLiteral(
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
            Box::new(ASTNode::ExpressionNode(Expression::IntegerLiteral(
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
    #[case("return 1 ^ 2;", ASTNode::Program(
        vec![ReturnStatement(
            Token{value: "return".to_string(), token_type: TokenType::Return, pos: 0},
            Box::new(ExpressionNode(
                Expression::Binary(
                    Token{value: "^".to_string(), token_type: TokenType::Caret, pos: 9},
                    Box::new(Expression::IntegerLiteral(
                        Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 7}
                    )),
                    Box::new(Expression::IntegerLiteral(
                        Token{value: "2".to_string(), token_type: TokenType::IntegerLiteral, pos: 11}
                    ))
                )
            ))
        )])
    )]
    #[case("return 1 + 2 * 3;", ASTNode::Program(
        vec![ReturnStatement(
            Token{value: "return".to_string(), token_type: TokenType::Return, pos: 0},
            Box::new(ExpressionNode(
                Expression::Binary(
                    Token{value: "+".to_string(), token_type: TokenType::Plus, pos: 9},
                    Box::new(Expression::IntegerLiteral(
                        Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 7}
                    )),
                    Box::new(Expression::Binary(
                        Token{value: "*".to_string(), token_type: TokenType::Star, pos: 13},
                        Box::new(Expression::IntegerLiteral(
                            Token{value: "2".to_string(), token_type: TokenType::IntegerLiteral, pos: 11}
                        )),
                        Box::new(Expression::IntegerLiteral(
                            Token{value: "3".to_string(), token_type: TokenType::IntegerLiteral, pos: 15}
                        ))
                    ))
                )
            ))
        )])
    )]
    #[case("return 1 || x * 3;", ASTNode::Program(
        vec![ReturnStatement(
            Token{value: "return".to_string(), token_type: TokenType::Return, pos: 0},
                Box::new(ExpressionNode(
                    Expression::Binary(
                        Token{value: "||".to_string(), token_type: TokenType::BarBar, pos: 9},
                        Box::new(Expression::IntegerLiteral(
                            Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 7}
                        )),
                        Box::new(Expression::Binary(
                            Token{value: "*".to_string(), token_type: TokenType::Star, pos: 14},
                            Box::new(Expression::Variable(
                                Token{value: "x".to_string(), token_type: TokenType::Identifier, pos: 12}
                            )),
                            Box::new(Expression::IntegerLiteral(
                                Token{value: "3".to_string(), token_type: TokenType::IntegerLiteral, pos: 16}
                            ))
                        ))
                    )
                ))
            )
        ])
    )]
    #[case("{ return 1 && x * 3; }", ASTNode::Program(vec![ASTNode::Scope(
        vec![ReturnStatement(
            Token{value: "return".to_string(), token_type: TokenType::Return, pos: 2},
                Box::new(ExpressionNode(
                    Expression::Binary(
                        Token{value: "&&".to_string(), token_type: TokenType::AndAnd, pos: 11},
                        Box::new(Expression::IntegerLiteral(
                            Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 9}
                        )),
                        Box::new(Expression::Binary(
                            Token{value: "*".to_string(), token_type: TokenType::Star, pos: 16},
                            Box::new(Expression::Variable(
                                Token{value: "x".to_string(), token_type: TokenType::Identifier, pos: 14}
                            )),
                            Box::new(Expression::IntegerLiteral(
                                Token{value: "3".to_string(), token_type: TokenType::IntegerLiteral, pos: 18}
                            ))
                        ))
                    )
                ))
            )
        ])]
    ))]
    fn test_parse_binary_expression(#[case] test_case: String, #[case] expected: ASTNode) {
        let tokens = Lexer::new(test_case).lex();
        let result = Parser::new(tokens).parse();
        assert_eq!(expected, result);
    }

    #[rstest::rstest]
    #[case("if (true) { return 1; }", ASTNode::Program(vec![ASTNode::If(
        Token { value: "if".to_string(), token_type: TokenType::If, pos: 0 },
        Box::new(ExpressionNode(Expression::Variable(
            Token{value: "true".to_string(), token_type: TokenType::Identifier, pos: 4}
        ))),
        Box::new(ASTNode::Scope(vec![ASTNode::ReturnStatement(
            Token{value: "return".to_string(), token_type: TokenType::Return, pos: 12},
            Box::new(ASTNode::ExpressionNode(Expression::IntegerLiteral(
                Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 19}
            )))
        )])),
        None
    )]))]
    fn test_parse_if_statement(#[case] test_case: String, #[case] expected: ASTNode) {
        let tokens = Lexer::new(test_case).lex();
        let result = Parser::new(tokens).parse();
        assert_eq!(expected, result);
    }

    #[rstest::rstest]
    #[case("if (ture) { return 1; } else { return 2; }", ASTNode::Program(vec![ASTNode::If(
        Token { value: "if".to_string(), token_type: TokenType::If, pos: 0 },
        Box::new(ASTNode::ExpressionNode(Expression::Variable(
            Token{value: "ture".to_string(), token_type: TokenType::Identifier, pos: 4}
        ))),
        Box::new(ASTNode::Scope(vec![ASTNode::ReturnStatement(
            Token{value: "return".to_string(), token_type: TokenType::Return, pos: 12},
            Box::new(ExpressionNode(Expression::IntegerLiteral(
                Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 19}
            )))
        )])),
        Some(Box::new(ASTNode::Scope(vec![ASTNode::ReturnStatement(
            Token{value: "return".to_string(), token_type: TokenType::Return, pos: 31},
            Box::new(ExpressionNode(Expression::IntegerLiteral(
                Token{value: "2".to_string(), token_type: TokenType::IntegerLiteral, pos: 38}
            )))
        )])))
    )]))]
    fn test_parse_if_else(#[case] test_case: String, #[case] expected: ASTNode) {
        let tokens = Lexer::new(test_case).lex();
        let result = Parser::new(tokens).parse();
        assert_eq!(expected, result);
    }

    #[rstest::rstest]
    #[case("if (ture) { return 1; } else if (false) { return 2; }", ASTNode::Program(vec![ASTNode::If(
        Token { value: "if".to_string(), token_type: TokenType::If, pos: 0 },
        Box::new(ASTNode::ExpressionNode(Expression::Variable(
            Token{value: "ture".to_string(), token_type: TokenType::Identifier, pos: 4}
        ))),
        Box::new(ASTNode::Scope(vec![ASTNode::ReturnStatement(
            Token{value: "return".to_string(), token_type: TokenType::Return, pos: 12},
            Box::new(ExpressionNode(Expression::IntegerLiteral(
                Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 19}
            )))
        )])),
        Some(Box::new(ASTNode::Scope(vec![ASTNode::If(
            Token { value: "if".to_string(), token_type: TokenType::If, pos: 29 },
            Box::new(ASTNode::ExpressionNode(Expression::Variable(
                Token{value: "false".to_string(), token_type: TokenType::Identifier, pos: 33}
            ))),
            Box::new(ASTNode::Scope(vec![ASTNode::ReturnStatement(
                Token{value: "return".to_string(), token_type: TokenType::Return, pos: 42},
                Box::new(ExpressionNode(Expression::IntegerLiteral(
                    Token{value: "2".to_string(), token_type: TokenType::IntegerLiteral, pos: 49}
                )))
            )])),
            None
        )])))
    )]))]
    fn test_parse_if_else_if(#[case] test_case: String, #[case] expected: ASTNode) {
        let tokens = Lexer::new(test_case).lex();
        let result = Parser::new(tokens).parse();
        assert_eq!(expected, result);
    }

    #[rstest::rstest]
    #[case("if (ture) { return 1; } else if (false) { return 2; } else { return 3; }", ASTNode::Program(vec![ASTNode::If(
        Token { value: "if".to_string(), token_type: TokenType::If, pos: 0 },
        Box::new(ASTNode::ExpressionNode(Expression::Variable(
            Token{value: "ture".to_string(), token_type: TokenType::Identifier, pos: 4}
        ))),
        Box::new(ASTNode::Scope(vec![ASTNode::ReturnStatement(
            Token{value: "return".to_string(), token_type: TokenType::Return, pos: 12},
            Box::new(ExpressionNode(Expression::IntegerLiteral(
                Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 19}
            )))
        )])),
        Some(Box::new(ASTNode::Scope(vec![ASTNode::If(
            Token { value: "if".to_string(), token_type: TokenType::If, pos: 29 },
            Box::new(ASTNode::ExpressionNode(Expression::Variable(
                Token{value: "false".to_string(), token_type: TokenType::Identifier, pos: 33}
            ))),
            Box::new(ASTNode::Scope(vec![ASTNode::ReturnStatement(
                Token{value: "return".to_string(), token_type: TokenType::Return, pos: 42},
                Box::new(ExpressionNode(Expression::IntegerLiteral(
                    Token{value: "2".to_string(), token_type: TokenType::IntegerLiteral, pos: 49}
                )))
            )])),
            Some(Box::new(ASTNode::Scope(vec![ASTNode::ReturnStatement(
                Token{value: "return".to_string(), token_type: TokenType::Return, pos: 61},
                Box::new(ExpressionNode(Expression::IntegerLiteral(
                    Token{value: "3".to_string(), token_type: TokenType::IntegerLiteral, pos: 68}
                )))
            )])))
        )])))
    )]))]
    fn test_parse_if_with_multiple_elses(#[case] test_case: String, #[case] expected: ASTNode) {
        let tokens = Lexer::new(test_case).lex();
        let result = Parser::new(tokens).parse();
        assert_eq!(expected, result);
    }

    #[rstest::rstest]
    #[case("if (ture) return 1; else if (false) return 2; else { return 3; }", ASTNode::Program(vec![ASTNode::If(
        Token { value: "if".to_string(), token_type: TokenType::If, pos: 0 },
        Box::new(ASTNode::ExpressionNode(Expression::Variable(
            Token{value: "ture".to_string(), token_type: TokenType::Identifier, pos: 4}
        ))),
        Box::new(ASTNode::Scope(vec![ASTNode::ReturnStatement(
            Token{value: "return".to_string(), token_type: TokenType::Return, pos: 10},
            Box::new(ExpressionNode(Expression::IntegerLiteral(
                Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 17}
            )))
        )])),
        Some(Box::new(ASTNode::Scope(vec![ASTNode::If(
            Token { value: "if".to_string(), token_type: TokenType::If, pos: 25 },
            Box::new(ASTNode::ExpressionNode(Expression::Variable(
                Token{value: "false".to_string(), token_type: TokenType::Identifier, pos: 29}
            ))),
            Box::new(ASTNode::Scope(vec![ASTNode::ReturnStatement(
                Token{value: "return".to_string(), token_type: TokenType::Return, pos: 36},
                Box::new(ExpressionNode(Expression::IntegerLiteral(
                    Token{value: "2".to_string(), token_type: TokenType::IntegerLiteral, pos: 43}
                )))
            )])),
            Some(Box::new(ASTNode::Scope(vec![ASTNode::ReturnStatement(
                Token{value: "return".to_string(), token_type: TokenType::Return, pos: 53},
                Box::new(ExpressionNode(Expression::IntegerLiteral(
                    Token{value: "3".to_string(), token_type: TokenType::IntegerLiteral, pos: 60}
                )))
            )])))
        )])))
    )]))]
    fn test_parse_if_with_multiple_elses_no_braces(
        #[case] test_case: String,
        #[case] expected: ASTNode,
    ) {
        let tokens = Lexer::new(test_case).lex();
        let result = Parser::new(tokens).parse();
        assert_eq!(expected, result);
    }

    #[rstest::rstest]
    #[case("while (true) { return 1; }", ASTNode::Program(vec![ASTNode::While(
        Token { value: "while".to_string(), token_type: TokenType::While, pos: 0 },
        Box::new(ASTNode::ExpressionNode(Expression::Variable(
            Token{value: "true".to_string(), token_type: TokenType::Identifier, pos: 7}
        ))),
        Box::new(ASTNode::Scope(vec![ASTNode::ReturnStatement(
            Token{value: "return".to_string(), token_type: TokenType::Return, pos: 15},
            Box::new(ExpressionNode(Expression::IntegerLiteral(
                Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 22}
            )))
        )]))
    )]))]
    #[case("while (true) return 1;", ASTNode::Program(vec![ASTNode::While(
        Token { value: "while".to_string(), token_type: TokenType::While, pos: 0 },
        Box::new(ASTNode::ExpressionNode(Expression::Variable(
            Token{value: "true".to_string(), token_type: TokenType::Identifier, pos: 7}
        ))),
        Box::new(ASTNode::Scope(vec![ASTNode::ReturnStatement(
            Token{value: "return".to_string(), token_type: TokenType::Return, pos: 13},
            Box::new(ExpressionNode(Expression::IntegerLiteral(
                Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 20}
            )))
        )]))
    )]))]
    fn test_parse_while_statement(#[case] test_case: String, #[case] expected: ASTNode) {
        let tokens = Lexer::new(test_case).lex();
        let result = Parser::new(tokens).parse();
        assert_eq!(expected, result);
    }

    #[rstest::rstest]
    #[case("int x = 1; do { x = x + 1; } while (x);", ASTNode::Program(vec![
        ASTNode::Declaration(
            Token{value: "int".to_string(), token_type: TokenType::Type, pos: 0},
            Token{value: "x".to_string(), token_type: TokenType::Identifier, pos: 4}
        ),
        ASTNode::Assignment(
            Token{value: "x".to_string(), token_type: TokenType::Identifier, pos: 4},
            Box::new(ASTNode::ExpressionNode(Expression::IntegerLiteral(
                Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 8}
            )))
        ),
        ASTNode::DoWhile(
            Token{value: "do".to_string(), token_type: TokenType::Do, pos: 11},
            Box::new(ASTNode::Scope(vec![ASTNode::Assignment(
                Token{value: "x".to_string(), token_type: TokenType::Identifier, pos: 16},
                Box::new(ASTNode::ExpressionNode(Expression::Binary(
                    Token{value: "+".to_string(), token_type: TokenType::Plus, pos: 22},
                    Box::new(Expression::Variable(
                        Token{value: "x".to_string(), token_type: TokenType::Identifier, pos: 20}
                    )),
                    Box::new(Expression::IntegerLiteral(
                        Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 24}
                    ))
                )))
            )])),
            Token{value: "while".to_string(), token_type: TokenType::While, pos: 29},
            Box::new(ExpressionNode(Expression::Variable(
                Token{value: "x".to_string(), token_type: TokenType::Identifier, pos: 36}
            )))
        )
    ]))]
    #[rstest::rstest]
    #[case("int x = 1; do x = x + 1; while (x);", ASTNode::Program(vec![
        ASTNode::Declaration(
            Token{value: "int".to_string(), token_type: TokenType::Type, pos: 0},
            Token{value: "x".to_string(), token_type: TokenType::Identifier, pos: 4}
        ),
        ASTNode::Assignment(
            Token{value: "x".to_string(), token_type: TokenType::Identifier, pos: 4},
            Box::new(ASTNode::ExpressionNode(Expression::IntegerLiteral(
                Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 8}
            )))
        ),
        ASTNode::DoWhile(
            Token{value: "do".to_string(), token_type: TokenType::Do, pos: 11},
            Box::new(ASTNode::Scope(vec![ASTNode::Assignment(
                Token{value: "x".to_string(), token_type: TokenType::Identifier, pos: 14},
                Box::new(ASTNode::ExpressionNode(Expression::Binary(
                    Token{value: "+".to_string(), token_type: TokenType::Plus, pos: 20},
                    Box::new(Expression::Variable(
                        Token{value: "x".to_string(), token_type: TokenType::Identifier, pos: 18}
                    )),
                    Box::new(Expression::IntegerLiteral(
                        Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 22}
                    ))
                )))
            )])),
            Token{value: "while".to_string(), token_type: TokenType::While, pos: 25},
            Box::new(ExpressionNode(Expression::Variable(
                Token{value: "x".to_string(), token_type: TokenType::Identifier, pos: 32}
            )))
        )
    ]))]
    fn test_do_while(#[case] test_case: String, #[case] expected: ASTNode) {
        let tokens = Lexer::new(test_case.clone()).lex();
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
    #[case("else { return 0; }")]
    #[case("if else { return 0; }")]
    #[case("if {1} { return 0; }")]
    #[case("if {1} ( return 0; )")]
    #[case("if if (1) { return 1; } ")]
    #[case("else if (1) { return 1; } ")]
    #[should_panic]
    fn test_wrong_if_statements(#[case] test_case: String) {
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

    #[rstest::rstest]
    #[case("{")]
    #[case("{ int x = 1;")]
    #[case("{ return 1;")]
    #[case(" return 1; }")]
    #[case("{ return 1;")]
    #[case("}")]
    #[should_panic]
    fn test_invalid_scope_syntax(#[case] test_case: String) {
        let tokens = Lexer::new(test_case).lex();
        let _result = Parser::new(tokens).parse();
    }
}
