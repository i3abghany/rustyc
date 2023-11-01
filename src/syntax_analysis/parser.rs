use crate::lexical_analysis::tokens::*;
use crate::syntax_analysis::ast::ASTNode::*;
use crate::syntax_analysis::ast::*;
use crate::utils::test_utils::interpret_llvm_ir;
use std::panic::panic_any;

pub struct Parser {
    tokens: Vec<Token>,
    terminator_stack: Vec<TokenType>,
    pos: usize,
}

fn binary_operator_precedence(token_type: TokenType) -> u8 {
    match token_type {
        TokenType::BarBar => 1,
        TokenType::AndAnd => 2,
        TokenType::Bar => 3,
        TokenType::Caret => 4,
        TokenType::And => 5,
        TokenType::EqualsEquals | TokenType::NotEquals => 6,
        TokenType::GreaterThan
        | TokenType::GreaterThanEquals
        | TokenType::LessThan
        | TokenType::LessThanEquals => 7,
        TokenType::Plus | TokenType::Minus => 8,
        TokenType::Star | TokenType::Slash => 9,
        _ => 0,
    }
}

fn unary_operator_precedence(token_type: &TokenType) -> u8 {
    match token_type {
        TokenType::Plus | TokenType::Minus | TokenType::Bang => 10,
        _ => 0,
    }
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0,
            terminator_stack: vec![],
        }
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
        TranslationUnit(result)
    }

    fn parse_scope(&mut self) -> ASTNode {
        let mut result = Vec::new();
        self.push_terminator(TokenType::CloseCurly);
        self.try_consume(TokenType::OpenCurly);
        while self.current().token_type != TokenType::CloseCurly
            && self.current().token_type != TokenType::Eof
        {
            result.push(self.parse_unit());
        }
        self.try_consume(TokenType::CloseCurly);
        self.pop_terminator(TokenType::CloseCurly);
        Scope(result)
    }

    fn parse_statement(&mut self) -> ASTNode {
        match self.current().token_type {
            TokenType::Type => self.parse_declaration_or_definition(),
            TokenType::Return => self.parse_return_statement(),
            TokenType::If => self.parse_if(),
            TokenType::While => self.parse_while(),
            TokenType::Do => self.parse_do_while(),
            TokenType::For => self.parse_for(),
            _ => self.parse_assignment_or_expression_statement(),
        }
    }

    fn parse_assignment_or_expression_statement(&mut self) -> ASTNode {
        let res = self.parse_assignment_or_expression_statement_no_semicolon();
        self.try_consume(TokenType::SemiColon);
        res
    }

    fn parse_assignment_or_expression_statement_no_semicolon(&mut self) -> ASTNode {
        if self.current().token_type == TokenType::SemiColon {
            ExpressionStatement(Expression::Empty)
        } else if self.is_assignment() {
            self.parse_assignment_no_semicolon()
        } else {
            ExpressionStatement(self.parse_expression())
        }
    }

    fn is_assignment(&self) -> bool {
        if self.current().token_type != TokenType::Identifier {
            return false;
        }

        let mut i = 1;
        loop {
            let next = self.peak(i);
            if next.token_type == TokenType::Equals {
                return true;
            }
            if next.token_type == TokenType::SemiColon
                || next.token_type == TokenType::Eof
                || self.is_next_terminator(&next.token_type)
            {
                break;
            }
            i += 1;
        }
        false
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

    fn parse_for(&mut self) -> ASTNode {
        let for_token = self.try_consume(TokenType::For);
        self.push_terminator(TokenType::CloseParen);
        self.try_consume(TokenType::OpenParen);

        let init = self.parse_statement();
        let condition = self.parse_assignment_or_expression_statement();

        let mut update = ExpressionStatement(Expression::Empty);
        if self.current().token_type != TokenType::CloseParen {
            update = self.parse_assignment_or_expression_statement_no_semicolon();
        }

        self.try_consume(TokenType::CloseParen);
        self.pop_terminator(TokenType::CloseParen);

        let body = self.parse_scope_or_single_statement();

        ASTNode::For(
            for_token,
            [Box::new(init), Box::new(condition), Box::new(update)],
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
        if self.is_assignment() {
            return self.parse_assignment_expression();
        }
        let mut left: Expression;
        let unary_op_precedence = unary_operator_precedence(&self.current().token_type);
        if unary_op_precedence != 0 && unary_op_precedence >= parent_precedence {
            left = Expression::Unary(
                self.consume().clone(),
                Box::new(self.parse_expression_internal(unary_op_precedence)),
            );
        } else {
            left = self.parse_primary_expression();
        }
        loop {
            let operator_token = self.current().clone();
            let operator_precedence = binary_operator_precedence(operator_token.token_type.clone());
            if parent_precedence >= operator_precedence {
                //  Because parent_precedence >= 0, this
                // condition is satisfied too if the current
                // token is not an operator, e.g. ; or ).
                break;
            }
            self.advance();
            let right = self.parse_expression_internal(operator_precedence);
            left = Expression::Binary(operator_token, Box::new(left), Box::new(right));
        }
        left
    }

    fn parse_parenthesized_expression(&mut self) -> Expression {
        self.try_consume(TokenType::OpenParen);
        self.push_terminator(TokenType::CloseParen);
        let expr = self.parse_expression();
        self.try_consume(TokenType::CloseParen);
        self.pop_terminator(TokenType::CloseParen);
        expr
    }

    fn parse_function_arguments(&mut self) -> Vec<Expression> {
        let mut result = Vec::new();
        self.try_consume(TokenType::OpenParen);
        self.push_terminator(TokenType::CloseParen);
        while self.current().token_type != TokenType::CloseParen
            && self.current().token_type != TokenType::Eof
        {
            result.push(self.parse_expression());
            if self.current().token_type != TokenType::CloseParen {
                self.try_consume(TokenType::Comma);
            }
        }
        self.try_consume(TokenType::CloseParen);
        self.pop_terminator(TokenType::CloseParen);
        result
    }

    fn push_terminator(&mut self, terminator: TokenType) {
        self.terminator_stack.push(terminator);
    }

    fn pop_terminator(&mut self, terminator: TokenType) {
        if self.terminator_stack.pop().unwrap() != terminator {
            panic!();
        }
    }

    fn is_next_terminator(&self, token_type: &TokenType) -> bool {
        !self.terminator_stack.is_empty() && token_type == self.terminator_stack.last().unwrap()
    }

    fn parse_function_call(&mut self) -> Expression {
        let identifier = self.try_consume(TokenType::Identifier);
        let arguments = self.parse_function_arguments();
        Expression::FunctionCall(identifier, arguments)
    }

    fn parse_primary_expression_starting_with_identifier(&mut self) -> Expression {
        if self.peak(1).token_type == TokenType::OpenParen {
            self.parse_function_call()
        } else {
            Expression::Variable(self.consume())
        }
    }

    fn parse_primary_expression(&mut self) -> Expression {
        match self.current().token_type {
            TokenType::Identifier => self.parse_primary_expression_starting_with_identifier(),
            TokenType::IntegerLiteral => Expression::IntegerLiteral(self.consume()),
            TokenType::OpenParen => self.parse_parenthesized_expression(),
            _ => panic!("Unexpected token: {:?}", self.current()),
        }
    }

    fn parse_assignment_expression(&mut self) -> Expression {
        let identifier_token = self.try_consume(TokenType::Identifier);
        self.try_consume(TokenType::Equals);
        let expression = self.parse_expression();
        Expression::Assignment(identifier_token, Box::new(expression))
    }

    fn parse_assignment_no_semicolon(&mut self) -> ASTNode {
        ASTNode::ExpressionStatement(self.parse_assignment_expression())
    }
    fn parse_assignment(&mut self) -> ASTNode {
        let assignment_expression = self.parse_assignment_no_semicolon();
        self.try_consume(TokenType::SemiColon);
        assignment_expression
    }

    fn parse_int_literal(&mut self) -> ASTNode {
        ASTNode::ExpressionNode(Expression::IntegerLiteral(self.consume()))
    }

    fn parse_declaration_or_definition(&mut self) -> ASTNode {
        let result = self.parse_declaration_or_definition_no_semicolon();
        match result {
            FunctionDefinition(..) => {}
            _ => {
                self.try_consume(TokenType::SemiColon);
            }
        }
        result
    }

    fn parse_declaration_or_definition_no_semicolon(&mut self) -> ASTNode {
        let type_token = self.try_consume(TokenType::Type);
        let identifier = self.try_consume(TokenType::Identifier);
        if self.current().token_type == TokenType::Equals {
            self.advance();
            let expression = ExpressionNode(self.parse_expression());
            VariableDefinition(type_token, identifier, Box::new(expression))
        } else if self.current().token_type == TokenType::SemiColon
            || self.current().token_type == TokenType::Comma
            || self.current().token_type == TokenType::CloseParen
        {
            VariableDeclaration(type_token, identifier)
        } else {
            let parameters = self.parse_function_parameters();

            if self.current().token_type == TokenType::SemiColon {
                FunctionDeclaration(type_token, identifier, parameters)
            } else {
                let body = self.parse_scope();
                FunctionDefinition(type_token, identifier, parameters, Box::new(body))
            }
        }
    }

    fn is_declaration(node: &ASTNode) -> bool {
        match node {
            VariableDeclaration(..) | FunctionDeclaration(..) => true,
            _ => false,
        }
    }

    fn parse_function_parameters(&mut self) -> Vec<ASTNode> {
        self.try_consume(TokenType::OpenParen);

        let mut result = Vec::new();

        while self.current().token_type != TokenType::CloseParen
            && self.current().token_type != TokenType::Eof
        {
            let p = self.parse_declaration_or_definition_no_semicolon();
            if Parser::is_declaration(&p) {
                result.push(p);
            } else {
                panic!("Expected parameter declaration, found: {:?}", p)
            }
            if self.current().token_type != TokenType::CloseParen {
                self.try_consume(TokenType::Comma);
            }
        }
        self.try_consume(TokenType::CloseParen);
        result
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
    use crate::lexical_analysis::lexer::Lexer;
    use crate::syntax_analysis::ast::ASTNode::*;

    #[rstest::rstest]
    #[case("int x = 55;", TranslationUnit(
        vec![
            VariableDefinition(
                Token{value: "int".to_string(), token_type: TokenType::Type, pos: 0},
                Token{value: "x".to_string(), token_type: TokenType::Identifier, pos: 4},
                Box::new(ExpressionNode(Expression::IntegerLiteral(
                    Token{value: "55".to_string(), token_type: TokenType::IntegerLiteral, pos: 8})
                ))
            ),
        ])
    )]
    fn test_parse_basic_declaration(#[case] test_case: String, #[case] expected: ASTNode) {
        let tokens = Lexer::new(test_case.clone()).lex();
        let result = Parser::new(tokens).parse();
        assert_eq!(expected, result);
    }

    #[rstest::rstest]
    #[case("return 123;", TranslationUnit(
        vec![ReturnStatement(
            Token{value: "return".to_string(), token_type: TokenType::Return, pos: 0},
            Box::new(ExpressionNode(Expression::IntegerLiteral(
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
    #[case("return 1 ^ 2;", TranslationUnit(
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
    #[case("return 1 + 2 * 3;", TranslationUnit(
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
    #[case("return 1 || x * 3;", TranslationUnit(
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
    #[case("{ return 1 && x * 3; }", TranslationUnit(vec![Scope(
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
        println!("{:#?}", result);
        assert_eq!(expected, result);
    }

    #[rstest::rstest]
    #[case("return 1 != 2;", TranslationUnit(
        vec![ReturnStatement(
            Token{value: "return".to_string(), token_type: TokenType::Return, pos: 0},
            Box::new(ExpressionNode(
                Expression::Binary(
                    Token{value: "!=".to_string(), token_type: TokenType::NotEquals, pos: 9},
                    Box::new(Expression::IntegerLiteral(
                        Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 7}
                    )),
                    Box::new(Expression::IntegerLiteral(
                        Token{value: "2".to_string(), token_type: TokenType::IntegerLiteral, pos: 12}
                    ))
                )
            ))
        )])
    )]
    #[case("return 1 >= 2;", TranslationUnit(
        vec![ReturnStatement(
            Token{value: "return".to_string(), token_type: TokenType::Return, pos: 0},
            Box::new(ExpressionNode(
                Expression::Binary(
                    Token{value: ">=".to_string(), token_type: TokenType::GreaterThanEquals, pos: 9},
                    Box::new(Expression::IntegerLiteral(
                        Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 7}
                    )),
                    Box::new(Expression::IntegerLiteral(
                        Token{value: "2".to_string(), token_type: TokenType::IntegerLiteral, pos: 12}
                    ))
                )
            ))
        )])
    )]
    #[case("return 1 <= 2;", TranslationUnit(
        vec![ReturnStatement(
            Token{value: "return".to_string(), token_type: TokenType::Return, pos: 0},
            Box::new(ExpressionNode(
                Expression::Binary(
                    Token{value: "<=".to_string(), token_type: TokenType::LessThanEquals, pos: 9},
                    Box::new(Expression::IntegerLiteral(
                        Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 7}
                    )),
                    Box::new(Expression::IntegerLiteral(
                        Token{value: "2".to_string(), token_type: TokenType::IntegerLiteral, pos: 12}
                    ))
                )
            ))
        )])
    )]
    #[case("return 1 < 2;", TranslationUnit(
        vec![ReturnStatement(
            Token{value: "return".to_string(), token_type: TokenType::Return, pos: 0},
            Box::new(ExpressionNode(
                Expression::Binary(
                    Token{value: "<".to_string(), token_type: TokenType::LessThan, pos: 9},
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
    #[case("return 1 > 2;", TranslationUnit(
        vec![ReturnStatement(
            Token{value: "return".to_string(), token_type: TokenType::Return, pos: 0},
            Box::new(ExpressionNode(
                Expression::Binary(
                    Token{value: ">".to_string(), token_type: TokenType::GreaterThan, pos: 9},
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
    fn test_comparison_expressions(#[case] test_case: String, #[case] expected: ASTNode) {
        let tokens = Lexer::new(test_case).lex();
        let result = Parser::new(tokens).parse();
        assert_eq!(expected, result);
    }

    #[rstest::rstest]
    #[case("if (true) { return 1; }", TranslationUnit(vec![If(
        Token { value: "if".to_string(), token_type: TokenType::If, pos: 0 },
        Box::new(ExpressionNode(Expression::Variable(
            Token{value: "true".to_string(), token_type: TokenType::Identifier, pos: 4}
        ))),
        Box::new(Scope(vec![ReturnStatement(
            Token{value: "return".to_string(), token_type: TokenType::Return, pos: 12},
            Box::new(ExpressionNode(Expression::IntegerLiteral(
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
    #[
        case("return -123;",
            TranslationUnit(
                vec![
                    ReturnStatement(
                        Token{value: "return".to_string(), token_type: TokenType::Return, pos: 0},
                        Box::new(
                            ExpressionNode(
                                Expression::Unary(
                                    Token{value: "-".to_string(), token_type: TokenType::Minus, pos: 7},
                                    Box::new(
                                        Expression::IntegerLiteral(
                                            Token{value: "123".to_string(), token_type: TokenType::IntegerLiteral, pos: 8}
                                        )
                                    )
                                )
                            )
                        )
                    )
                ]
            )
        )
    ]
    #[case("return -2 + 1;",
        TranslationUnit(
            vec![
                ReturnStatement(
                    Token{value: "return".to_string(), token_type: TokenType::Return, pos: 0},
                    Box::new(
                        ExpressionNode(
                            Expression::Binary(
                                Token{value: "+".to_string(), token_type: TokenType::Plus, pos: 10},
                                Box::new(
                                    Expression::Unary(
                                        Token{value: "-".to_string(), token_type: TokenType::Minus, pos: 7},
                                        Box::new(
                                            Expression::IntegerLiteral(
                                                Token{value: "2".to_string(), token_type: TokenType::IntegerLiteral, pos: 8}
                                            )
                                        )
                                    )
                                ),
                                Box::new(
                                    Expression::IntegerLiteral(
                                        Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 12}
                                    )
                                )
                            )
                        )
                    )
                )
            ]
        )
    )]
    fn test_parse_unary_expression(#[case] test_case: String, #[case] expected: ASTNode) {
        let tokens = Lexer::new(test_case).lex();
        let result = Parser::new(tokens).parse();
        assert_eq!(expected, result);
    }

    #[rstest::rstest]
    #[case("if (ture) { return 1; } else { return 2; }", TranslationUnit(vec![If(
        Token { value: "if".to_string(), token_type: TokenType::If, pos: 0 },
        Box::new(ExpressionNode(Expression::Variable(
            Token{value: "ture".to_string(), token_type: TokenType::Identifier, pos: 4}
        ))),
        Box::new(Scope(vec![ReturnStatement(
            Token{value: "return".to_string(), token_type: TokenType::Return, pos: 12},
            Box::new(ExpressionNode(Expression::IntegerLiteral(
                Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 19}
            )))
        )])),
        Some(Box::new(Scope(vec![ReturnStatement(
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
    #[case("if (ture) { return 1; } else if (false) { return 2; }", TranslationUnit(vec![If(
        Token { value: "if".to_string(), token_type: TokenType::If, pos: 0 },
        Box::new(ExpressionNode(Expression::Variable(
            Token{value: "ture".to_string(), token_type: TokenType::Identifier, pos: 4}
        ))),
        Box::new(Scope(vec![ReturnStatement(
            Token{value: "return".to_string(), token_type: TokenType::Return, pos: 12},
            Box::new(ExpressionNode(Expression::IntegerLiteral(
                Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 19}
            )))
        )])),
        Some(Box::new(Scope(vec![If(
            Token { value: "if".to_string(), token_type: TokenType::If, pos: 29 },
            Box::new(ExpressionNode(Expression::Variable(
                Token{value: "false".to_string(), token_type: TokenType::Identifier, pos: 33}
            ))),
            Box::new(Scope(vec![ReturnStatement(
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
    #[case("if (ture) { return 1; } else if (false) { return 2; } else { return 3; }", TranslationUnit(vec![If(
        Token { value: "if".to_string(), token_type: TokenType::If, pos: 0 },
        Box::new(ExpressionNode(Expression::Variable(
            Token{value: "ture".to_string(), token_type: TokenType::Identifier, pos: 4}
        ))),
        Box::new(Scope(vec![ReturnStatement(
            Token{value: "return".to_string(), token_type: TokenType::Return, pos: 12},
            Box::new(ExpressionNode(Expression::IntegerLiteral(
                Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 19}
            )))
        )])),
        Some(Box::new(Scope(vec![If(
            Token { value: "if".to_string(), token_type: TokenType::If, pos: 29 },
            Box::new(ExpressionNode(Expression::Variable(
                Token{value: "false".to_string(), token_type: TokenType::Identifier, pos: 33}
            ))),
            Box::new(Scope(vec![ReturnStatement(
                Token{value: "return".to_string(), token_type: TokenType::Return, pos: 42},
                Box::new(ExpressionNode(Expression::IntegerLiteral(
                    Token{value: "2".to_string(), token_type: TokenType::IntegerLiteral, pos: 49}
                )))
            )])),
            Some(Box::new(Scope(vec![ReturnStatement(
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
    #[case("if (ture) return 1; else if (false) return 2; else { return 3; }", TranslationUnit(vec![If(
        Token { value: "if".to_string(), token_type: TokenType::If, pos: 0 },
        Box::new(ExpressionNode(Expression::Variable(
            Token{value: "ture".to_string(), token_type: TokenType::Identifier, pos: 4}
        ))),
        Box::new(Scope(vec![ReturnStatement(
            Token{value: "return".to_string(), token_type: TokenType::Return, pos: 10},
            Box::new(ExpressionNode(Expression::IntegerLiteral(
                Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 17}
            )))
        )])),
        Some(Box::new(Scope(vec![If(
            Token { value: "if".to_string(), token_type: TokenType::If, pos: 25 },
            Box::new(ExpressionNode(Expression::Variable(
                Token{value: "false".to_string(), token_type: TokenType::Identifier, pos: 29}
            ))),
            Box::new(Scope(vec![ReturnStatement(
                Token{value: "return".to_string(), token_type: TokenType::Return, pos: 36},
                Box::new(ExpressionNode(Expression::IntegerLiteral(
                    Token{value: "2".to_string(), token_type: TokenType::IntegerLiteral, pos: 43}
                )))
            )])),
            Some(Box::new(Scope(vec![ReturnStatement(
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
    #[case("while (true) { return 1; }", TranslationUnit(vec![While(
        Token { value: "while".to_string(), token_type: TokenType::While, pos: 0 },
        Box::new(ExpressionNode(Expression::Variable(
            Token{value: "true".to_string(), token_type: TokenType::Identifier, pos: 7}
        ))),
        Box::new(Scope(vec![ReturnStatement(
            Token{value: "return".to_string(), token_type: TokenType::Return, pos: 15},
            Box::new(ExpressionNode(Expression::IntegerLiteral(
                Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 22}
            )))
        )]))
    )]))]
    #[case("while (true) return 1;", TranslationUnit(vec![While(
        Token { value: "while".to_string(), token_type: TokenType::While, pos: 0 },
        Box::new(ExpressionNode(Expression::Variable(
            Token{value: "true".to_string(), token_type: TokenType::Identifier, pos: 7}
        ))),
        Box::new(Scope(vec![ReturnStatement(
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
    #[case("int x = 1; do { x = x + 1; } while (x);", TranslationUnit(vec![
        VariableDefinition(
            Token{value: "int".to_string(), token_type: TokenType::Type, pos: 0},
            Token{value: "x".to_string(), token_type: TokenType::Identifier, pos: 4},
            Box::new(ExpressionNode(Expression::IntegerLiteral(
                Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 8}
            )))
        ),
        DoWhile(
            Token{value: "do".to_string(), token_type: TokenType::Do, pos: 11},
            Box::new(Scope(vec![ExpressionStatement(Expression::Assignment(
            Token{value: "x".to_string(), token_type: TokenType::Identifier, pos: 16},
                Box::new(Expression::Binary(
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
    #[case("int x = 1; do x = x + 1; while (x);", TranslationUnit(vec![
        VariableDefinition(
            Token{value: "int".to_string(), token_type: TokenType::Type, pos: 0},
            Token{value: "x".to_string(), token_type: TokenType::Identifier, pos: 4},
            Box::new(ExpressionNode(Expression::IntegerLiteral(
                Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 8}
            )))
        ),
        DoWhile(
            Token{value: "do".to_string(), token_type: TokenType::Do, pos: 11},
            Box::new(Scope(vec![ExpressionStatement(Expression::Assignment(
                Token{value: "x".to_string(), token_type: TokenType::Identifier, pos: 14},
                Box::new(Expression::Binary(
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
    #[case("for (;;);", TranslationUnit(vec![For(
        Token{value: "for".to_string(), token_type: TokenType::For, pos: 0},
        [
            Box::new(ExpressionStatement(Expression::Empty)),
            Box::new(ExpressionStatement(Expression::Empty)),
            Box::new(ExpressionStatement(Expression::Empty)),
        ],
        Box::new(Scope(vec![ExpressionStatement(Expression::Empty)]))
    )]))]
    #[case("for (int i;;) { ;; }", TranslationUnit(vec![For(
        Token{value: "for".to_string(), token_type: TokenType::For, pos: 0},
        [
            Box::new(VariableDeclaration(
                Token{value: "int".to_string(), token_type: TokenType::Type, pos: 5},
                Token{value: "i".to_string(), token_type: TokenType::Identifier, pos: 9}
            )),
            Box::new(ExpressionStatement(Expression::Empty)),
            Box::new(ExpressionStatement(Expression::Empty)),
        ],
        Box::new(Scope(vec![ExpressionStatement(Expression::Empty), ExpressionStatement(Expression::Empty)]))
    )]))]
    #[case("for (int i = 1;;) {}", TranslationUnit(vec![For(
        Token{value: "for".to_string(), token_type: TokenType::For, pos: 0},
        [
            Box::new(VariableDefinition(
                Token{value: "int".to_string(), token_type: TokenType::Type, pos: 5},
                Token{value: "i".to_string(), token_type: TokenType::Identifier, pos: 9},
                Box::new(ExpressionNode(Expression::IntegerLiteral(
                    Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 13},
                ))),
            )),
            Box::new(ExpressionStatement(Expression::Empty)),
            Box::new(ExpressionStatement(Expression::Empty)),
        ],
        Box::new(Scope(vec![]))
    )]))]
    #[case("for (; i < 10;) {}", TranslationUnit(vec![For(
        Token{value: "for".to_string(), token_type: TokenType::For, pos: 0},
        [
            Box::new(ExpressionStatement(Expression::Empty)),
            Box::new(ExpressionStatement(Expression::Binary(
                Token{value: "<".to_string(), token_type: TokenType::LessThan, pos: 9},
                Box::new(Expression::Variable(
                    Token{value: "i".to_string(), token_type: TokenType::Identifier, pos: 7}
                )),
                Box::new(Expression::IntegerLiteral(
                    Token{value: "10".to_string(), token_type: TokenType::IntegerLiteral, pos: 11}
                ))
            ))),
            Box::new(ExpressionStatement(Expression::Empty)),
        ],
        Box::new(Scope(vec![]))
    )]))]
    #[case("for (; i = 1;) {}", TranslationUnit(vec![For(
        Token{value: "for".to_string(), token_type: TokenType::For, pos: 0},
        [
            Box::new(ExpressionStatement(Expression::Empty)),
            Box::new(ExpressionStatement(Expression::Assignment(
                Token{value: "i".to_string(), token_type: TokenType::Identifier, pos: 7},
                Box::new(Expression::IntegerLiteral(
                    Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 11}
                ))
            ))),
            Box::new(ExpressionStatement(Expression::Empty)),
        ],
        Box::new(Scope(vec![]))
    )]))]
    #[case("for (;; i) {}", TranslationUnit(vec![For(
        Token{value: "for".to_string(), token_type: TokenType::For, pos: 0},
        [
            Box::new(ExpressionStatement(Expression::Empty)),
            Box::new(ExpressionStatement(Expression::Empty)),
            Box::new(ExpressionStatement(Expression::Variable(
                Token{value: "i".to_string(), token_type: TokenType::Identifier, pos: 8},
            ))),
        ],
        Box::new(Scope(vec![]))
    )]))]
    #[case("for (;; i = i + 1) {}", TranslationUnit(vec![For(
        Token{value: "for".to_string(), token_type: TokenType::For, pos: 0},
        [
            Box::new(ExpressionStatement(Expression::Empty)),
            Box::new(ExpressionStatement(Expression::Empty)),
            Box::new(ExpressionStatement(Expression::Assignment(
                Token{value: "i".to_string(), token_type: TokenType::Identifier, pos: 8},
                Box::new(Expression::Binary(
                    Token{value: "+".to_string(), token_type: TokenType::Plus, pos: 14},
                    Box::new(Expression::Variable(
                        Token{value: "i".to_string(), token_type: TokenType::Identifier, pos: 12}
                    )),
                    Box::new(Expression::IntegerLiteral(
                        Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 16}
                    ))
                ))
            ))),
        ],
        Box::new(Scope(vec![]))
    )]))]
    #[case("for (int i = 1; i < 10; i = i + 1) {}", TranslationUnit(vec![For(
        Token{value: "for".to_string(), token_type: TokenType::For, pos: 0},
        [
            Box::new(VariableDefinition(
                Token{value: "int".to_string(), token_type: TokenType::Type, pos: 5},
                Token{value: "i".to_string(), token_type: TokenType::Identifier, pos: 9},
                Box::new(ExpressionNode(Expression::IntegerLiteral(
                    Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 13}
                )))
            )),
            Box::new(ExpressionStatement(Expression::Binary(
                Token{value: "<".to_string(), token_type: TokenType::LessThan, pos: 18},
                Box::new(Expression::Variable(
                    Token{value: "i".to_string(), token_type: TokenType::Identifier, pos: 16}
                )),
                Box::new(Expression::IntegerLiteral(
                    Token{value: "10".to_string(), token_type: TokenType::IntegerLiteral, pos: 20}
                ))
            ))),
            Box::new(ExpressionStatement(Expression::Assignment(
                Token{value: "i".to_string(), token_type: TokenType::Identifier, pos: 24},
                Box::new(Expression::Binary(
                    Token{value: "+".to_string(), token_type: TokenType::Plus, pos: 30},
                    Box::new(Expression::Variable(
                        Token{value: "i".to_string(), token_type: TokenType::Identifier, pos: 28}
                    )),
                    Box::new(Expression::IntegerLiteral(
                        Token{value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 32}
                    ))
                ))
            ))),
        ],
        Box::new(Scope(vec![]))
    )]))]
    fn test_parse_for_statement(#[case] test_case: String, #[case] expected: ASTNode) {
        let tokens = Lexer::new(test_case.clone()).lex();
        let result = Parser::new(tokens).parse();
        assert_eq!(expected, result);
    }

    #[rstest::rstest]
    #[case("int func();", TranslationUnit(vec![
        FunctionDeclaration(
            Token { value: "int".to_string(), token_type: TokenType::Type, pos: 0 },
            Token { value: "func".to_string(), token_type: TokenType::Identifier, pos: 4 },
            vec![]
        )
    ]))]
    #[case("int func(int x);", TranslationUnit(vec![
    FunctionDeclaration(
            Token { value: "int".to_string(), token_type: TokenType::Type, pos: 0 },
            Token { value: "func".to_string(), token_type: TokenType::Identifier, pos: 4 },
            vec![
                VariableDeclaration(
                    Token { value: "int".to_string(), token_type: TokenType::Type, pos: 9 },
                    Token { value: "x".to_string(), token_type: TokenType::Identifier, pos: 13 },
                )
            ]
        )
    ]))]
    #[case("int func(int x, int y);", TranslationUnit(vec![
    FunctionDeclaration(
            Token { value: "int".to_string(), token_type: TokenType::Type, pos: 0 },
            Token { value: "func".to_string(), token_type: TokenType::Identifier, pos: 4 },
            vec![
                VariableDeclaration(
                    Token { value: "int".to_string(), token_type: TokenType::Type, pos: 9 },
                    Token { value: "x".to_string(), token_type: TokenType::Identifier, pos: 13 },
                ),
                VariableDeclaration(
                    Token { value: "int".to_string(), token_type: TokenType::Type, pos: 16 },
                    Token { value: "y".to_string(), token_type: TokenType::Identifier, pos: 20 },
                )
            ]
        )
    ]))]
    fn test_function_definition(#[case] test_case: String, #[case] expected: ASTNode) {
        let tokens = Lexer::new(test_case.clone()).lex();
        let result = Parser::new(tokens).parse();
        assert_eq!(expected, result);
    }

    #[rstest::rstest]
    #[case("int func();", TranslationUnit(vec![
        FunctionDeclaration(
            Token { value: "int".to_string(), token_type: TokenType::Type, pos: 0 },
            Token { value: "func".to_string(), token_type: TokenType::Identifier, pos: 4 },
            vec![]
        )
    ]))]
    #[case("int func(int x);", TranslationUnit(vec![
    FunctionDeclaration(
            Token { value: "int".to_string(), token_type: TokenType::Type, pos: 0 },
            Token { value: "func".to_string(), token_type: TokenType::Identifier, pos: 4 },
            vec![
                VariableDeclaration(
                    Token { value: "int".to_string(), token_type: TokenType::Type, pos: 9 },
                    Token { value: "x".to_string(), token_type: TokenType::Identifier, pos: 13 },
                )
            ]
        )
    ]))]
    #[case("int func(int x, int y);", TranslationUnit(vec![
    FunctionDeclaration(
            Token { value: "int".to_string(), token_type: TokenType::Type, pos: 0 },
            Token { value: "func".to_string(), token_type: TokenType::Identifier, pos: 4 },
            vec![
                VariableDeclaration(
                    Token { value: "int".to_string(), token_type: TokenType::Type, pos: 9 },
                    Token { value: "x".to_string(), token_type: TokenType::Identifier, pos: 13 },
                ),
                VariableDeclaration(
                    Token { value: "int".to_string(), token_type: TokenType::Type, pos: 16 },
                    Token { value: "y".to_string(), token_type: TokenType::Identifier, pos: 20 },
                )
            ]
        )
    ]))]
    fn test_function_declaration(#[case] test_case: String, #[case] expected: ASTNode) {
        let tokens = Lexer::new(test_case.clone()).lex();
        let result = Parser::new(tokens).parse();
        assert_eq!(expected, result);
    }

    #[rstest::rstest]
    #[case("f();", TranslationUnit(vec![
        ExpressionStatement(
            Expression::FunctionCall(
                Token { value: "f".to_string(), token_type: TokenType::Identifier, pos: 0},
                vec![]
            )
        )]
    ))]
    #[case("f(1);", TranslationUnit(vec![
        ExpressionStatement(
            Expression::FunctionCall(
                Token { value: "f".to_string(), token_type: TokenType::Identifier, pos: 0},
                vec![
                    Expression::IntegerLiteral(
                        Token { value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 2}
                    )
                ]
            )
        )]
    ))]
    #[case("f(1, x, g());", TranslationUnit(vec![
        ExpressionStatement(
            Expression::FunctionCall(
                Token { value: "f".to_string(), token_type: TokenType::Identifier, pos: 0},
                vec![
                    Expression::IntegerLiteral(
                        Token { value: "1".to_string(), token_type: TokenType::IntegerLiteral, pos: 2}
                    ),
                    Expression::Variable(
                        Token { value: "x".to_string(), token_type: TokenType::Identifier, pos: 5}
                    ),
                    Expression::FunctionCall(
                        Token { value: "g".to_string(), token_type: TokenType::Identifier, pos: 8},
                        vec![]
                    )
                ]
            )
        )]
    ))]
    fn test_parse_function_call(#[case] test_case: String, #[case] expected: ASTNode) {
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
