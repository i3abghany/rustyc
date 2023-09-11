use crate::tokens::*;

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    IntegerLiteralExpression(Token),
    VariableExpression(Token),
}

#[derive(Debug, PartialEq, Eq)]
pub enum ASTNode {
    ExpressionNode(Expression),
    Assignment(Token, Box<ASTNode>),      // Identifier, Expression
    Declaration(Token, Token),            // Type, Identifier
    ReturnStatement(Token, Box<ASTNode>), // ReturnKeyword, Expression
    Program(Vec<ASTNode>),
}
