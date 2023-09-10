use crate::tokens::*;

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    IntegerLiteralExpression(Token),
    VariableExpression(Token),
}

#[derive(Debug, PartialEq, Eq)]
pub enum ASTNode {
    ExpressionNode(Expression),
    // TODO split declaration and assignment into two separate nodes.
    Declaration(Token, Token, Box<ASTNode>), // Type, Identifier, Expression
    ReturnStatement(Token, Box<ASTNode>),    // ReturnKeyword, Expression
    Program(Vec<ASTNode>),
}
