use crate::tokens::*;

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    IntegerLiteral(Token),
    Variable(Token),
    Binary(Token, Box<Expression>, Box<Expression>),
    Unary(Token, Box<Expression>),
    Parenthesized(Box<Expression>),
    Empty,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ASTNode {
    ExpressionStatement(Box<Expression>),
    ExpressionNode(Expression),
    Assignment(Token, Box<ASTNode>),      // Identifier, Expression
    Declaration(Token, Token),            // Type, Identifier
    ReturnStatement(Token, Box<ASTNode>), // ReturnKeyword, Expression
    Program(Vec<ASTNode>),
    If(Token, Box<ASTNode>, Box<ASTNode>, Option<Box<ASTNode>>), // If, Condition, Body, Else
    While(Token, Box<ASTNode>, Box<ASTNode>),                    // While, Condition, Body
    DoWhile(Token, Box<ASTNode>, Token, Box<ASTNode>),           // Do, Body, While, Condition
    Scope(Vec<ASTNode>),
}
