use crate::tokens::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression {
    IntegerLiteral(Token),
    Variable(Token),
    Binary(Token, Box<Expression>, Box<Expression>),
    Unary(Token, Box<Expression>),
    Parenthesized(Box<Expression>),
    Assignment(Token, Box<Expression>),
    FunctionCall(Token, Vec<Expression>),
    Empty,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ASTNode {
    ExpressionStatement(Expression),
    ExpressionNode(Expression),
    VariableDeclaration(Token, Token), // Type, Identifier
    VariableDefinition(Token, Token, Box<ASTNode>), // Type, Identifier, Expression
    FunctionDeclaration(Token, Token, Vec<ASTNode>), // Type, Identifier, Parameters
    FunctionDefinition(Token, Token, Vec<ASTNode>, Box<ASTNode>), // Type, Identifier, Parameters, Body
    ReturnStatement(Token, Box<ASTNode>),                         // ReturnKeyword, Expression
    TranslationUnit(Vec<ASTNode>),
    If(Token, Box<ASTNode>, Box<ASTNode>, Option<Box<ASTNode>>), // If, Condition, Body, Else
    While(Token, Box<ASTNode>, Box<ASTNode>),                    // While, Condition, Body
    DoWhile(Token, Box<ASTNode>, Token, Box<ASTNode>),           // Do, Body, While, Condition
    For(Token, [Box<ASTNode>; 3], Box<ASTNode>), // For, [Declaration, Assignment, Condition, Update], Body
    Scope(Vec<ASTNode>),
}
