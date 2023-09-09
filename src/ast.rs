use crate::tokens::*;

#[derive(Debug, PartialEq, Eq)]
pub enum ASTNode {
    IntegerLiteralExpression(Token),
    // TODO split declaration and assignment into two separate nodes.
    Declaration(Token, Token, Box<ASTNode>), // Type, Identifier, Expression
    ReturnStatement(Token, Box<ASTNode>),    // ReturnKeyword, Expression
    Program(Vec<ASTNode>),
}

pub trait EvaluateExpression {
    fn evaluate(&self) -> String;
}

impl EvaluateExpression for ASTNode {
    fn evaluate(&self) -> String {
        match self {
            ASTNode::IntegerLiteralExpression(token) => token.value.clone(),
            _ => panic!("ASTNode of type {:?} cannot be evaluated", self),
        }
    }
}
