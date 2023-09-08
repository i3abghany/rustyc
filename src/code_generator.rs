use crate::ast::*;

pub struct CodeGenerator {}

impl CodeGenerator {
    pub fn generate(root: &ASTNode) -> String {
        return match root {
            ASTNode::Program(_) => CodeGenerator::generate_program(root),
            ASTNode::ReturnStatement(_, _) => CodeGenerator::generate_return_statement(root),
            ASTNode::IntegerLiteralExpression(token) => token.value.clone(),
            _ => todo!(),
        };
    }

    fn generate_program(node: &ASTNode) -> String {
        let mut result = String::new();
        return match node {
            ASTNode::Program(nodes_vector) => {
                for node in nodes_vector {
                    result.push_str(CodeGenerator::generate(node).as_str());
                }
                result
            }
            _ => panic!(""),
        };
    }

    fn generate_return_statement(node: &ASTNode) -> String {
        return match node {
            ASTNode::ReturnStatement(_, expr_node) => {
                format!("mov ${}, %eax\nret", CodeGenerator::generate(expr_node))
            }
            _ => panic!(""),
        };
    }
}

#[cfg(test)]
mod tests {

    use super::CodeGenerator;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[rstest::rstest]
    #[case("return 6123;", "mov $6123, %eax\nret")]
    fn test_generate_return_statement(#[case] test_case: String, #[case] expected: String) {
        let tokens = Lexer::new(test_case).lex();
        let ast = Parser::new(tokens).parse();
        let generated = CodeGenerator::generate(&ast);
        assert_eq!(expected, generated);
    }
}
