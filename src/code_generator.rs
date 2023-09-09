use crate::ast::*;
use std::collections::HashMap;

pub struct Variable {
    variable_type: String, // TODO change to enum
    stack_offset: usize,
}

pub struct CodeGenerator {
    stack_top: usize,
    variables: HashMap<String, Variable>,
}

impl CodeGenerator {
    pub fn new() -> Self {
        Self {
            stack_top: 0,
            variables: HashMap::default(),
        }
    }

    pub fn generate(&mut self, root: &ASTNode) -> String {
        return match root {
            ASTNode::Program(_) => self.generate_program(root),
            ASTNode::ReturnStatement(_, _) => self.generate_return_statement(root),
            ASTNode::Declaration(_, _, _) => self.generate_declaration(root),
            ASTNode::IntegerLiteralExpression(_) => root.evaluate(),
            _ => todo!(),
        };
    }

    fn generate_program(&mut self, node: &ASTNode) -> String {
        let mut result = String::new();
        return match node {
            ASTNode::Program(nodes_vector) => {
                for node in nodes_vector {
                    result.push_str(self.generate(node).as_str());
                }
                result
            }
            _ => panic!(""),
        };
    }

    fn generate_return_statement(&mut self, node: &ASTNode) -> String {
        return match node {
            ASTNode::ReturnStatement(_, expr_node) => {
                format!("mov ${}, %eax\nret", self.generate(expr_node))
            }
            _ => panic!(""),
        };
    }

    fn generate_declaration(&mut self, node: &ASTNode) -> String {
        return match node {
            ASTNode::Declaration(variable_type, identifier, expr_node) => {
                if self.variables.contains_key(&identifier.value) {
                    panic!(
                        "Declaration: the identifier `{}` is already in use",
                        identifier.value
                    )
                }

                self.variables.insert(
                    identifier.value.clone(),
                    Variable {
                        variable_type: variable_type.value.clone(),
                        stack_offset: self.stack_top,
                    },
                );

                let variable_size = CodeGenerator::variable_size(&variable_type.value);
                let push: &str = match variable_size {
                    4 => "pushd",
                    _ => panic!(
                        "The size {} of the primitive data type {} is not yet supported",
                        variable_size, variable_type.value
                    ),
                };

                format!("{} ${}\n", push, self.generate(expr_node))
            }
            _ => panic!(),
        };
    }

    fn variable_size(variable_type: &String) -> usize {
        4
    }
}

#[cfg(test)]
mod tests {

    use super::CodeGenerator;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn generate_code(src: String) -> String {
        let tokens = Lexer::new(src).lex();
        let ast = Parser::new(tokens).parse();
        let mut generator = CodeGenerator::new();
        let generated = generator.generate(&ast);
        return generated;
    }

    #[rstest::rstest]
    #[case("return 6123;", "mov $6123, %eax\nret")]
    fn test_generate_return_statement(#[case] test_case: String, #[case] expected: String) {
        let generated = generate_code(test_case);
        assert_eq!(expected, generated);
    }

    #[rstest::rstest]
    #[case("int x = 42;", "pushd $42\n")]
    fn test_generate_declaration(#[case] test_case: String, #[case] expected: String) {
        let generated = generate_code(test_case);
        assert_eq!(expected, generated);
    }
}
