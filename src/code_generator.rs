use crate::ast::*;
use std::collections::HashMap;

pub struct Variable {
    variable_type: String, // TODO change to enum
    stack_offset: i64,
}

pub struct CodeGenerator {
    stack_top: i64,
    variables: HashMap<String, Variable>,
}

impl CodeGenerator {
    pub fn new() -> Self {
        Self {
            stack_top: -8, // Reserves initial space for the return address
            variables: HashMap::default(),
        }
    }


    pub fn generate(&mut self, root: &ASTNode) -> String {
        return match root {
            ASTNode::Program(_) => self.generate_program(root),
            ASTNode::ReturnStatement(_, _) => self.generate_return_statement(root),
            ASTNode::Declaration(_, _, _) => self.generate_declaration(root),
            ASTNode::ExpressionNode(_) => self.generate_expression(root),
        };
    }

    fn generate_expression(&self, node: &ASTNode) -> String {
        return match node {
            ASTNode::ExpressionNode(expr) => match expr {
                Expression::IntegerLiteralExpression(_) => self.generate_integral_literal(node),
                Expression::VariableExpression(_) => self.generate_variable_expression(node),
            },
            _ => panic!(""),
        };
    }

    fn generate_integral_literal(&self, node: &ASTNode) -> String {
        match node {
            ASTNode::ExpressionNode(Expression::IntegerLiteralExpression(token)) => {
                format!("${}", token.value.clone())
            }
            _ => panic!(""),
        }
    }

    fn generate_variable_expression(&self, node: &ASTNode) -> String {
        match node {
            ASTNode::ExpressionNode(Expression::VariableExpression(token)) => {
                let definition = self
                    .variables
                    .get(token.value.as_str())
                    .unwrap_or_else(|| panic!("Undefined variable: {}", token.value.as_str()));
                format!("{}(%rbp)", definition.stack_offset)
            }
            _ => panic!(""),
        }
    }

    fn generate_program(&mut self, node: &ASTNode) -> String {
        let mut result = String::new();
        match node {
            ASTNode::Program(nodes_vector) => {
                for node in nodes_vector {
                    result.push_str(self.generate(node).as_str());
                }
                format!(
                    "push %rbp\n\
                         mov %rsp, %rbp\n\
                         subq ${}, %rsp\n\
                         {}",
                    -self.stack_top, result
                )
            }
            _ => panic!(""),
        }
    }

    fn generate_return_statement(&mut self, node: &ASTNode) -> String {
        return match node {
            ASTNode::ReturnStatement(_, expr_node) => {
                format!(
                    "mov %rbp, %rsp\nmov {}, %rax\npop %rbp\nret\n",
                    self.generate(expr_node)
                )
            }
            _ => panic!(""),
        };
    }

    fn generate_declaration(&mut self, node: &ASTNode) -> String {
        match node {
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
                let mov_mnemonic = match variable_size {
                    1 => "movb",
                    2 => "movw",
                    4 => "movl",
                    8 => "movq",
                    _ => panic!(""),
                };
                let result = format!(
                    "{} {}, {}(%rbp)\n",
                    mov_mnemonic,
                    self.generate(expr_node),
                    (self.stack_top)
                );
                self.stack_top -= variable_size as i64;
                return result;
            }
            _ => panic!(),
        }
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
    #[case(
        "return 3;",
        "push %rbp\n\
        mov %rsp, %rbp\n\
        subq $8, %rsp\n\
        mov %rbp, %rsp\n\
        mov $3, %rax\n\
        pop %rbp\n\
        ret\n"
    )]
    fn test_generate_return_statement(#[case] test_case: String, #[case] expected: String) {
        let generated = generate_code(test_case);
        assert_eq!(expected, generated);
    }

    #[rstest::rstest]
    #[ignore] // TODO: inject a return statement in the end.
    #[case(
        "int x = 42;",
        "mov %rsp, %rbp\n\
        leaq -8(%rsp), %rsp\n\
        movl $42, (%rsp)\n"
    )]
    fn test_generate_declaration(#[case] test_case: String, #[case] expected: String) {
        let generated = generate_code(test_case);
        assert_eq!(expected, generated);
    }

    #[rstest::rstest]
    #[case(
        "int x = 42; return x;",
        "push %rbp\n\
        mov %rsp, %rbp\n\
        subq $12, %rsp\n\
        movl $42, -8(%rbp)\n\
        mov %rbp, %rsp\n\
        mov -8(%rbp), %rax\n\
        pop %rbp\n\
        ret\n"
    )]
    #[case(
        "int x = 42; int y = 12; return y;",
        "push %rbp\n\
        mov %rsp, %rbp\n\
        subq $16, %rsp\n\
        movl $42, -8(%rbp)\n\
        movl $12, -12(%rbp)\n\
        mov %rbp, %rsp\n\
        mov -12(%rbp), %rax\n\
        pop %rbp\n\
        ret\n"
    )]
    fn test_return_variable(#[case] test_case: String, #[case] expected: String) {
        let generated = generate_code(test_case);
        assert_eq!(expected, generated);
    }
}
