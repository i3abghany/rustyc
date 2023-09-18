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
        match root {
            ASTNode::Program(_) => self.generate_program(root),
            ASTNode::ReturnStatement(_, _) => self.generate_return_statement(root),
            ASTNode::Declaration(_, _) => self.generate_declaration(root),
            ASTNode::Assignment(_, _) => self.generate_assignment(root),
            ASTNode::ExpressionNode(_) => self.generate_expression(root),
        }
    }

    fn generate_expression(&self, node: &ASTNode) -> String {
        match node {
            ASTNode::ExpressionNode(expr) => match expr {
                Expression::IntegerLiteral(_) => self.generate_integral_literal(node),
                Expression::Variable(_) => self.generate_variable_expression(node),
                _ => panic!(""),
            },
            _ => panic!(""),
        }
    }

    fn generate_integral_literal(&self, node: &ASTNode) -> String {
        match node {
            ASTNode::ExpressionNode(Expression::IntegerLiteral(token)) => {
                format!(
                    "mov ${}, {}\n",
                    token.value.clone(),
                    CodeGenerator::get_reg1(8)
                )
            }
            _ => panic!(""),
        }
    }

    fn generate_variable_expression(&self, node: &ASTNode) -> String {
        match node {
            ASTNode::ExpressionNode(Expression::Variable(token)) => {
                let definition = self
                    .variables
                    .get(token.value.as_str())
                    .unwrap_or_else(|| panic!("Undefined variable: {}", token.value.as_str()));
                let variable_size = CodeGenerator::variable_size(&definition.variable_type);
                let mov_instruction = CodeGenerator::mov_mnemonic(variable_size);
                format!(
                    "{} {}(%rbp), {}\n",
                    mov_instruction,
                    definition.stack_offset,
                    CodeGenerator::get_reg1(variable_size)
                )
            }
            _ => panic!(""),
        }
    }

    fn generate_program(&mut self, node: &ASTNode) -> String {
        let mut result = String::new();
        match node {
            ASTNode::Program(nodes_vector) => {
                for node in nodes_vector {
                    result.push_str(&self.generate(node));
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
        match node {
            ASTNode::ReturnStatement(_, expr_node) => {
                let mut result = self.generate(expr_node);
                result.push_str(&format!(
                    "mov %rbp, %rsp\nmov {}, %rax\npop %rbp\nret\n",
                    CodeGenerator::get_reg1(8)
                ));
                result
            }
            _ => panic!("Return: Expected a return node, found {:?}", node),
        }
    }

    fn generate_assignment(&mut self, node: &ASTNode) -> String {
        match node {
            ASTNode::Assignment(identifier, expr_node) => {
                let variable = self.variables.get(&identifier.value).expect(&format!(
                    "Assignment: the identifier `{}` is not defined",
                    identifier.value
                ));
                let stack_offset = variable.stack_offset;

                // TODO support referential assignment
                let variable_size = CodeGenerator::variable_size(&variable.variable_type);
                let mov_instruction = CodeGenerator::mov_mnemonic(variable_size);

                let mut result = self.generate(expr_node);

                result.push_str(&format!(
                    "{} {}, {}(%rbp)\n",
                    mov_instruction,
                    CodeGenerator::get_reg1(variable_size),
                    stack_offset
                ));

                result
            }
            _ => panic!(""),
        }
    }

    fn generate_declaration(&mut self, node: &ASTNode) -> String {
        match node {
            ASTNode::Declaration(variable_type, identifier) => {
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
                self.stack_top -= variable_size as i64;
            }
            _ => panic!("Declaration: Expected declaration node, found {:?}", node),
        }
        "".to_string()
    }

    fn variable_size(variable_type: &str) -> usize {
        4
    }

    fn mov_mnemonic(size: usize) -> &'static str {
        match size {
            1 => "movb",
            2 => "movw",
            4 => "movl",
            8 => "movq",
            _ => panic!("Unsupported size `{}`.", size),
        }
    }

    fn get_reg1(size: usize) -> &'static str {
        match size {
            1 | 2 | 4 => "%ebx",
            8 => "%rbx",
            _ => panic!("Invalid register size: {}", size),
        }
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
        mov $3, %rbx\n\
        mov %rbp, %rsp\n\
        mov %rbx, %rax\n\
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
        mov $42, %rbx\n\
        movl %ebx, -8(%rbp)\n\
        movl -8(%rbp), %ebx\n\
        mov %rbp, %rsp\n\
        mov %rbx, %rax\n\
        pop %rbp\n\
        ret\n"
    )]
    #[case(
        "int x = 42; int y = 12; return y;",
        "push %rbp\n\
        mov %rsp, %rbp\n\
        subq $16, %rsp\n\
        mov $42, %rbx\n\
        movl %ebx, -8(%rbp)\n\
        mov $12, %rbx\n\
        movl %ebx, -12(%rbp)\n\
        movl -12(%rbp), %ebx\n\
        mov %rbp, %rsp\n\
        mov %rbx, %rax\n\
        pop %rbp\n\
        ret\n"
    )]
    fn test_return_variable(#[case] test_case: String, #[case] expected: String) {
        let generated = generate_code(test_case);
        assert_eq!(expected, generated);
    }

    #[rstest::rstest]
    #[case(
        "int x = 1; int y = x; return y;",
        "push %rbp\n\
        mov %rsp, %rbp\n\
        subq $16, %rsp\n\
        mov $1, %rbx\n\
        movl %ebx, -8(%rbp)\n\
        movl -8(%rbp), %ebx\n\
        movl %ebx, -12(%rbp)\n\
        movl -12(%rbp), %ebx\n\
        mov %rbp, %rsp\n\
        mov %rbx, %rax\n\
        pop %rbp\n\
        ret\n"
    )]
    fn test_assignment_with_variable_expression(
        #[case] test_case: String,
        #[case] expected: String,
    ) {
        let generated = generate_code(test_case);
        assert_eq!(expected, generated);
    }
}
