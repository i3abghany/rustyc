use crate::ast::ASTNode::*;
use crate::ast::*;
use crate::symbol_table;
use crate::symbol_table::*;
use crate::tokens::*;

pub struct CodeGenerator {
    symbol_table: SymbolTable,
}

impl CodeGenerator {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
        }
    }

    pub fn generate(&mut self, root: &ASTNode) -> String {
        match root {
            ASTNode::Program(_) => self.generate_program(root),
            ASTNode::ReturnStatement(_, _) => self.generate_return_statement(root),
            ASTNode::Declaration(_, _) => self.generate_declaration(root),
            ASTNode::Assignment(_, _) => self.generate_assignment(root),
            ASTNode::ExpressionNode(expression) => self.generate_expression(expression),
            ASTNode::Scope(_) => self.generate_scope(root),
            _ => panic!(""),
        }
    }

    fn generate_scope(&mut self, scope: &ASTNode) -> String {
        let mut result = String::new();
        match scope {
            ASTNode::Scope(statements) => {
                let stack_top = self.symbol_table.current_scope_stack_top();
                self.symbol_table
                    .push_scope(symbol_table::Scope::new(stack_top));
                for statement in statements {
                    result.push_str(self.generate(statement).as_str());
                }
                self.symbol_table.pop_scope();
                result
            }
            _ => panic!(""),
        }
    }

    fn generate_expression(&mut self, expression: &Expression) -> String {
        match expression {
            Expression::IntegerLiteral(_) => self.generate_integral_literal(expression),
            Expression::Variable(_) => self.generate_variable_expression(expression),
            Expression::Binary(_, _, _) => self.generate_binary_expression(expression),
            Expression::Parenthesized(internal_expression) => {
                self.generate_expression(internal_expression)
            }
        }
    }

    fn generate_binary_expression(&mut self, expression: &Expression) -> String {
        let mut result = String::new();
        match expression {
            Expression::Binary(token, left, right) => {
                result.push_str(&self.generate_expression(right));
                result.push_str(format!("push {}\n", CodeGenerator::get_reg1(8)).as_str());
                result.push_str(&self.generate_expression(left));
                result.push_str(format!("pop {}\n", CodeGenerator::get_reg2(8)).as_str());

                // TODO: Support floating point operations
                let reg1 = CodeGenerator::get_reg1(8);
                let reg2 = CodeGenerator::get_reg2(8);
                match token.token_type {
                    TokenType::Plus => {
                        result.push_str(&format!("add {}, {}\n", reg2, reg1));
                    }
                    TokenType::Minus => {
                        result.push_str(&format!("sub {}, {}\n", reg2, reg1));
                    }
                    TokenType::Star => {
                        result.push_str(&format!("imul {}, {}\n", reg2, reg1));
                    }
                    TokenType::Slash => {
                        result.push_str(&format!("push %rax\n"));
                        result.push_str(&format!("push %rdx\n"));
                        result.push_str(&format!("mov {}, %rax\n", reg1));
                        result.push_str(&format!("mov $0, %rdx\n"));
                        result.push_str(&format!("idiv {}\n", reg2));
                        result.push_str(&format!("mov %rax, {}\n", reg1));
                        result.push_str(&format!("pop %rdx\n"));
                        result.push_str(&format!("pop %rax\n"));
                    }
                    // TODO: Account for short-circuiting of boolean expressions.
                    //  For now, we evaluate the full expression no matter how it
                    //  is structured.
                    TokenType::AndAnd => {
                        result.push_str(&format!("and {}, {}\nand $1, {}\n", reg2, reg1, reg1));
                    }
                    TokenType::BarBar => {
                        result.push_str(&format!("or {}, {}\nand $1, {}\n", reg2, reg1, reg1));
                    }
                    TokenType::And => {
                        result.push_str(&format!("and {}, {}\n", reg2, reg1));
                    }
                    TokenType::Bar => {
                        result.push_str(&format!("or {}, {}\n", reg2, reg1));
                    }
                    TokenType::Caret => {
                        result.push_str(&format!("xor {}, {}\n", reg2, reg1));
                    }
                    _ => panic!("Unsupported operator: {:?}", token),
                }
            }
            _ => panic!(
                "Internal Error: Expected a binary expression, found: {:#?}",
                expression
            ),
        }
        result
    }

    fn generate_integral_literal(&self, expression: &Expression) -> String {
        match expression {
            Expression::IntegerLiteral(token) => {
                format!(
                    "mov ${}, {}\n",
                    token.value.clone(),
                    CodeGenerator::get_reg1(8)
                )
            }
            _ => panic!(
                "Internal Error: Expected integral literal, found: {:#?}",
                expression
            ),
        }
    }

    fn generate_variable_expression(&self, expression: &Expression) -> String {
        match expression {
            Expression::Variable(token) => {
                let definition = self
                    .symbol_table
                    .get(token.value.as_str())
                    .unwrap_or_else(|| panic!("Undefined variable: {}", token.value.as_str()));
                match definition {
                    Symbol::Variable { stack_offset, .. } => {
                        let mov_instruction = CodeGenerator::mov_mnemonic(definition.size());
                        format!(
                            "{} {}(%rbp), {}\n",
                            mov_instruction,
                            stack_offset,
                            CodeGenerator::get_reg1(definition.size())
                        )
                    }
                    _ => panic!(""),
                }
            }
            _ => panic!(
                "Internal Error: Expected variable expression, found: {:#?}",
                expression
            ),
        }
    }

    fn generate_program(&mut self, node: &ASTNode) -> String {
        let mut result = String::new();
        match node {
            ASTNode::Program(nodes_vector) => {
                self.symbol_table.push_scope(symbol_table::Scope::new(-8));
                self.symbol_table.reset_largest_offset();
                for node in nodes_vector {
                    result.push_str(&self.generate(node));
                }
                self.symbol_table.pop_scope();

                format!(
                    ".global main\n\
                    main:\n\
                    push %rbp\n\
                    mov %rsp, %rbp\n\
                    subq ${}, %rsp\n\
                    {}",
                    -self.symbol_table.current_largest_offset(),
                    result
                )
            }
            _ => panic!("Internal Error: Expected program node, found {:?}", node),
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
                let variable = self.symbol_table.get(&identifier.value).unwrap_or_else(|| {
                    panic!(
                        "Assignment: the identifier `{}` is not defined",
                        identifier.value
                    )
                });
                let mut variable_stack_offset = 0;
                let variable_size = variable.size();
                match variable {
                    Symbol::Variable { stack_offset, .. } => {
                        variable_stack_offset = *stack_offset;
                    }
                }

                // TODO support referential assignment
                let mov_instruction = CodeGenerator::mov_mnemonic(variable.size());
                let mut result = self.generate(expr_node);

                result.push_str(&format!(
                    "{} {}, {}(%rbp)\n",
                    mov_instruction,
                    CodeGenerator::get_reg1(variable_size),
                    variable_stack_offset
                ));
                result
            }
            _ => panic!(""),
        }
    }

    fn generate_declaration(&mut self, node: &ASTNode) -> String {
        match node {
            ASTNode::Declaration(variable_type, identifier) => {
                if let Some(_) = self.symbol_table.get_at_current_scope(&identifier.value) {
                    panic!(
                        "Declaration: the identifier `{}` is already in use",
                        identifier.value
                    )
                }

                let stack_offset = self.symbol_table.current_scope_stack_top();
                let symbol = Symbol::Variable {
                    variable_type: variable_type.value.clone(),
                    stack_offset,
                };
                self.symbol_table.insert(&identifier.value, symbol);
            }
            _ => panic!("Declaration: Expected declaration node, found {:?}", node),
        }
        "".to_string()
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

    fn get_reg2(size: usize) -> &'static str {
        match size {
            1 | 2 | 4 => "%ecx",
            8 => "%rcx",
            _ => panic!("Invalid register size: {}", size),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::CodeGenerator;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::test_utils::*;

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
        ".global main\n\
        main:\n\
        push %rbp\n\
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
        ".global main\n\
        main:\n\
        push %rbp\n\
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
        ".global main\n\
        main:\n\
        push %rbp\n\
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
        ".global main\n\
        main:\n\
        push %rbp\n\
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

    #[rstest::rstest]
    #[case("return 5 + 3 * 2 + (2 * 19 * 4) / 2 + 9 * 12 / 3 * 3;", 195)]
    #[case("return (1 | 2 | 4 | 8 | 16 | 32) & 85;", 21)]
    #[case("int x = 12; int y = 423; return x || y;", 1)]
    #[case("int false = 0; int true = 123; return true || false;", 1)]
    #[case(
        "int x = 312; int y = 99; int z; z = 2 * x / 3 + y * y; return z - x;",
        9697
    )]
    fn test_generate_expression_with_precedence(#[case] test_case: String, #[case] expected: i32) {
        let generated = generate_code(test_case);
        expect_exit_code(generated, expected);
    }

    #[rstest::rstest]
    #[case("int x = 55; { int y = 5; x = y; } return x;", 5)]
    #[case("int x = 6; { int y = 55; return y; } return x;", 55)]
    #[case("int x = 6; { int y = 55; return y; }", 55)]
    #[case("int x = 1; { int x = 2; { int x = 3; return x; } }", 3)]
    #[case("int x = 1; { int y = 2; { return x; } }", 1)]
    #[case(
        "int x = 1; { int y = 5; } { int y = 6; } int y = 7; { int y = 8; return y; }",
        8
    )]
    #[case("{{{{{{{{{{ return 5; }}}}}}}}}}", 5)]
    fn test_generated_scoped_programs(#[case] test_case: String, #[case] expected: i32) {
        let generated = generate_code(test_case);
        expect_exit_code(generated, expected);
    }

    #[rstest::rstest]
    #[case("{ int x = 4; } return x;")]
    #[case("{ int x = 4; } { int y = 5; int z = 6; } return x + z;")]
    #[should_panic]
    fn test_undefined_variables_in_scope(#[case] test_case: String) {
        let generated = generate_code(test_case);
    }
}
