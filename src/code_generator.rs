use crate::ast::ASTNode::*;
use crate::ast::*;
use crate::symbol_table;
use crate::symbol_table::*;
use crate::tokens::*;
use uuid::Uuid;

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
            Program(..) => self.generate_program(root),
            ReturnStatement(..) => self.generate_return_statement(root),
            VariableDeclaration(..) => self.generate_variable_declaration(root),
            VariableDefinition(..) => self.generate_variable_definition(root),
            FunctionDeclaration(..) => todo!(),
            FunctionDefinition(..) => todo!(),
            ExpressionNode(expression) => self.generate_expression(expression),
            Scope(..) => self.generate_scope(root),
            If(..) => self.generate_if_statement(root),
            While(..) => self.generate_while(root),
            DoWhile(..) => self.generate_do_while(root),
            ExpressionStatement(..) => self.generate_expression_statement(root),
            For(..) => self.generate_for(root),
        }
    }

    fn generate_for(&mut self, node: &ASTNode) -> String {
        match node {
            For(_, [init, condition, update], body) => {
                let stack_top = self.symbol_table.current_scope_stack_top();
                self.symbol_table
                    .push_scope(symbol_table::Scope::new(stack_top));
                let mut result = String::new();
                result.push_str(&self.generate(init));
                let enter_label = self.unique_label("__FOR_ENTER_");
                result.push_str(&format!("{}:\n", enter_label));
                let mut body = self.generate(body);
                body.push_str(&self.generate(update));
                body.push_str(&format!("jmp {}\n", enter_label));
                let condition = self.generate(condition);

                if condition.is_empty() {
                    result.push_str(&body);
                } else {
                    result.push_str(&self.generate_condition_block(
                        "__FOR_EXIT_",
                        &condition,
                        &body,
                    ));
                }

                self.symbol_table.pop_scope();
                result
            }
            _ => panic!("Internal error: Expected for statement, found {:?}", node),
        }
    }

    fn generate_expression_statement(&mut self, node: &ASTNode) -> String {
        match node {
            ExpressionStatement(expression) => {
                if let Expression::Assignment(..) = expression {
                    self.generate_assignment(expression)
                } else {
                    self.generate_expression(expression)
                }
            }
            _ => panic!(
                "Internal error: Expected expression statement, found {:?}",
                node
            ),
        }
    }

    fn generate_do_while(&mut self, node: &ASTNode) -> String {
        match node {
            ASTNode::DoWhile(_, body, _, condition) => {
                let mut result = String::new();
                let enter_label = self.unique_label("__DO_WHILE_ENTER_");
                let body_label = self.unique_label("__DO_WHILE_BODY_");
                result.push_str(&format!("jmp {}\n", body_label));
                let mut body = format!("{}:\n{}", body_label, &self.generate(body));
                body.push_str(&format!("jmp {}\n", enter_label));
                let condition = self.generate(condition);
                result.push_str(&format!("{}:\n", enter_label));
                result.push_str(&self.generate_condition_block(
                    "__DO_WHILE_EXIT_",
                    &condition,
                    &body,
                ));
                result
            }
            _ => panic!("Internal error: Expected do while node, found {:?}", node),
        }
    }

    fn generate_while(&mut self, while_node: &ASTNode) -> String {
        match while_node {
            ASTNode::While(_, condition, body) => {
                let mut result = String::new();
                let enter_label = self.unique_label("__WHILE_ENTER_");
                let condition = self.generate(condition);
                let mut body = self.generate(body);
                body.push_str(&format!("jmp {}\n", enter_label));

                result.push_str(&format!("{}:\n", enter_label));
                result.push_str(&self.generate_condition_block("__WHILE_EXIT_", &condition, &body));
                result
            }
            _ => panic!(
                "Internal error: Expected while node, found {:?}",
                while_node
            ),
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
            Expression::Empty => "".to_string(),
            Expression::IntegerLiteral(_) => self.generate_integral_literal(expression),
            Expression::Variable(_) => self.generate_variable_expression(expression),
            Expression::Binary(_, _, _) => self.generate_binary_expression(expression),
            Expression::Unary(_, _) => self.generate_unary_expression(expression),
            Expression::Assignment(..) => self.generate_assignment(expression),
            Expression::Parenthesized(internal_expression) => {
                self.generate_expression(internal_expression)
            }
        }
    }

    fn generate_unary_expression(&mut self, expression: &Expression) -> String {
        let mut result = String::new();
        match expression {
            Expression::Unary(operator, expression) => {
                result.push_str(self.generate_expression(&expression).as_str());
                match operator.token_type {
                    TokenType::Plus => {}
                    TokenType::Minus => {
                        result.push_str(&format!("neg {}\n", CodeGenerator::get_reg1(8)));
                    }
                    _ => panic!("Unsupported unary operator: {:#?}", operator),
                }
            }
            _ => panic!(
                "Internal Error: Expected a unary expression, found: {:#?}",
                expression
            ),
        }
        result
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
                let reg1 = CodeGenerator::get_reg1(4);
                let reg2 = CodeGenerator::get_reg2(4);
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
                        result.push_str(&format!("mov {}, %eax\n", reg1));
                        result.push_str(&format!("mov $0, %edx\n"));
                        result.push_str(&format!("idiv {}\n", reg2));
                        result.push_str(&format!("mov %eax, {}\n", reg1));
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
                    TokenType::EqualsEquals => {
                        result.push_str(&format!("cmp {}, {}\n", reg2, reg1));
                        result.push_str(&format!("sete %bl\n"));
                        result.push_str(&format!("movzbl %bl, {}\n", reg1));
                    }
                    TokenType::NotEquals => {
                        result.push_str(&format!("cmp {}, {}\n", reg2, reg1));
                        result.push_str(&format!("setne %bl\n"));
                        result.push_str(&format!("movzbl %bl, %ebx\n"));
                    }
                    TokenType::GreaterThan => {
                        result.push_str(&format!("cmp {}, {}\n", reg2, reg1));
                        result.push_str(&format!("setg %bl\n"));
                        result.push_str(&format!("movzbl %bl, %ebx\n"));
                    }
                    TokenType::GreaterThanEquals => {
                        result.push_str(&format!("cmp {}, {}\n", reg2, reg1));
                        result.push_str(&format!("setge %bl\n"));
                        result.push_str(&format!("movzbl %bl, %ebx\n"));
                    }
                    TokenType::LessThan => {
                        result.push_str(&format!("cmp {}, {}\n", reg2, reg1));
                        result.push_str(&format!("setl %bl\n"));
                        result.push_str(&format!("movzbl %bl, %ebx\n"));
                    }
                    TokenType::LessThanEquals => {
                        result.push_str(&format!("cmp {}, {}\n", reg2, reg1));
                        result.push_str(&format!("setle %bl\n"));
                        result.push_str(&format!("movzbl %bl, %ebx\n"));
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

    fn generate_assignment(&mut self, expression: &Expression) -> String {
        match expression {
            Expression::Assignment(identifier, expr_node) => {
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
                let mut result = self.generate_expression(expr_node);

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

    fn unique_label(&mut self, prefix: &str) -> String {
        return format!("{}_{}", prefix, Uuid::new_v4().simple());
    }

    fn generate_condition_block(&mut self, prefix: &str, condition: &str, body: &str) -> String {
        let mut results = String::new();
        results.push_str(condition);
        results.push_str(&format!("cmp $0, {}\n", CodeGenerator::get_reg1(8)));
        let else_label = self.unique_label(prefix);
        results.push_str(&format!("je {}\n", else_label));
        results.push_str(body);
        results.push_str(&format!("{}:\n", else_label));
        results
    }

    fn generate_if_statement(&mut self, node: &ASTNode) -> String {
        match node {
            ASTNode::If(_, condition, body, else_body) => {
                let mut results = String::new();
                let condition = self.generate(condition);
                let body = self.generate(body);
                results.push_str(&self.generate_condition_block("__IF_", &condition, &body));
                if let Some(else_body) = else_body {
                    results.push_str(&self.generate(else_body));
                }
                results
            }
            _ => panic!("Internal error: Expected if statement, found {:?}", node),
        }
    }

    fn generate_variable_declaration(&mut self, node: &ASTNode) -> String {
        match node {
            ASTNode::VariableDeclaration(variable_type, identifier) => {
                if self
                    .symbol_table
                    .get_at_current_scope(&identifier.value)
                    .is_some()
                {
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

    fn generate_variable_definition(&mut self, node: &ASTNode) -> String {
        if let VariableDefinition(type_token, identifier, expression) = node {
            self.generate_variable_declaration(&VariableDeclaration(
                type_token.clone(),
                identifier.clone(),
            ));
            if let ExpressionNode(expr) = &(**expression) {
                self.generate_assignment(&Assignment(identifier.clone(), Box::new(expr.clone())))
            } else {
                panic!(
                    "Internal error: Expected expression node, found {:?}",
                    expression
                )
            }
        } else {
            panic!(
                "Internal error: Expected variable definition, found {:?}",
                node
            )
        }
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
    #[case("int x = 1; int y = 2; return x > y;", 0)]
    #[case("int x = 1; int y = 2; return x < y;", 1)]
    #[case("int x = 1; int y = 2; return x >= y;", 0)]
    #[case("int x = 1; int y = 2; return x <= y;", 1)]
    #[case("int x = 1; int y = 2; return x != y;", 1)]
    #[case("int x = 1; int y = 2; int z = 1; return x + z == y;", 1)]
    #[case("int x = 1; int y = x; return x == y;", 1)]
    #[case("return (1 | 2 | 4 | 8 | 16 | 32) & 85;", 21)]
    #[case("int x = 12; int y = 423; return x || y;", 1)]
    #[case("int x = -7; int y = 15; return x + y;", 8)]
    #[case("int x = -----++++----+------12; return x * x;", 144)]
    #[case("int x = 1; return -(+(-(+x)));", 1)]
    #[case("int false = 0; int true = 123; return true || false;", 1)]
    #[case(
        "int x = 312; int y = 99; int z; z = 2 * x / 3 + y * y; return z - x;",
        9697
    )]
    fn test_generate_expression_with_precedence(
        #[case] test_case: String,
        #[case] expected: i32,
    ) -> std::io::Result<()> {
        let generated = generate_code(test_case);
        expect_exit_code(generated, expected)?;
        Ok(())
    }

    #[rstest::rstest]
    #[case("int x = 55; { int y = 5; x = y; } return x;", 5)]
    #[case("int x = 6; { int y = 55; return y; } return x;", 55)]
    #[case("int x = 6; { int y = 55; return y; }", 55)]
    #[case("int x = 1; { int x = 2; { int x = 3; return x; } }", 3)]
    #[case("int x = 1; { int y = 2; { return x; } }", 1)]
    #[case("{{{{{{{{{{ return 5; }}}}}}}}}}", 5)]
    #[case("{{{{{{ }}}} return 1; }}", 1)]
    #[case(
        "int x = 1; { int y = 5; } { int y = 6; } int y = 7; { int y = 8; return y; }",
        8
    )]
    fn test_generated_scoped_programs(
        #[case] test_case: String,
        #[case] expected: i32,
    ) -> std::io::Result<()> {
        let generated = generate_code(test_case);
        expect_exit_code(generated, expected)?;
        Ok(())
    }

    #[rstest::rstest]
    #[case("{ int x = 4; } return x;")]
    #[case("{ int x = 4; } { int y = 5; int z = 6; } return x + z;")]
    #[case("{{{{{{ }}}} return 1; }")]
    #[case("if (1) { int x = 1; } return x;")]
    #[case("if (1) { int x = 1; } else if (0) { int y = 1; } return y;")]
    #[case("if (1) int x = 1; return x;")]
    #[case("while (0) { int x = 1; } return x;")]
    #[case("while (0) int x = 1; return x;")]
    #[case("do { int x; } while (1); return x;")]
    #[should_panic]
    fn test_undefined_variables_in_scope(#[case] test_case: String) {
        let generated = generate_code(test_case);
    }

    #[rstest::rstest]
    #[case("int x = 55; if (x & 1) { return 16; } return x;", 16)]
    #[case("int x = 55; if (x & 0) { return 16; } return x;", 55)]
    #[case("int x = 12; if (x & 1) { return 16; } else { return 5 * 12; }", 60)]
    #[case(
        "if (0) { return 16; } else if (1) { return 5 * 12; } else { return 7; }",
        60
    )]
    fn test_if_statements(#[case] test_case: String, #[case] expected: i32) -> std::io::Result<()> {
        let generated = generate_code(test_case);
        expect_exit_code(generated, expected)?;
        Ok(())
    }

    #[rstest::rstest]
    #[case(
        "int res = 0; for (int i = 0; i <= 5; i = i + 1) { res = res + i * i; } return res;",
        55
    )]
    #[case(
        "int a = 0;
        int b = 1;
        int c;
        for (int i = 0; i <= 44; i = i + 1) {
            c = b + a;
            a = b;
            b = c;
        }
        return c;",
        1836311903
    )]
    fn test_for_statements(
        #[case] test_case: String,
        #[case] expected: i32,
    ) -> std::io::Result<()> {
        let generated = generate_code(test_case);
        expect_exit_code(generated, expected)?;
        Ok(())
    }

    // Calculating the maximum Fibonacci number that fits in a 32-bit integer.
    #[rstest::rstest]
    #[case(
        "int x = 44;
    int a = 0;
    int b = 1;
    int c;
    while (x >= 0) {
       c = a + b;
       a = b;
       b = c;
       x = x - 1;
    }
    return c;",
        1836311903
    )]
    #[case(
        "int x = 5; int sum = 0; while (x != 0) { sum = sum + x * x; x = x - 1; } return sum; ",
        55
    )]
    #[case(
        "int n = 10;
    int result = 1;
    while (n > 1) {
        result = result * n;
        n = n - 1;
    }
    return result;",
        3628800
    )]
    fn test_while_statements(
        #[case] test_case: String,
        #[case] expected: i32,
    ) -> std::io::Result<()> {
        let generated = generate_code(test_case);
        expect_exit_code(generated, expected)?;
        Ok(())
    }

    #[rstest::rstest]
    #[case(
        "int x = 45;\
            int a = 0;\
            int b = 1;\
            int c;\
            do {\
               c = a + b;\
               a = b;\
               b = c;\
               x = x - 1;\
            } while (x);\
            return c;",
        1836311903
    )]
    #[case(
        "int x = 6;
    int y = 3;
    int temp1 = x * y + 3;
    int temp2 = x - y;
    int result = 1;
    int counter = 0;

    while (temp1 > temp2) {
        temp2 = temp2 + x;
        temp1 = temp1 - y;
        counter = counter + 1;
    }

    while (counter > 0) {
        result = result * temp1 + temp2;
        counter = counter - 1;
    }

    return result;",
        465
    )]
    #[case(
        "int x = 5; int sum = 0; do { sum = sum + x * x; x = x - 1; } while (x); return sum; ",
        55
    )]
    fn test_do_while_statements(
        #[case] test_case: String,
        #[case] expected: i32,
    ) -> std::io::Result<()> {
        let generated = generate_code(test_case);
        expect_exit_code(generated, expected)?;
        Ok(())
    }
}
