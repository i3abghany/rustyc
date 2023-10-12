use crate::ast::ASTNode::*;
use crate::ast::*;
pub trait CodeGenerator {
    fn generate(&mut self, root: &ASTNode) -> String {
        match root {
            TranslationUnit(..) => self.generate_translation_unit(root),
            ReturnStatement(..) => self.generate_return_statement(root),
            VariableDeclaration(..) => self.generate_variable_declaration(root),
            VariableDefinition(..) => self.generate_variable_definition(root),
            FunctionDeclaration(..) => self.generate_function_declaration(root),
            FunctionDefinition(..) => self.generate_function_definition(root),
            ExpressionNode(expression) => self.generate_expression(expression),
            Scope(..) => self.generate_scope(root),
            If(..) => self.generate_if_statement(root),
            While(..) => self.generate_while(root),
            DoWhile(..) => self.generate_do_while(root),
            ExpressionStatement(..) => self.generate_expression_statement(root),
            For(..) => self.generate_for(root),
        }
    }

    fn generate_translation_unit(&mut self, node: &ASTNode) -> String;
    fn generate_return_statement(&mut self, node: &ASTNode) -> String;
    fn generate_variable_declaration(&mut self, node: &ASTNode) -> String;
    fn generate_variable_definition(&mut self, node: &ASTNode) -> String;
    fn generate_function_declaration(&mut self, node: &ASTNode) -> String;
    fn generate_function_definition(&mut self, node: &ASTNode) -> String;
    fn generate_expression(&mut self, expression: &Expression) -> String;
    fn generate_scope(&mut self, scope: &ASTNode) -> String;
    fn generate_if_statement(&mut self, node: &ASTNode) -> String;
    fn generate_while(&mut self, while_node: &ASTNode) -> String;
    fn generate_do_while(&mut self, node: &ASTNode) -> String;
    fn generate_expression_statement(&mut self, node: &ASTNode) -> String;
    fn generate_for(&mut self, node: &ASTNode) -> String;
}

#[cfg(test)]
mod tests {
    use crate::code_generator::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::test_utils::*;
    use crate::x86_generator::X86CodeGenerator;

    fn generate_code(src: String) -> String {
        let tokens = Lexer::new(src).lex();
        let ast = Parser::new(tokens).parse();
        let mut generator = X86CodeGenerator::new();
        let generated = generator.generate(&ast);
        return generated;
    }

    #[rstest::rstest]
    #[case(
        "int main() { return 5 + 3 * 2 + (2 * 19 * 4) / 2 + 9 * 12 / 3 * 3; }",
        195
    )]
    #[case("int main() { int x = 1; int y = 2; return x > y; }", 0)]
    #[case("int main() { int x = 1; int y = 2; return x < y; }", 1)]
    #[case("int main() { int x = 1; int y = 2; return x >= y; }", 0)]
    #[case("int main() { int x = 1; int y = 2; return x <= y; }", 1)]
    #[case("int main() { int x = 1; int y = 2; return x != y; }", 1)]
    #[case(
        "int main() { int x = 1; int y = 2; int z = 1; return x + z == y; }",
        1
    )]
    #[case("int main() { int x = 1; int y = x; return x == y; }", 1)]
    #[case("int main() { return (1 | 2 | 4 | 8 | 16 | 32) & 85; }", 21)]
    #[case("int main() { int x = 12; int y = 423; return x || y; }", 1)]
    #[case("int main() { int x = -7; int y = 15; return x + y; }", 8)]
    #[case("int main() { int x = -----++++----+------12; return x * x; }", 144)]
    #[case("int main() { int x = 1; return -(+(-(+x))); }", 1)]
    #[case(
        "int main() { int false = 0; int true = 123; return true || false; }",
        1
    )]
    #[case(
        "int main() { int x = 312; int y = 99; int z; z = 2 * x / 3 + y * y; return z - x; }",
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
    #[case("int main() { int x = 55; { int y = 5; x = y; } return x; }", 5)]
    #[case("int main() { int x = 6; { int y = 55; return y; } return x; }", 55)]
    #[case("int main() { int x = 6; { int y = 55; return y; } }", 55)]
    #[case("int main() { int x = 1; { int x = 2; { int x = 3; return x; } } }", 3)]
    #[case("int main() { int x = 1; { int y = 2; { return x; } } }", 1)]
    #[case("int main() { {{{{{{{{{{ return 5; }}}}}}}}}} }", 5)]
    #[case("int main() { {{{{{{ }}}} return 1; }} }", 1)]
    #[case(
        "int main() { int x = 1; { int y = 5; } { int y = 6; } int y = 7; { int y = 8; return y; } }",
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
    #[case("int main() { { int x = 4; } return x; }")]
    #[case("int main() { { int x = 4; } { int y = 5; int z = 6; } return x + z; }")]
    #[case("int main() { {{{{{{ }}}} return 1; } }")]
    #[case("int main() { if (1) { int x = 1; } return x; }")]
    #[case("int main() { if (1) { int x = 1; } else if (0) { int y = 1; } return y; }")]
    #[case("int main() { if (1) int x = 1; return x; }")]
    #[case("int main() { while (0) { int x = 1; } return x; }")]
    #[case("int main() { while (0) int x = 1; return x; }")]
    #[case("int main() { do { int x; } while (1); return x; }")]
    #[should_panic]
    fn test_undefined_variables_in_scope(#[case] test_case: String) {
        let generated = generate_code(test_case);
    }

    #[rstest::rstest]
    #[case("int main() { int x = 55; if (x & 1) { return 16; } return x; }", 16)]
    #[case("int main() { int x = 55; if (x & 0) { return 16; } return x; }", 55)]
    #[case(
        "int main() { int x = 12; if (x & 1) { return 16; } else { return 5 * 12; } }",
        60
    )]
    #[case(
        "int main() {  if (0) { return 16; } else if (1) { return 5 * 12; } else { return 7; } }",
        60
    )]
    fn test_if_statements(#[case] test_case: String, #[case] expected: i32) -> std::io::Result<()> {
        let generated = generate_code(test_case);
        expect_exit_code(generated, expected)?;
        Ok(())
    }

    #[rstest::rstest]
    #[case(
        "int main() { int res = 0; for (int i = 0; i <= 5; i = i + 1) { res = res + i * i; } return res; }",
        55
    )]
    #[case(
        "int main() {
            int a = 0;
            int b = 1;
            int c;
            for (int i = 0; i <= 44; i = i + 1) {
                c = b + a;
                a = b;
                b = c;
            }
            return c;
        }",
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
        "int main() {
            int x = 44;
            int a = 0;
            int b = 1;
            int c;
            while (x >= 0) {
               c = a + b;
               a = b;
               b = c;
               x = x - 1;
            }
            return c;
        }",
        1836311903
    )]
    #[case(
        "int main() { int x = 5; int sum = 0; while (x != 0) { sum = sum + x * x; x = x - 1; } return sum; }",
        55
    )]
    #[case(
        "int main() {
            int n = 10;
            int result = 1;
            while (n > 1) {
                result = result * n;
                n = n - 1;
            }
            return result;
        }",
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
        "int main() {\
            int x = 45;\
            int a = 0;\
            int b = 1;\
            int c;\
            do {\
               c = a + b;\
               a = b;\
               b = c;\
               x = x - 1;\
            } while (x);\
            return c;\
        }",
        1836311903
    )]
    #[case(
        "int main() {
            int x = 6;
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

            return result;
        }",
        465
    )]
    #[case(
        "int main() { int x = 5; int sum = 0; do { sum = sum + x * x; x = x - 1; } while (x); return sum; }",
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

    #[rstest::rstest]
    #[case("int not_main() { return 1; }")]
    #[case("return 1;")]
    #[should_panic]
    fn test_program_without_main(#[case] test_case: String) {
        let generated = generate_code(test_case);
        expect_exit_code(generated, 1).unwrap();
    }

    #[rstest::rstest]
    #[case(
        "int f(int y, int x) { return x + y; }

        int main() {
                return f(6, 7);
        }",
        13
    )]
    #[case(
        "
        int add(int y, int x) {
            return x + y;
        }

        int ident(int x) { return x; }

        int fib(int n) {

            int a = 0;
            int b = 1;
            int c;
            while (n >= 0) {
                c = add(a, b);
                a = ident(b);
                b = ident(ident(c));
                n = ident(n - 1);
            }
            return ident(c);
        }

        int main() {
            return fib(3) * fib(5) - fib(2);
        }",
        62
    )]
    fn test_function_invocation(
        #[case] test_case: String,
        #[case] expected: i32,
    ) -> std::io::Result<()> {
        let generated = generate_code(test_case);
        expect_exit_code(generated, expected)?;
        Ok(())
    }
}
