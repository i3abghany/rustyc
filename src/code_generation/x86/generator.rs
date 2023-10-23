use crate::code_generation::code_generator::*;
use crate::code_generation::x86::symbol_table::{self, Symbol, SymbolTable};
use crate::lexical_analysis::tokens::*;
use crate::syntax_analysis::ast::ASTNode::*;
use crate::syntax_analysis::ast::*;
use uuid::Uuid;

pub struct X86CodeGenerator {
    symbol_table: SymbolTable,
}

impl CodeGenerator for X86CodeGenerator {
    fn generate(&mut self, node: &ASTNode) -> String {
        match node {
            TranslationUnit(..) => self.generate_translation_unit(node),
            ReturnStatement(..) => self.generate_return_statement(node),
            VariableDeclaration(..) => self.generate_variable_declaration(node),
            VariableDefinition(..) => self.generate_variable_definition(node),
            FunctionDeclaration(..) => self.generate_function_declaration(node),
            FunctionDefinition(..) => self.generate_function_definition(node),
            ExpressionNode(expression) => self.generate_expression(expression),
            Scope(..) => self.generate_scope(node),
            If(..) => self.generate_if_statement(node),
            While(..) => self.generate_while(node),
            DoWhile(..) => self.generate_do_while(node),
            ExpressionStatement(..) => self.generate_expression_statement(node),
            For(..) => self.generate_for(node),
        }
    }
}

impl X86CodeGenerator {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
        }
    }

    fn generate_scope_with_variables(
        &mut self,
        scope_node: &ASTNode,
        scope: symbol_table::Scope,
    ) -> String {
        let mut result = String::new();
        match scope_node {
            ASTNode::Scope(statements) => {
                self.symbol_table.push_scope(scope);
                for statement in statements {
                    result.push_str(self.generate(statement).as_str());
                }
                self.symbol_table.pop_scope();
                result
            }
            _ => panic!(),
        }
    }
    fn generate_translation_unit(&mut self, node: &ASTNode) -> String {
        let mut result = String::new();
        match node {
            TranslationUnit(nodes_vector) => {
                // TODO Support global variables
                // FIXME this is a scope for storing global FUNCTION declarations only
                //  and should not be used for global variables
                self.symbol_table.push_scope(symbol_table::Scope::new(0));
                for node in nodes_vector {
                    result.push_str(&self.generate(node));
                }
                self.symbol_table.pop_scope();
                result
            }
            _ => panic!("Internal Error: Expected program node, found {:?}", node),
        }
    }

    fn generate_return_statement(&mut self, node: &ASTNode) -> String {
        match node {
            ReturnStatement(_, expr_node) => {
                let mut result = self.generate(expr_node);
                result.push_str(&format!(
                    "mov %rbp, %rsp\nmov {}, %rax\npop %rbp\nret\n",
                    X86CodeGenerator::get_reg1(8)
                ));
                result
            }
            _ => panic!("Return: Expected a return node, found {:?}", node),
        }
    }

    fn generate_variable_declaration(&mut self, node: &ASTNode) -> String {
        match node {
            VariableDeclaration(variable_type, identifier) => {
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

                self.symbol_table
                    .insert_top(&identifier.value, &variable_type.value);
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
                self.generate_assignment(&Expression::Assignment(
                    identifier.clone(),
                    Box::new(expr.clone()),
                ))
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

    fn generate_function_declaration(&mut self, node: &ASTNode) -> String {
        match node {
            FunctionDeclaration(return_type, identifier, func_parameters) => {
                if self
                    .symbol_table
                    .get_at_current_scope(&identifier.value)
                    .is_some()
                {
                    panic!(
                        "Declaration: the function `{}` is already declared",
                        identifier.value
                    )
                }

                let mut parameters = Vec::new();
                for node in func_parameters {
                    if let VariableDeclaration(variable_type, ..) = node {
                        parameters.push(variable_type.value.clone());
                    } else if let FunctionDeclaration(..) = node {
                        todo!("Support function declaration as a parameter")
                    } else {
                        panic!("")
                    }
                }
                let symbol = Symbol::Function {
                    return_type: return_type.value.clone(),
                    parameters,
                };

                self.symbol_table.insert(&identifier.value, &symbol);
            }
            _ => panic!("Declaration: Expected declaration node, found {:?}", node),
        }
        "".to_string()
    }

    fn generate_function_definition(&mut self, node: &ASTNode) -> String {
        let mut result = String::new();

        if let FunctionDefinition(return_type, identifier, parameters, body) = node {
            self.generate_function_declaration(&FunctionDeclaration(
                return_type.clone(),
                identifier.clone(),
                parameters.clone(),
            ));

            let mut scope = symbol_table::Scope::default(&self.symbol_table);
            for param in parameters {
                if let VariableDeclaration(variable_type, name) = param {
                    scope.insert_top(name.value.as_str(), &variable_type.value);
                } else if let FunctionDeclaration(..) = node {
                    todo!("Support function declaration as a parameter")
                } else {
                    panic!("")
                }
            }

            self.symbol_table.reset_largest_offset();
            result.push_str(&self.generate_scope_with_variables(body, scope));

            // Quick hack. Instead of checking whether the function returns
            //  at the end, just inject a redundant return
            // TODO show warnings in case of mismatched returns

            format!(
                ".global {name}\n\
                    {name}:\n\
                    push %rbp\n\
                    mov %rsp, %rbp\n\
                    subq ${frame_size}, %rsp\n\
                    {body}\
                    # Redundant return ---\n\
                    {redundant_return}\
                    # --------------------\n\
                ",
                name = identifier.value,
                frame_size = -self.symbol_table.current_largest_offset(),
                body = result,
                redundant_return = X86CodeGenerator::generate_return_void()
            )
        } else {
            panic!(
                "Internal error: Expected variable definition, found {:?}",
                node
            )
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
            Expression::FunctionCall(..) => self.generate_function_call(expression),
            Expression::Parenthesized(internal_expression) => {
                self.generate_expression(internal_expression)
            }
        }
    }

    fn generate_scope(&mut self, scope: &ASTNode) -> String {
        self.generate_scope_with_variables(scope, symbol_table::Scope::default(&self.symbol_table))
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
    fn get_expression_size_in_bytes(exp: &Expression) -> usize {
        4
    }

    fn generate_function_call(&mut self, call: &Expression) -> String {
        let mut result = String::new();
        match call {
            Expression::FunctionCall(name, parameters) => {
                // TODO extract pointer size into a function (and use it in the symbol table too)
                let mut push_offset = -16; // return address + rbp
                for param in parameters {
                    let param_size = X86CodeGenerator::get_expression_size_in_bytes(param);
                    // Since we're dealing with the stack, subtraction of the offset occurs first
                    push_offset -= param_size as i32;
                    result.push_str(&format!(
                        "{computation}\
                         {mov} {result}, {offset}(%rsp)\n",
                        computation = self.generate_expression(param),
                        mov = X86CodeGenerator::mov_mnemonic(param_size),
                        result = X86CodeGenerator::get_reg1(param_size),
                        offset = push_offset
                    ));
                }
                result.push_str(&format!("call {}\n", name.value));
                result
            }
            _ => panic!("Exp"),
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
                        result.push_str(&format!("neg {}\n", X86CodeGenerator::get_reg1(8)));
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
                result.push_str(format!("push {}\n", X86CodeGenerator::get_reg1(8)).as_str());
                result.push_str(&self.generate_expression(left));
                result.push_str(format!("pop {}\n", X86CodeGenerator::get_reg2(8)).as_str());

                // TODO: Support floating point operations
                let reg1 = X86CodeGenerator::get_reg1(4);
                let reg2 = X86CodeGenerator::get_reg2(4);
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
                    X86CodeGenerator::get_reg1(8)
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
                        let mov_instruction = X86CodeGenerator::mov_mnemonic(definition.size());
                        format!(
                            "{} {}(%rbp), {}\n",
                            mov_instruction,
                            *stack_offset,
                            X86CodeGenerator::get_reg1(definition.size())
                        )
                    }
                    _ => panic!(),
                }
            }
            _ => panic!(
                "Internal Error: Expected variable expression, found: {:#?}",
                expression
            ),
        }
    }

    fn generate_return_void() -> &'static str {
        "mov %rbp, %rsp\npop %rbp\nret\n"
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
                    _ => panic!(),
                }

                // TODO support referential assignment
                let mov_instruction = X86CodeGenerator::mov_mnemonic(variable.size());
                let mut result = self.generate_expression(expr_node);

                result.push_str(&format!(
                    "{} {}, {}(%rbp)\n",
                    mov_instruction,
                    X86CodeGenerator::get_reg1(variable_size),
                    variable_stack_offset
                ));
                result
            }
            _ => panic!(),
        }
    }

    fn unique_label(&mut self, prefix: &str) -> String {
        return format!("{}_{}", prefix, Uuid::new_v4().simple());
    }

    fn generate_condition_block(&mut self, prefix: &str, condition: &str, body: &str) -> String {
        let mut results = String::new();
        results.push_str(condition);
        results.push_str(&format!("cmp $0, {}\n", X86CodeGenerator::get_reg1(8)));
        let else_label = self.unique_label(prefix);
        results.push_str(&format!("je {}\n", else_label));
        results.push_str(body);
        results.push_str(&format!("{}:\n", else_label));
        results
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
