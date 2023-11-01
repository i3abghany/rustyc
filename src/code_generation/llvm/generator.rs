use crate::code_generation::code_generator::*;
use crate::code_generation::llvm::symbol_table::*;
use crate::lexical_analysis::tokens::*;
use crate::syntax_analysis::ast::ASTNode::*;
use crate::syntax_analysis::ast::*;
use std::fmt::Pointer;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::*;
use inkwell::values::*;

pub struct LLVMGenerator<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    symbol_table: SymbolTable<'ctx>,
    current_function: Option<FunctionValue<'ctx>>, // The function currently being generated
    counter: i32,                                  // For naming labels of blocks
}

impl<'ctx> LLVMGenerator<'ctx> {
    pub fn new(context: &'ctx mut Context) -> Self {
        let builder = context.create_builder();
        let module = context.create_module("main");
        let symbol_table = SymbolTable::new();

        Self {
            context,
            builder,
            module,
            symbol_table,
            current_function: None,
            counter: 0,
        }
    }
}

impl<'ctx> CodeGenerator for LLVMGenerator<'ctx> {
    fn generate(&mut self, node: &ASTNode) -> String {
        let _ = self.generate_internal(node);
        self.module.print_to_string().to_string()
    }
}

impl<'ctx> LLVMGenerator<'ctx> {
    fn generate_internal(&mut self, node: &ASTNode) -> impl AnyValue<'ctx> {
        match node {
            TranslationUnit(..) => self.generate_translation_unit(node).as_any_value_enum(),
            ReturnStatement(..) => self.generate_return_statement(node).as_any_value_enum(),
            VariableDeclaration(..) => self.generate_variable_declaration(node).as_any_value_enum(),
            VariableDefinition(..) => self.generate_variable_definition(node).as_any_value_enum(),
            FunctionDeclaration(..) => self.generate_function_declaration(node).as_any_value_enum(),
            FunctionDefinition(..) => self.generate_function_definition(node).as_any_value_enum(),
            ExpressionNode(expression) => self.generate_expression(expression).as_any_value_enum(),
            Scope(..) => self.generate_scope(node).as_any_value_enum(),
            If(..) => self.generate_if_statement(node).as_any_value_enum(),
            While(..) => self.generate_while(node).as_any_value_enum(),
            DoWhile(..) => self.generate_do_while(node).as_any_value_enum(),
            ExpressionStatement(..) => self.generate_expression_statement(node).as_any_value_enum(),
            For(..) => self.generate_for(node).as_any_value_enum(),
            _ => panic!(),
        }
    }
    pub fn generate_translation_unit(&mut self, node: &ASTNode) -> IntValue<'ctx> {
        match node {
            TranslationUnit(statements) => {
                for statement in statements {
                    self.generate_internal(&statement);
                }
            }
            _ => panic!(
                "Internal error: expected translation unit, found: {:?}",
                node
            ),
        }
        // FIXME this is a placeholder as this function does not generate LLVM objects.
        self.context.i32_type().const_int(0, false)
    }

    fn is_in_global_scope(&self) -> bool {
        self.symbol_table.scopes.len() == 0
    }

    fn generate_variable_declaration(&mut self, node: &ASTNode) -> PointerValue<'ctx> {
        match node {
            VariableDeclaration(variable_type, identifier) => {
                let variable_type = variable_type.value.as_str();
                if self.is_in_global_scope() {
                    if let Some(variable) =
                        self.symbol_table.find_in_global_scope(&identifier.value)
                    {
                        return variable.pointer;
                    }

                    let g = self.module.add_global(
                        self.get_llvm_type_from_string(variable_type),
                        None,
                        identifier.value.as_str(),
                    );

                    // TODO set the alignment depending on the size of the type.
                    g.set_initializer(&self.get_llvm_type_from_string(variable_type).const_zero());

                    let g = g.as_pointer_value();
                    self.symbol_table.insert_global(
                        &identifier.value,
                        &Variable {
                            pointer: g,
                            variable_type: self.get_llvm_type_from_string(variable_type),
                            is_initialized: false,
                        },
                    );
                    g
                } else {
                    if self
                        .symbol_table
                        .find_in_current_scope(&identifier.value)
                        .is_some()
                    {
                        panic!("Variable {} already defined", identifier.value);
                    }

                    let alloca = self.generate_alloca_instruction(
                        &self.builder.get_insert_block().unwrap(),
                        identifier.value.as_str(),
                        variable_type,
                    );
                    self.symbol_table.insert(
                        &identifier.value,
                        &Variable {
                            pointer: alloca,
                            variable_type: self.get_llvm_type_from_string(variable_type),
                            is_initialized: false,
                        },
                    );
                    alloca
                }
            }
            _ => panic!(
                "Internal error: expected variable declaration, found: {:?}",
                node
            ),
        }
    }

    fn generate_variable_definition(&mut self, node: &ASTNode) -> PointerValue<'ctx> {
        match node {
            VariableDefinition(variable_type, identifier, expression) => {
                if self.is_in_global_scope() {
                    let decl = self.symbol_table.find_in_global_scope(&identifier.value);
                    match decl {
                        Some(decl) => {
                            if decl.is_initialized {
                                panic!("Variable {} already defined", identifier.value);
                            } else {
                                let global_var =
                                    self.module.get_global(identifier.value.as_str()).unwrap();
                                match expression.as_ref() {
                                    ExpressionNode(expression) => {
                                        let generated_expression =
                                            self.generate_expression(expression);
                                        global_var.set_initializer(&generated_expression);
                                    }
                                    _ => panic!(
                                        "Internal error: expected expression node, found: {:?}",
                                        node
                                    ),
                                };
                                return global_var.as_pointer_value();
                            }
                        }
                        None => {}
                    }

                    let variable_type = variable_type.value.as_str();
                    let g = self.module.add_global(
                        self.get_llvm_type_from_string(variable_type),
                        None,
                        identifier.value.as_str(),
                    );

                    // FIXME make sure that the initializer is evaluated at compile time.
                    let g = match expression.as_ref() {
                        ExpressionNode(expression) => {
                            let generated_expression = self.generate_expression(expression);
                            g.set_initializer(&generated_expression);
                            g
                        }
                        _ => panic!(
                            "Internal error: expected expression node, found: {:?}",
                            node
                        ),
                    };

                    let g = g.as_pointer_value();
                    self.symbol_table.insert_global(
                        &identifier.value,
                        &Variable {
                            pointer: g,
                            variable_type: self.get_llvm_type_from_string(variable_type),
                            is_initialized: true,
                        },
                    );
                    g
                } else {
                    if self
                        .symbol_table
                        .find_in_current_scope(&identifier.value)
                        .is_some()
                    {
                        panic!("Variable {} already defined", identifier.value);
                    }

                    let variable_type = variable_type.value.as_str();
                    let alloca = self.generate_alloca_instruction(
                        &self.builder.get_insert_block().unwrap(),
                        identifier.value.as_str(),
                        variable_type,
                    );
                    match expression.as_ref() {
                        ExpressionNode(expression) => {
                            let generated_expression = self.generate_expression(expression);
                            self.builder
                                .build_store(alloca, generated_expression)
                                .unwrap();
                            self.symbol_table.insert(
                                &identifier.value,
                                &Variable {
                                    pointer: alloca,
                                    variable_type: self.get_llvm_type_from_string(variable_type),
                                    is_initialized: true,
                                },
                            );
                            alloca
                        }
                        _ => panic!(
                            "Internal error: expected expression node, found: {:?}",
                            node
                        ),
                    }
                }
            }
            _ => panic!(
                "Internal error: expected variable definition, found: {:?}",
                node
            ),
        }
    }

    fn generate_function_declaration(&mut self, node: &ASTNode) -> FunctionValue<'ctx> {
        match node {
            FunctionDeclaration(return_type, name, params) => {
                let mut param_types = Vec::new();
                for param in params {
                    match param {
                        VariableDeclaration(type_token, _) => {
                            param_types.push(
                                self.get_llvm_type_from_string(type_token.value.as_str())
                                    .into(),
                            );
                        }
                        _ => panic!(
                            "Internal error: expected variable declaration, found: {:?}",
                            param
                        ),
                    }
                }
                let return_type = self.get_llvm_type_from_string(return_type.value.as_str());
                let function_type = return_type.fn_type(&param_types, false);
                let function = self
                    .module
                    .add_function(name.value.as_str(), function_type, None);
                for (i, arg) in function.get_param_iter().enumerate() {
                    match &params[i] {
                        VariableDeclaration(_, name) => {
                            arg.set_name(name.value.as_str());
                        }
                        _ => panic!(
                            "Internal error: expected variable declaration, found: {:?}",
                            params[i]
                        ),
                    }
                }
                function
            }
            _ => panic!(
                "Internal error: expected function declaration, found: {:?}",
                node
            ),
        }
    }

    fn generate_function_definition(&mut self, node: &ASTNode) -> FunctionValue<'ctx> {
        match node {
            FunctionDefinition(return_type, name, params, body) => {
                let declaration = self.generate_function_declaration(&FunctionDeclaration(
                    return_type.clone(),
                    name.clone(),
                    params.clone(),
                ));

                self.current_function = Some(declaration);

                self.symbol_table.push_scope();

                let basic_block = self.context.append_basic_block(declaration, "entry");
                self.builder.position_at_end(basic_block);

                for (i, arg) in declaration.get_param_iter().enumerate() {
                    match &params[i] {
                        VariableDeclaration(variable_type, name) => {
                            let variable_type = variable_type.value.as_str();
                            let alloca = self.generate_alloca_instruction(
                                &basic_block,
                                name.value.as_str(),
                                variable_type,
                            );
                            self.builder.build_store(alloca, arg).unwrap();
                            self.symbol_table.insert(
                                &name.value,
                                &Variable {
                                    pointer: alloca,
                                    variable_type: self.get_llvm_type_from_string(variable_type),
                                    is_initialized: true,
                                },
                            );
                        }
                        _ => panic!(
                            "Internal error: expected variable declaration, found: {:?}",
                            params[i]
                        ),
                    }
                }

                match body.as_ref() {
                    Scope(statements) => {
                        for statement in statements {
                            self.generate_internal(&statement);
                        }
                    }
                    _ => panic!("Internal error: expected scope, found: {:?}", body),
                }
                self.symbol_table.pop_scope();
                return declaration;
            }
            _ => panic!(),
        }
    }

    fn generate_alloca_instruction(
        &mut self,
        entry_block: &BasicBlock,
        name: &str,
        variable_type: &str,
    ) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        match entry_block.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(*entry_block),
        }
        let llvm_type = self.get_llvm_type_from_string(variable_type);
        builder.build_alloca(llvm_type, name).unwrap()
    }

    fn generate_return_statement(&mut self, node: &ASTNode) -> InstructionValue<'ctx> {
        match node {
            ReturnStatement(_, expression_node) => match expression_node.as_ref() {
                ExpressionNode(expression) => {
                    let generated_expression = self.generate_expression(expression);
                    self.builder
                        .build_return(Some(&generated_expression))
                        .unwrap()
                }
                _ => panic!(
                    "Internal error: expected expression node, found: {:?}",
                    node
                ),
            },
            _ => panic!(
                "Internal error: expected return statement, found: {:?}",
                node
            ),
        }
    }

    fn generate_expression(&mut self, expression: &Expression) -> BasicValueEnum<'ctx> {
        match expression {
            Expression::IntegerLiteral { .. } => self.generate_integer_literal(expression),
            Expression::Variable(name) => self.generate_variable_expression(name),
            Expression::Binary(operator, lhs, rhs) => {
                self.generate_binary_expression(operator, lhs, rhs)
            }
            Expression::Unary(operator, operand) => {
                self.generate_unary_expression(operator, operand)
            }
            Expression::Assignment(lhs, rhs) => self.generate_assignment(lhs, rhs),
            _ => todo!(),
        }
    }

    fn generate_unary_expression(
        &mut self,
        operator: &Token,
        operand: &Expression,
    ) -> BasicValueEnum<'ctx> {
        let operand = self.generate_expression(operand).into_int_value();
        match operator.token_type {
            TokenType::Minus => self.builder.build_int_neg(operand, "temp_neg"),
            TokenType::Plus => Ok(operand),
            TokenType::Bang => {
                let operand = self
                    .builder
                    .build_int_cast(operand, self.context.bool_type(), "bool_operand")
                    .unwrap();
                self.builder.build_not(operand, "temp_not")
            }
            _ => panic!(),
        }
        .unwrap()
        .as_basic_value_enum()
    }

    fn generate_assignment(&mut self, lhs: &Token, rhs: &Expression) -> BasicValueEnum<'ctx> {
        if self.is_in_global_scope() {
            panic!(
                "Assignment to variable {} not allowed in global scope",
                lhs.value
            );
        }
        if self.symbol_table.find_hierarchically(&lhs.value).is_none() {
            panic!("Reference to undefined variable `{}`", lhs.value);
        }

        let rhs = self.generate_expression(&rhs);

        self.builder
            .build_store(
                self.symbol_table
                    .find_hierarchically(&lhs.value)
                    .unwrap()
                    .pointer,
                rhs,
            )
            .unwrap();

        rhs.clone()
    }

    fn generate_binary_expression(
        &mut self,
        token: &Token,
        lhs: &Expression,
        rhs: &Expression,
    ) -> BasicValueEnum<'ctx> {
        let lhs = self.generate_expression(lhs).into_int_value();
        let rhs = self.generate_expression(rhs).into_int_value();
        match token.token_type {
            TokenType::EqualsEquals => {
                self.builder
                    .build_int_compare(inkwell::IntPredicate::EQ, lhs, rhs, "bool_value")
            }
            TokenType::NotEquals => {
                self.builder
                    .build_int_compare(inkwell::IntPredicate::NE, lhs, rhs, "bool_value")
            }
            TokenType::GreaterThan => {
                self.builder
                    .build_int_compare(inkwell::IntPredicate::SGT, lhs, rhs, "bool_value")
            }
            TokenType::GreaterThanEquals => {
                self.builder
                    .build_int_compare(inkwell::IntPredicate::SGE, lhs, rhs, "bool_value")
            }
            TokenType::LessThan => {
                self.builder
                    .build_int_compare(inkwell::IntPredicate::SLT, lhs, rhs, "bool_value")
            }
            TokenType::LessThanEquals => {
                self.builder
                    .build_int_compare(inkwell::IntPredicate::SLE, lhs, rhs, "bool_value")
            }
            TokenType::Plus => self.builder.build_int_add(lhs, rhs, "temp_add"),
            TokenType::Minus => self.builder.build_int_sub(lhs, rhs, "temp_sub"),
            TokenType::Star => self.builder.build_int_mul(lhs, rhs, "temp_mul"),
            TokenType::Slash => self.builder.build_int_signed_div(lhs, rhs, "temp_div"),
            TokenType::And => self.builder.build_and(lhs, rhs, "temp_and"),
            TokenType::Bar => self.builder.build_or(lhs, rhs, "temp_or"),
            TokenType::Caret => self.builder.build_xor(lhs, rhs, "temp_xor"),
            TokenType::AndAnd | TokenType::BarBar => {
                let lhs = self
                    .builder
                    .build_int_cast(lhs, self.context.bool_type(), "bool_lhs")
                    .unwrap();
                let rhs = self
                    .builder
                    .build_int_cast(rhs, self.context.bool_type(), "bool_rhs")
                    .unwrap();
                if token.token_type == TokenType::AndAnd {
                    self.builder.build_and(lhs, rhs, "temp_logical_and")
                } else {
                    self.builder.build_or(lhs, rhs, "temp_logical_or")
                }
            }
            _ => panic!(),
        }
        .unwrap()
        .as_basic_value_enum()
    }

    fn generate_variable_expression(&mut self, name: &Token) -> BasicValueEnum<'ctx> {
        if let Some(variable) = self.symbol_table.find(&name.value) {
            self.builder
                .build_load(
                    variable.variable_type,
                    variable.pointer,
                    name.value.as_str(),
                )
                .unwrap()
        } else if let Some(global_variable) = self.symbol_table.find_in_global_scope(&name.value) {
            self.builder
                .build_load(
                    global_variable.variable_type,
                    global_variable.pointer,
                    name.value.as_str(),
                )
                .unwrap()
        } else {
            panic!("Reference to undefined variable `{}`", name.value);
        }
    }

    fn generate_integer_literal(&mut self, expression: &Expression) -> BasicValueEnum<'ctx> {
        match expression {
            Expression::IntegerLiteral(token) => self
                .context
                .i32_type()
                .const_int(token.value.parse().unwrap(), false)
                .as_basic_value_enum(),
            _ => panic!(),
        }
    }
    fn generate_scope(&mut self, node: &ASTNode) -> IntValue<'ctx> {
        self.symbol_table.push_scope();

        let statements = match node {
            Scope(statements) => statements,
            _ => panic!(
                "Internal error: expected translation unit, found: {:?}",
                node
            ),
        };

        for statement in statements {
            self.generate(statement);
        }

        self.symbol_table.pop_scope();

        self.context.i32_type().const_int(0, false)
    }

    fn generate_if_statement(&mut self, node: &ASTNode) -> IntValue<'ctx> {
        let (condition_node, then_node, else_node) = match node {
            If(_, condition_node, then_node, else_node) => (condition_node, then_node, else_node),
            _ => panic!(),
        };

        let counter = self.counter;

        let then_block = self
            .context
            .append_basic_block(self.current_function.unwrap(), &format!("then_{counter}"));
        let end_block = self
            .context
            .append_basic_block(self.current_function.unwrap(), &format!("if_end_{counter}"));
        let else_block = if else_node.is_some() {
            self.context
                .append_basic_block(self.current_function.unwrap(), &format!("else_{counter}"))
        } else {
            end_block
        };

        self.counter += 1;

        let condition = match condition_node.as_ref() {
            ASTNode::ExpressionNode(expression) => expression,
            _ => panic!(),
        };

        let cond_result = self.generate_expression(condition);

        let zero = self.context.i32_type().const_int(0, false);
        let i32_value = self
            .builder
            .build_int_z_extend(
                cond_result.into_int_value(),
                self.context.i32_type(),
                "extended_condition",
            )
            .unwrap();
        let bool_value = self
            .builder
            .build_int_compare(inkwell::IntPredicate::NE, i32_value, zero, "bool_value")
            .unwrap();
        self.builder
            .build_conditional_branch(bool_value, then_block, else_block)
            .unwrap();

        self.builder.position_at_end(then_block);

        self.generate(then_node);
        self.builder.build_unconditional_branch(end_block).unwrap();

        if else_node.is_some() {
            self.builder.position_at_end(else_block);
            self.generate(else_node.as_ref().unwrap());
            self.builder.build_unconditional_branch(end_block).unwrap();
        }

        self.builder.position_at_end(end_block);

        zero
    }

    fn generate_while(&mut self, node: &ASTNode) -> IntValue<'ctx> {
        let counter = self.counter;
        let cond_block = self.context.append_basic_block(
            self.current_function.unwrap(),
            &format!("while_condition_{counter}"),
        );
        let body_block = self.context.append_basic_block(
            self.current_function.unwrap(),
            &format!("while_body_{counter}"),
        );
        let end_block = self.context.append_basic_block(
            self.current_function.unwrap(),
            &format!("while_end_{counter}"),
        );

        self.counter += 1;

        self.builder.build_unconditional_branch(cond_block).unwrap();

        self.builder.position_at_end(cond_block);

        let (condition_node, body_node) = match node {
            While(_, condition_node, body_node) => (condition_node, body_node),
            _ => panic!(),
        };

        self.while_loop_common(
            condition_node,
            body_node,
            &cond_block,
            &body_block,
            &end_block,
        )
    }

    fn generate_do_while(&mut self, node: &ASTNode) -> IntValue<'ctx> {
        let counter = self.counter;
        let cond_block = self.context.append_basic_block(
            self.current_function.unwrap(),
            &format!("do_while_condition_{counter}"),
        );
        let body_block = self.context.append_basic_block(
            self.current_function.unwrap(),
            &format!("do_while_body_{counter}"),
        );
        let end_block = self.context.append_basic_block(
            self.current_function.unwrap(),
            &format!("do_while_end_{counter}"),
        );

        self.counter += 1;

        self.builder.build_unconditional_branch(body_block).unwrap();

        self.builder.position_at_end(cond_block);

        let (condition_node, body_node) = match node {
            DoWhile(_, body_node, _, condition_node) => (condition_node, body_node),
            _ => panic!(),
        };

        self.while_loop_common(
            condition_node,
            body_node,
            &cond_block,
            &body_block,
            &end_block,
        )
    }

    fn while_loop_common(
        &mut self,
        condition_node: &ASTNode,
        body_node: &ASTNode,
        cond_block: &BasicBlock,
        body_block: &BasicBlock,
        end_block: &BasicBlock,
    ) -> IntValue<'ctx> {
        let condition = match condition_node {
            ASTNode::ExpressionNode(expression) => expression,
            _ => panic!(),
        };
        let cond_result = self.generate_expression(condition);
        let i32_value = self
            .builder
            .build_int_z_extend(
                cond_result.into_int_value(),
                self.context.i32_type(),
                "extended_condition",
            )
            .unwrap();
        let zero = self.context.i32_type().const_int(0, false);
        let bool_value = self
            .builder
            .build_int_compare(inkwell::IntPredicate::NE, i32_value, zero, "bool_value")
            .unwrap();

        self.builder
            .build_conditional_branch(bool_value, *body_block, *end_block)
            .unwrap();

        self.builder.position_at_end(*body_block);

        self.generate(body_node);
        self.builder
            .build_unconditional_branch(*cond_block)
            .unwrap();

        self.builder.position_at_end(*end_block);

        self.context.i32_type().const_int(0, false)
    }

    fn generate_expression_statement(&mut self, node: &ASTNode) -> BasicValueEnum<'ctx> {
        let expression = match node {
            ExpressionStatement(expression) => expression,
            _ => panic!(),
        };
        return self.generate_expression(expression);
    }

    fn generate_for(&mut self, node: &ASTNode) -> IntValue<'ctx> {
        let counter = self.counter;
        let cond_block = self.context.append_basic_block(
            self.current_function.unwrap(),
            &format!("for_condition_{counter}"),
        );
        let body_block = self.context.append_basic_block(
            self.current_function.unwrap(),
            &format!("for_body_{counter}"),
        );
        let end_block = self.context.append_basic_block(
            self.current_function.unwrap(),
            &format!("for_end_{counter}"),
        );

        self.counter += 1;

        let (init_node, cond_node, update_node, body_node) = match node {
            For(_, [init_node, cond_node, update_node], body_node) => {
                (init_node, cond_node, update_node, body_node)
            }
            _ => panic!(),
        };

        self.generate_internal(init_node);

        self.builder.build_unconditional_branch(cond_block).unwrap();

        self.builder.position_at_end(cond_block);

        let condition = match cond_node.as_ref() {
            ASTNode::ExpressionStatement(expression) => expression,
            _ => panic!(),
        };
        let cond_result = self.generate_expression(condition);
        let i32_value = self
            .builder
            .build_int_z_extend(
                cond_result.into_int_value(),
                self.context.i32_type(),
                "extended_condition",
            )
            .unwrap();
        let zero = self.context.i32_type().const_int(0, false);
        let bool_value = self
            .builder
            .build_int_compare(inkwell::IntPredicate::NE, i32_value, zero, "bool_value")
            .unwrap();
        self.builder
            .build_conditional_branch(bool_value, body_block, end_block)
            .unwrap();

        self.builder.position_at_end(body_block);
        self.generate_internal(body_node);

        self.generate_internal(update_node);

        self.builder.build_unconditional_branch(cond_block).unwrap();

        self.builder.position_at_end(end_block);

        self.context.i32_type().const_int(0, false)
    }

    fn get_llvm_type_from_string(&self, type_str: &str) -> BasicTypeEnum<'ctx> {
        match type_str {
            "int" => BasicTypeEnum::IntType(self.context.i32_type()),
            _ => panic!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::CodeGenerator;
    use crate::utils::test_utils::{interpret_llvm_ir, parse_test_file};
    use crate::{code_generation, lexical_analysis, syntax_analysis};
    use inkwell::context::Context;

    fn run_tests_from_file(path: &str) {
        let test_cases = parse_test_file(path);
        for test_case in test_cases {
            let mut context = Context::create();
            let tokens = lexical_analysis::lexer::Lexer::new(test_case.source).lex();
            let ast = syntax_analysis::parser::Parser::new(tokens).parse();
            let generated_ir =
                code_generation::llvm::generator::LLVMGenerator::new(&mut context).generate(&ast);
            let exit_code = interpret_llvm_ir(&generated_ir);
            assert_eq!(
                test_case.expected % 256,
                exit_code % 256,
                "Test case: {} -- Expected: {}, found: {}\nGenerated IR:\n{}",
                test_case.name,
                test_case.expected,
                exit_code,
                generated_ir
            );
        }
    }

    #[test]
    fn test_variable_declarations_and_definitions() {
        run_tests_from_file("./src/tests/variables.c");
    }

    #[test]
    fn test_operators() {
        run_tests_from_file("./src/tests/operators.c");
    }

    #[test]
    #[should_panic]
    fn test_erroneous_variable_declarations_and_definitions() {
        run_tests_from_file("./src/tests/variables_error.c");
    }

    #[test]
    fn test_assignment() {
        run_tests_from_file("./src/tests/assignment.c");
    }

    #[test]
    fn test_basic_if() {
        run_tests_from_file("./src/tests/if.c");
    }

    #[test]
    fn test_while() {
        run_tests_from_file("./src/tests/while.c");
    }

    #[test]
    fn test_do_while() {
        run_tests_from_file("./src/tests/do_while.c");
    }

    #[test]
    fn test_for() {
        run_tests_from_file("./src/tests/for.c");
    }
}
