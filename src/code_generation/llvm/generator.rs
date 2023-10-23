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
            // Scope(..) => self.generate_scope(node),
            // If(..) => self.generate_if_statement(node),
            // While(..) => self.generate_while(node),
            // DoWhile(..) => self.generate_do_while(node),
            // ExpressionStatement(..) => self.generate_expression_statement(node),
            // For(..) => self.generate_for(node),
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
                // TODO add the function to the symbol table.
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
            _ => todo!(),
        }
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

    fn generate_scope(&mut self, scope: &ASTNode) -> String {
        todo!()
    }

    fn generate_if_statement(&mut self, node: &ASTNode) -> String {
        todo!()
    }

    fn generate_while(&mut self, while_node: &ASTNode) -> String {
        todo!()
    }

    fn generate_do_while(&mut self, node: &ASTNode) -> String {
        todo!()
    }

    fn generate_expression_statement(&mut self, node: &ASTNode) -> String {
        todo!()
    }

    fn generate_for(&mut self, node: &ASTNode) -> String {
        todo!()
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
                test_case.expected, exit_code,
                "Test case: {} -- Expected: {}, found: {}",
                test_case.name, test_case.expected, exit_code
            );
        }
    }

    #[test]
    fn test_variable_declarations_and_definitions() {
        run_tests_from_file("./src/tests/variables.c");
    }

    #[test]
    #[should_panic]
    fn test_erroneous_variable_declarations_and_definitions() {
        run_tests_from_file("./src/tests/variables_error.c");
    }
}