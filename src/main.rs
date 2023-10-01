mod ast;
mod code_generator;
mod lexer;
mod parser;
mod symbol_table;
mod test_utils;
mod tokens;

fn main() {
    let source = "for (i = 0; i < 10; i = i + 1;) { int x = 10; }";
    let tokens = lexer::Lexer::new(source.to_string()).lex();
    let ast = parser::Parser::new(tokens).parse();
    print!("{:#?}", ast)
}
