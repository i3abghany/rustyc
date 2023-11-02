use crate::code_generation::code_generator::CodeGenerator;
use crate::code_generation::llvm::generator::LLVMGenerator;
use crate::lexical_analysis::lexer::Lexer;
use crate::syntax_analysis::parser::Parser;
use std::fs::{remove_file, File};
use std::io::Write;
use std::process::Command;
use uuid::Uuid;

pub struct Compilation;

impl Compilation {
    pub fn new() -> Self {
        Self
    }

    pub fn create_object_file(&self, c_src: &str, output_name: &str) {
        self.invoke_llc(c_src, "obj", output_name);
    }

    pub fn create_asm_file(&self, c_src: &str, output_name: &str) {
        self.invoke_llc(c_src, "asm", output_name);
    }

    pub fn create_llvm_ir_file(&self, c_src: &str, output_name: &str) {
        let llvm_ir = self.generate_llvm_ir(c_src);
        let mut ir_file = File::create(&output_name).unwrap();
        ir_file.write_all(llvm_ir.as_bytes()).unwrap();
    }

    pub fn create_executable_file(&self, sources: Vec<String>, output_name: &str) {
        let mut filenames = vec![];
        for source in sources {
            let id = Uuid::new_v4();
            let object_name = format!("./{}.o", id);
            self.create_object_file(source.as_str(), &object_name);
            filenames.push(object_name);
        }

        let output = Command::new("gcc")
            .arg("-o")
            .arg(output_name)
            .args(&filenames)
            .output()
            .expect("Failed to compile generated code");

        for filename in &filenames {
            remove_file(filename).unwrap();
        }

        assert_eq!(
            output.status.code().unwrap(),
            0,
            "Linking error: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    fn invoke_llc(&self, c_src: &str, filetype: &str, output_name: &str) {
        let llvm_ir = self.generate_llvm_ir(c_src);

        let id = Uuid::new_v4();
        let ir_path = format!("./{}.ll", id);
        let mut ir_file = File::create(&ir_path).unwrap();
        ir_file.write_all(llvm_ir.as_bytes()).unwrap();

        let output = Command::new("llc")
            .arg("-opaque-pointers")
            .arg(&format!("--filetype={}", filetype))
            .arg("-o")
            .arg(output_name)
            .arg(&ir_path)
            .output()
            .expect("Failed to compile generated code");

        remove_file(&ir_path).unwrap();

        assert_eq!(
            output.status.code().unwrap(),
            0,
            "llc error: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    fn generate_llvm_ir(&self, c_src: &str) -> String {
        let tokens = Lexer::new(c_src.to_string()).lex();
        let ast = Parser::new(tokens).parse();
        let mut llvm_context = inkwell::context::Context::create();
        let llvm_ir = LLVMGenerator::new(&mut llvm_context).generate(&ast);
        llvm_ir
    }
}
