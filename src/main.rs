mod code_generation;
mod lexical_analysis;
mod syntax_analysis;
mod utils;

use clap::Parser;
use crate::utils::compilation;

#[derive(Parser, Debug)]
#[clap(about, long_about = None)]
struct Args {
    #[clap(short = 'l', long, value_parser)]
    emit_llvm: bool,

    #[clap(short = 'c', long, value_parser)]
    emit_object: bool,

    #[clap(short = 's', long, value_parser)]
    emit_asm: bool,

    #[clap(short = 'o', long, value_parser)]
    exe_filename: Option<String>,

    #[clap(value_parser)]
    input_source_files: Vec<String>,
}

fn main() {
    let mut args = Args::parse();

    let exclusive_args_count = args.emit_llvm as usize
        + args.emit_object as usize
        + args.emit_asm as usize
        + args.exe_filename.is_some() as usize;

    if exclusive_args_count > 1 {
        eprintln!("Error: cannot specify more than one option from [-l, -c, -s, -o] at one time");
        return;
    } else if exclusive_args_count == 0 {
        args.exe_filename = Some("a.out".to_string());
    }

    if args.input_source_files.is_empty() {
        eprintln!("Error: at least one source file has to be provided");
        return;
    }

    let compilation = compilation::Compilation::new();
    for filename in &args.input_source_files {
        let source = std::fs::read_to_string(&filename).unwrap();

        let name_components = filename.split('.').collect::<Vec<&str>>();
        let stripped_filename: String;
        if name_components.len() >= 2 {
            stripped_filename = name_components[..name_components.len() - 1].join(".");
        } else {
            stripped_filename = name_components[0].to_string();
        }

        if args.emit_llvm {
            compilation.create_llvm_ir_file(&source, &format!("{}.ll", stripped_filename));
        } else if args.emit_object {
            compilation.create_object_file(&source, &format!("{}.o", stripped_filename));
        } else if args.emit_asm {
            compilation.create_asm_file(&source, &format!("{}.s", stripped_filename));
        }
    }

    if args.exe_filename.is_some() {
        let mut sources = vec![];

        for filename in &args.input_source_files {
            let source = std::fs::read_to_string(&filename).unwrap();
            sources.push(source);
        }

        compilation.create_executable_file(sources, &args.exe_filename.unwrap());
    }
}
