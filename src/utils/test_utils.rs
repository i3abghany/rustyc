use std::env;
use std::fs::{remove_file, File};
use std::io::{Read, Write};
use std::process::Command;
use uuid::Uuid;
pub fn expect_exit_code(source: String, expected: i32) -> std::io::Result<()> {
    // Instead of using named temp files, normal files are
    // created in the same directory to avoid permissions-
    // related problems in the CI pipelines.

    let id = Uuid::new_v4();
    let src_path = format!("./{}.s", id);
    let exe_path = format!("./{}.exe", id);

    let mut src = File::create(&src_path)?;
    src.write_all(source.as_bytes())?;

    // FIXME the created files may not get deleted in case
    //  of an error in the execution of one of the commands
    let mut output = Command::new("gcc")
        .arg(&src_path)
        .arg("-o")
        .arg(&exe_path)
        .output()
        .expect("Failed to compile generated code");
    let exit_code = output.status;
    remove_file(&src_path)?;
    assert!(exit_code.success());

    let mut output = Command::new(&exe_path)
        .output()
        .expect("Failed to execute generated executable");
    let exit_code = output.status.code().unwrap();
    remove_file(&exe_path)?;

    // Some platforms return an int32 while others only
    // return the least significant byte. We truncate
    // both down to their least significant byte for
    // tests to be consistent across platforms.
    assert_eq!(expected % 256, exit_code % 256);

    Ok(())
}

#[derive(Debug)]
pub struct TestCase {
    pub name: String,
    pub source: String,
    pub expected: i32,
}

pub fn parse_test_file(path: &str) -> Vec<TestCase> {
    let mut file = File::open(path).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let test_strings = contents.split("// CASE ").skip(1).collect::<Vec<_>>();
    let mut result = vec![];
    for test_string in test_strings {
        let mut lines = test_string.lines();
        let name = lines.next().unwrap().clone().to_string();
        let expected = lines
            .next()
            .unwrap()
            .strip_prefix("// RETURNS ")
            .unwrap()
            .parse::<i32>()
            .unwrap();
        let source = lines.collect::<Vec<_>>().join("\n");
        result.push(TestCase {
            name,
            source,
            expected,
        });
    }
    result
}

pub fn interpret_llvm_ir(ir: &str) -> i32 {
    let id = Uuid::new_v4();
    let ir_path = format!("./{}.ll", id);
    let mut ir_file = File::create(&ir_path).unwrap();
    ir_file.write_all(ir.as_bytes()).unwrap();

    let mut output = Command::new("lli")
        .arg("-opaque-pointers")
        .arg(&ir_path)
        .output()
        .expect("Failed to compile generated code");
    let exit_code = output.status;
    remove_file(&ir_path).unwrap();
    return exit_code.code().unwrap();
}
