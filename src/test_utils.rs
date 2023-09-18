use std::fmt::format;
use std::fs::File;
use std::io::Write;
use std::process::Command;
use uuid::Uuid;
pub fn expect_return_code(source: String, expected: i32) {
    let id = Uuid::new_v4();
    let base_path = std::env::temp_dir();
    let source_path = format!("{}_{}.s", base_path.display(), id);
    let exe_path = format!("{}_{}.exe", base_path.display(), id);

    // FIXME: Make sure tempfiles are not deleted before being used by subsequent steps.
    {
        let mut executable = File::create(&exe_path).unwrap();
        let mut source_file = File::create(&source_path).unwrap();
        write!(source_file, "{}", source).unwrap();
    }
    let mut child = Command::new("gcc")
        .arg(&source_path)
        .arg("-o")
        .arg(&exe_path)
        .spawn()
        .expect("Failed to compile generated code");
    let exit_code = child.wait().unwrap();
    assert!(exit_code.success());

    let output = Command::new(exe_path)
        .output()
        .expect("Failed to execute generated executable");
    assert_eq!(expected, output.status.code().unwrap());
}
