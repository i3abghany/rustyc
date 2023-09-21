use std::fs::{File, remove_file};
use std::io::Write;
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
