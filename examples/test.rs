#![feature(phase)]

#[phase(plugin, link)]
extern crate eh;

use eh::{EhResult, ErrorData, print_error_stack};


struct FileNotFound {
    file: Option<Path>,
}

impl ErrorData for FileNotFound {
    fn name(&self) -> &str {
        "File not found"
    }

    fn detail(&self) -> Option<String> {
        match self.file {
            Some(ref file) => {
                Some(format!("file={}", file.display()))
            },
            None => None,
        }
    }

    fn default_description() -> &'static str {
        "Failed to locate file"
    }
}

struct RecordNotFound;

impl ErrorData for RecordNotFound {
    fn name(&self) -> &str {
        "Record not found"
    }
}


fn test() -> EhResult<()> {
    fail!(FileNotFound { file: Some(Path::new("/missing.txt")) });
}

fn bar() -> EhResult<()> {
    try!(test());
    Ok(())
}

fn baz() -> EhResult<()> {
    match bar() {
        Err(err) => fail!(RecordNotFound, "could not find record", err),
        Ok(x) => Ok(x),
    }
}

fn main() {
    match baz() {
        Ok(_) => println!("Produced a result"),
        Err(err) => print_error_stack(&err),
    }
}
