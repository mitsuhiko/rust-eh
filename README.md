# rust-eh

This is an error handling experiment.  Primarily it tries to bring the
debuggability of Python tracebacks to Rust results.

Current status:

```
Traceback (most recent cause last):
  File "/Users/mitsuhiko/Development/rust-eh/examples/test.rs", line 42
    fail!(FileNotFound { file: Some(Path::new("/missing.txt")) });
  File "/Users/mitsuhiko/Development/rust-eh/examples/test.rs", line 46
    try!(test());
File not found: Failed to locate file (file=/missing.txt)

The above error resulted in another:

Traceback (most recent cause last):
  File "/Users/mitsuhiko/Development/rust-eh/examples/test.rs", line 52
    Err(err) => fail!(RecordNotFound, "could not find record", err),
Record not found: could not find record
```
