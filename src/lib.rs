//! rust-eh is a new attempt to error handling.
#![crate_name = "eh"]
#![crate_type = "lib"]
#![license = "BSD"]
#![comment = "Rust error experimentation."]

#![deny(non_camel_case_types)]
#![feature(macro_rules)]
#![feature(associated_types)]
#![feature(while_let)]

use std::{raw, mem, io};
use std::intrinsics::TypeId;


/// Holds error location information.
#[deriving(Clone, PartialEq, Eq)]
pub struct ErrorLocation {
    /// the file that caused the error.
    pub file: Path,
    /// the line that caused the error
    pub line: uint,
    /// the column that caused the error.
    pub col: uint,
}

impl ErrorLocation {

    /// Returns the source line for this error location if this is possible.
    ///
    /// This will load the source file from the file system and read the
    /// appropriate line number.  This can be a slow operation which is
    /// why this is only recommended for debug builds.
    pub fn get_source_line(&self) -> io::IoResult<String> {
        let file = try!(io::File::open(&self.file));
        let mut reader = io::BufferedReader::new(file);
        match reader.lines().skip(self.line - 1).next() {
            Some(Ok(line)) => Ok(line),
            _ => Err(io::IoError {
                kind: io::EndOfFile,
                desc: "Reached end of file unexpectedly",
                detail: None,
            }),
        }
    }
}

/// This trait needs to be implemented to provide a custom error data.
/// Error data is used for both carrying additional information as well
/// as identifying which type of error occurred.
pub trait ErrorData: 'static + Send {

    /// The name of the error as it should appear in error messages.
    fn name(&self) -> &str;

    /// Optionally some computed string about extra detail that is
    /// available on the error data.
    fn detail(&self) -> Option<String> {
        None
    }

    /// This is a default string that is used for describing the error
    /// if the failure was not provided with one.
    fn default_description() -> &'static str {
        "An error occurred"
    }

    #[doc(hidden)]
    fn get_error_type(&self) -> TypeId { TypeId::of::<Self>() }
}

#[doc(hidden)]
pub trait ErrorDataExt<'a> {
    fn is<E: ErrorData>(self) -> bool;
    fn cast<E: ErrorData>(self) -> Option<&'a E>;
}

#[doc(hidden)]
impl<'a> ErrorDataExt<'a> for &'a ErrorData {

    #[inline(always)]
    fn is<E: ErrorData>(self) -> bool {
        self.get_error_type() == TypeId::of::<E>()
    }

    #[inline(always)]
    fn cast<E: ErrorData>(self) -> Option<&'a E> {
        if self.is() {
            unsafe {
                let to: raw::TraitObject = mem::transmute_copy(&self);
                Some(mem::transmute(to.data))
            }
        } else {
            None
        }
    }
}

/// Represents an error.  For most intents and purposes only the `Error::Actual`
/// is used.  The `Error::Propagated` is used in debug builds to augment the
/// error information by recording all the stacks the error gets propagated
/// through.
pub enum Error {
    Actual {
        /// The error data of this error.
        data: Box<ErrorData>,
        /// The static description of the error.
        description: &'static str,
        /// Optionally the location of where the error came from.
        location: Option<Box<ErrorLocation>>,
        /// Optionally the original error that caused this one.
        cause: Option<Box<Error>>,
    },
    Propagated {
        /// The parent error.
        parent: Box<Error>,
        /// The location of where the propagation happened.
        location: Option<Box<ErrorLocation>>,
    },
}

impl Error {
    /// Returns the name of the error.  This directly comes from the error
    /// data.
    pub fn name(&self) -> &str {
        match *self {
            Error::Actual { ref data, .. } => {
                data.name()
            },
            Error::Propagated { ref parent, .. } => {
                parent.name()
            }
        }
    }

    /// The static description of the error.
    pub fn description(&self) -> &str {
        match *self {
            Error::Actual { description, .. } => {
                description
            },
            Error::Propagated { ref parent, .. } => {
                parent.description()
            }
        }
    }

    /// The detail of the error.
    pub fn detail(&self) -> Option<String> {
        match *self {
            Error::Actual { ref data, .. } => {
                data.detail()
            },
            Error::Propagated { ref parent, .. } => {
                parent.detail()
            }
        }
    }

    /// The location of where the error last happened.
    pub fn location(&self) -> Option<&ErrorLocation> {
        match *self {
            Error::Actual { ref location, .. } => {
                match *location {
                    Some(ref loc) => Some(&**loc),
                    None => None,
                }
            },
            Error::Propagated { ref location, ref parent, .. } => {
                match *location {
                    Some(ref loc) => Some(&**loc),
                    None => parent.location(),
                }
            }
        }
    }

    /// Checks if the error is of a specific error data type.
    pub fn is<E: ErrorData>(&self) -> bool {
        match *self {
            Error::Actual { ref data, ref cause, .. } => {
                data.is() || match *cause {
                    Some(ref x) => x.is(),
                    None => false,
                }
            },
            Error::Propagated { ref parent, ..} => {
                parent.is()
            }
        }
    }

    /// Returns the error data.  Requires that the type is known.
    pub fn get_data<E: ErrorData>(&self) -> Option<&E> {
        match *self {
            Error::Actual { ref data, ref cause, .. } => {
                match data.cast() {
                    Some(x) => Some(x),
                    None => {
                        match *cause {
                            Some(ref x) => x.get_data(),
                            None => None,
                        }
                    }
                }
            },
            Error::Propagated { ref parent, ..} => {
                parent.get_data()
            }
        }
    }

    /// Returns the error that caused this one.
    pub fn cause(&self) -> Option<&Error> {
        match *self {
            Error::Actual { ref cause, .. } => {
                match *cause {
                    Some(ref cause) => Some(&**cause),
                    None => None,
                }
            },
            Error::Propagated { ref parent, .. } => parent.cause(),
        }
    }

    /// Returns the trace of the error.  This will be a tuple with the
    /// root and frames as `(root, frame)`.  The root is the actual
    /// error that was thrown whereas the causes are propagaged errors
    /// in the order the occurred.  This generally only works in debug
    /// builds.
    pub fn trace(&self) -> (&Error, Vec<&Error>) {
        let mut root;
        let mut causes = vec![];
        let mut ptr = self;

        loop {
            causes.push(ptr);
            match *ptr {
                Error::Actual { .. } => {
                    root = &*ptr;
                    break;
                },
                Error::Propagated { ref parent, .. } => {
                    ptr = &**parent;
                }
            }
        }

        causes.reverse();
        (root, causes)
    }
}

/// Helper trait that can construct errors for `fail!` and `try!`.
pub trait ConstructError<A> {
    fn construct_error(args: A, loc: Option<ErrorLocation>) -> Self;
}

/// This implementation passes through errors and augments them in debug
/// builds.
impl ConstructError<(Error,)> for Error {
    #[inline(always)]
    fn construct_error((err,): (Error,), loc: Option<ErrorLocation>) -> Error {
        if !cfg!(ndebug) {
            match loc {
                Some(loc) => {
                    return Error::Propagated {
                        parent: box err,
                        location: Some(box loc),
                    };
                },
                None => {},
            }
        }
        return err;
    }
}

/// This implementation constructs an error from error data alone.  This
/// will use the default description of the error data.
impl<S: ErrorData> ConstructError<(S,)> for Error {
    #[inline(always)]
    fn construct_error((data,): (S,), loc: Option<ErrorLocation>) -> Error {
        ConstructError::construct_error((data, ErrorData::default_description()), loc)
    }
}

/// This implementation constructs an error from error data alone.  This
/// will use the default description of the error data.
///
/// This will also set the cause.
impl<S: ErrorData> ConstructError<(S, Error)> for Error {
    #[inline(always)]
    fn construct_error((data, cause): (S, Error), loc: Option<ErrorLocation>) -> Error {
        ConstructError::construct_error((data, ErrorData::default_description(), cause), loc)
    }
}

/// This implementation constructs an error from error data and some
/// static description.
impl<S: ErrorData> ConstructError<(S, &'static str)> for Error {
    #[inline(always)]
    fn construct_error((data, desc): (S, &'static str), loc: Option<ErrorLocation>) -> Error {
        Error::Actual {
            data: box data,
            description: desc,
            location: match loc {
                Some(location) => Some(box location),
                None => None,
            },
            cause: None,
        }
    }
}

impl<S: ErrorData> ConstructError<(S, &'static str, Error)> for Error {
    #[inline(always)]
    fn construct_error((data, desc, cause): (S, &'static str, Error),
                       loc: Option<ErrorLocation>) -> Error {
        Error::Actual {
            data: box data,
            description: desc,
            location: match loc {
                Some(location) => Some(box location),
                None => None,
            },
            cause: Some(box cause),
        }
    }
}

/// A result type that uses the eh-error as error.
pub type EhResult<T> = Result<T, Error>;

/// Helper for formatting errors.
pub struct ErrorFormatter<W: Writer> {
    writer: W,
}

/// Helper for formatting errors.
impl<W: Writer> ErrorFormatter<W> {

    /// Creates a new error formatter.
    pub fn new(writer: W) -> ErrorFormatter<W> {
        ErrorFormatter {
            writer: writer,
        }
    }

    /// Formats the entire trace of an error.
    ///
    /// This will find all error traces and format them out with
    /// their causes.
    pub fn format_trace(&mut self, err: &Error) -> io::IoResult<()> {
        let mut traces = vec![];
        let mut ptr = err;

        loop {
            let (root, trace) = ptr.trace();
            traces.push((root, trace));
            match root.cause() {
                Some(x) => { ptr = x; }
                None => { break; }
            }
        }

        traces.reverse();
        for (idx, &(ref root, ref trace)) in traces.iter().enumerate() {
            if idx > 0 {
                try!(writeln!(self.writer, ""));
                try!(writeln!(self.writer, "The above error resulted in another:"));
                try!(writeln!(self.writer, ""));
            }
            try!(writeln!(self.writer, "Traceback (most recent cause last):"));
            for cause in trace.iter() {
                try!(self.format_frame(*cause));
            }
            try!(self.format_cause(*root));
        }

        Ok(())
    }

    /// Formats a single error frame.
    pub fn format_frame(&mut self, err: &Error) -> io::IoResult<()> {
        match err.location() {
            Some(loc) => {
                try!(writeln!(self.writer, "  File \"{}\", line {}",
                              loc.file.display(), loc.line));
                match loc.get_source_line() {
                    Ok(line) => try!(writeln!(self.writer, "    {}",
                        line.trim_chars([' ', '\t', '\r', '\n'].as_slice()))),
                    Err(_) => {}
                }
            }
            None => {
                try!(writeln!(self.writer, "  File <unknown>, line ?"));
            }
        }
        Ok(())
    }

    /// Formats the error cause information (name, description and detail).
    pub fn format_cause(&mut self, err: &Error) -> io::IoResult<()> {
        try!(write!(self.writer, "{}: {}",
                    err.name(), err.description()));
        match err.detail() {
            Some(detail) => try!(write!(self.writer, " ({})", detail)),
            None => {}
        }
        try!(writeln!(self.writer, ""));
        Ok(())
    }
}

/// Helper function to print the error cause stack to stderr.
pub fn print_error_stack(err: &Error) {
    let mut fmt = ErrorFormatter::new(std::io::stdio::stderr());
    let _ = fmt.format_trace(err);
}


/// Returns the error location.
///
/// This always returns the current error location.  It's recommended to use
/// `debug_error_location` instead.
#[macro_export]
macro_rules! error_location {
    () => ({
        ::eh::ErrorLocation {
            file: ::std::path::Path::new(file!()),
            line: line!(),
            col: col!(),
        }
    })
}

/// Returns the error location as option.
///
/// In debug builds this will return the error location, in release builds
/// this returns `None`.
#[macro_export]
macro_rules! debug_error_location {
    () => ({
        if cfg!(ndebug) {
            None
        } else {
            Some(error_location!())
        }
    })
}

/// Returns early with an error.
///
/// This is implemented through the `ConstructError` trait.  The following
/// common versions exist:
///
/// Fail with just an error data and default description:
///
/// ```rust,no_run
/// fail!(MyError)
/// ```
///
/// Fail with just an error data and default description and a cause:
///
/// ```rust,no_run
/// fail!(MyError, original_error)
/// ```
///
/// Fail with error data and custom description:
///
/// ```rust,no_run
/// fail!(MyError, "my description")
/// ```
///
/// Fail with error data and custom description and a cause:
///
/// ```rust,no_run
/// fail!(MyError, "my description", original_error)
/// ```
///
/// Propagate another error:
///
/// ```rust,no_run
/// fail!(original_error)
/// ```
#[macro_export]
macro_rules! fail {
    ($($expr:expr),*) => ({
        return Err(::eh::ConstructError::construct_error(
            ($($expr,)*), debug_error_location!()))
    });
}

/// Unwraps the `Ok` part of a result and propagates the error
/// through `fail!`.
#[macro_export]
macro_rules! try {
    ($expr:expr) => (match $expr {
        Err(x) => fail!(x),
        Ok(x) => x,
    })
}
