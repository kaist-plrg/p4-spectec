//! The C ABI of the K library that `Offload` calls, an `unsafe` module
//!
//! The library is what `llvm-kompile definition.kore dt c` makes of a definition,
//! with the API of `kllvm-c/kllvm-c.h`; `Runtime::simplify` evaluates the functions
//! of a term, e.g. `_+Int_(2, 3)` to `5`. A library keeps global state and reserves
//! 2 TB of address space for each of its three arenas, so a process opens each once,
//! never closes it, and releases the arenas of one before opening another.
//! Besides `engine/k/ffi.rs`, this is the only module where the crate allows `unsafe`.

#![allow(unsafe_code)]

use std::{
    collections::HashMap,
    ffi::{CStr, CString, c_char, c_void},
    path::{Path, PathBuf},
    sync::{Mutex, OnceLock},
};

use libloading::Library;

use super::ast::Sort;

unsafe extern "C" {
    /// Frees the output buffer, which the runtime allocates with `malloc`.
    fn free(ptr: *mut c_void);
}

// == The C API

/// An object of the C API, used only through its functions.
type Opaque = c_void;

/// The functions of `kllvm-c.h` used here.
struct Api {
    kllvm_init: unsafe extern "C" fn(),
    kllvm_reset_munmap_all_arenas: unsafe extern "C" fn(),
    kore_error_new: unsafe extern "C" fn() -> *mut Opaque,
    kore_error_is_success: unsafe extern "C" fn(*const Opaque) -> bool,
    kore_error_message: unsafe extern "C" fn(*const Opaque) -> *const c_char,
    kore_error_free: unsafe extern "C" fn(*mut Opaque),
    kore_pattern_parse: unsafe extern "C" fn(*const c_char) -> *mut Opaque,
    kore_pattern_free: unsafe extern "C" fn(*const Opaque),
    kore_composite_sort_new: unsafe extern "C" fn(*const c_char) -> *mut Opaque,
    kore_composite_sort_add_argument: unsafe extern "C" fn(*const Opaque, *const Opaque),
    kore_sort_free: unsafe extern "C" fn(*const Opaque),
    kore_simplify: unsafe extern "C" fn(
        *mut Opaque,
        *const Opaque,
        *const Opaque,
        *mut *mut c_char,
        *mut usize,
    ),
}

/// Looks a function up in the library, as the type of the field it fills.
macro_rules! lookup {
    ($library:expr, $name:ident) => {{
        // SAFETY: the symbol is the function of kllvm-c.h with this signature;
        // the pointer stays valid as long as the library, which is never closed
        let symbol = unsafe { $library.get(concat!(stringify!($name), "\0").as_bytes()) }
            .map_err(|error| format!("{}: {error}", stringify!($name)))?;
        *symbol
    }};
}

// == The runtime

/// The K runtime of one compiled definition.
pub struct Runtime {
    api: Api,
}

/// The runtimes opened in this process, by the path of their library, and the
/// path of the one in use; the others have released their arenas.
#[derive(Default)]
struct Opened {
    runtimes: HashMap<PathBuf, &'static Runtime>,
    current: Option<PathBuf>,
}

/// The runtimes opened in this process.
fn opened() -> &'static Mutex<Opened> {
    static OPENED: OnceLock<Mutex<Opened>> = OnceLock::new();
    OPENED.get_or_init(Mutex::default)
}

impl Runtime {
    /// Opens a definition's library and initializes its runtime,
    /// or gives the runtime already opened from that path.
    pub fn open(path: &Path) -> Result<&'static Self, String> {
        let path = path
            .canonicalize()
            .map_err(|error| format!("{}: {error}", path.display()))?;
        let mut opened = opened().lock().map_err(|error| error.to_string())?;
        if opened.current.as_ref() == Some(&path) {
            return Ok(opened.runtimes[&path]);
        }
        if opened.runtimes.contains_key(&path) {
            return Err(format!(
                "{}: its arenas were released for another definition",
                path.display()
            ));
        }
        // Release the arenas of the definition run before
        if let Some(previous) = opened.current.take() {
            let runtime = opened.runtimes[&previous];
            // SAFETY: no term of that runtime is used after another definition is opened
            unsafe { (runtime.api.kllvm_reset_munmap_all_arenas)() };
        }
        // SAFETY: loading runs the library's initializers, those of the
        // LLVM backend's runtime, which only set up its own state
        let library = unsafe { Library::new(&path) }
            .map_err(|error| format!("{}: {error}", path.display()))?;
        let api = Api {
            kllvm_init: lookup!(library, kllvm_init),
            kllvm_reset_munmap_all_arenas: lookup!(library, kllvm_reset_munmap_all_arenas),
            kore_error_new: lookup!(library, kore_error_new),
            kore_error_is_success: lookup!(library, kore_error_is_success),
            kore_error_message: lookup!(library, kore_error_message),
            kore_error_free: lookup!(library, kore_error_free),
            kore_pattern_parse: lookup!(library, kore_pattern_parse),
            kore_pattern_free: lookup!(library, kore_pattern_free),
            kore_composite_sort_new: lookup!(library, kore_composite_sort_new),
            kore_composite_sort_add_argument: lookup!(library, kore_composite_sort_add_argument),
            kore_sort_free: lookup!(library, kore_sort_free),
            kore_simplify: lookup!(library, kore_simplify),
        };
        // The runtime keeps global state for the rest of the process
        std::mem::forget(library);
        // SAFETY: called once, before any other call into the runtime
        unsafe { (api.kllvm_init)() };
        let runtime: &'static Self = Box::leak(Box::new(Self { api }));
        opened.runtimes.insert(path.clone(), runtime);
        opened.current = Some(path);
        Ok(runtime)
    }

    /// A sort of the C API; the caller frees it.
    fn sort(&self, sort: &Sort) -> Result<*mut Opaque, String> {
        let Sort::Sort(name, args) = sort else {
            return Err(format!("sort variable {sort} in a term"));
        };
        let name_c = CString::new(name.as_str()).map_err(|_| format!("NUL in sort {name}"))?;
        // SAFETY: a C string; the result is a new sort
        let sort_c = unsafe { (self.api.kore_composite_sort_new)(name_c.as_ptr()) };
        for arg in args {
            let arg_c = self.sort(arg)?;
            // SAFETY: both sorts are live; the argument is copied, so freed here
            unsafe {
                (self.api.kore_composite_sort_add_argument)(sort_c, arg_c);
                (self.api.kore_sort_free)(arg_c);
            }
        }
        Ok(sort_c)
    }

    /// Evaluates the functions of a term in KORE text, of the given sort;
    /// the result in binary KORE, or the runtime's error message.
    pub fn simplify(&self, term: &str, sort: &Sort) -> Result<Vec<u8>, String> {
        let term_c = CString::new(term).map_err(|_| "NUL in a term".to_owned())?;
        let sort_c = self.sort(sort)?;
        // SAFETY: every pointer passed is live for the call; each object made
        // here is freed once, and the output buffer is malloc'ed by the runtime
        unsafe {
            let pattern = (self.api.kore_pattern_parse)(term_c.as_ptr());
            if pattern.is_null() {
                (self.api.kore_sort_free)(sort_c);
                return Err(format!("the runtime cannot parse {term}"));
            }
            let error = (self.api.kore_error_new)();
            let mut data: *mut c_char = std::ptr::null_mut();
            let mut size: usize = 0;
            (self.api.kore_simplify)(error, pattern, sort_c, &mut data, &mut size);
            let result = if (self.api.kore_error_is_success)(error) && !data.is_null() {
                Ok(std::slice::from_raw_parts(data.cast::<u8>(), size).to_vec())
            } else {
                let message = (self.api.kore_error_message)(error);
                Err(if message.is_null() {
                    "the runtime gives no result".to_owned()
                } else {
                    CStr::from_ptr(message).to_string_lossy().into_owned()
                })
            };
            if !data.is_null() {
                free(data.cast());
            }
            (self.api.kore_error_free)(error);
            (self.api.kore_pattern_free)(pattern);
            (self.api.kore_sort_free)(sort_c);
            result
        }
    }
}
