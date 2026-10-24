//! The K runtime of one definition, through the LLVM backend's C bindings
//!
//! `llvm-kompile definition.kore dt c -- -o lib.so` compiles a definition,
//! with its functions and hooks, into a library with the API of
//! `kllvm-c/kllvm-c.h` (llvm-backend `docs/embedding.md`).
//! `Runtime::open` loads that library, and `Runtime::simplify` evaluates
//! the functions of a term, e.g. `_+Int_(2, 3)` to `5`, without rewriting.
//! The library keeps global state, so a process opens one definition.

//!
//! This is the one module with unsafe code: the calls into the library.

#![allow(unsafe_code)]

use std::{
    ffi::{CStr, CString, c_char, c_int, c_void},
    path::Path,
};

use super::ast::Sort;

// == The dynamic loader

/// `dlopen` resolves every symbol of the library at once.
const RTLD_NOW: c_int = 2;

unsafe extern "C" {
    fn dlopen(filename: *const c_char, flags: c_int) -> *mut c_void;
    fn dlsym(handle: *mut c_void, symbol: *const c_char) -> *mut c_void;
    fn dlerror() -> *const c_char;
    fn free(ptr: *mut c_void);
}

/// The last error of the dynamic loader.
fn dl_error() -> String {
    // SAFETY: dlerror returns null or a C string valid until the next dl call
    unsafe {
        let message = dlerror();
        if message.is_null() {
            "unknown error".to_owned()
        } else {
            CStr::from_ptr(message).to_string_lossy().into_owned()
        }
    }
}

// == The C API

/// An object of the C API, used only through its functions.
type Opaque = c_void;

/// The functions of `kllvm-c.h` used here.
struct Api {
    kllvm_init: unsafe extern "C" fn(),
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

/// Looks a function up in the library, as the given function type.
macro_rules! lookup {
    ($handle:expr, $name:ident) => {{
        let name = CString::new(stringify!($name)).expect("no NUL in a symbol name");
        // SAFETY: the library is open; the symbol is checked for null below
        let ptr = unsafe { dlsym($handle, name.as_ptr()) };
        if ptr.is_null() {
            return Err(format!("{}: {}", stringify!($name), dl_error()));
        }
        // SAFETY: the symbol is the function of kllvm-c.h with this signature;
        // the target type is the field the macro fills
        #[allow(clippy::missing_transmute_annotations)]
        let function = unsafe { std::mem::transmute::<*mut c_void, _>(ptr) };
        function
    }};
}

// == The runtime

/// The K runtime of one compiled definition.
pub struct Runtime {
    api: Api,
}

impl Runtime {
    /// Opens a definition's library and initializes its runtime.
    pub fn open(path: &Path) -> Result<Self, String> {
        let path_c = CString::new(path.as_os_str().as_encoded_bytes())
            .map_err(|_| format!("{}: NUL in the path", path.display()))?;
        // SAFETY: a C string path; the handle is never closed, as the
        // runtime keeps global state for the rest of the process
        let handle = unsafe { dlopen(path_c.as_ptr(), RTLD_NOW) };
        if handle.is_null() {
            return Err(format!("{}: {}", path.display(), dl_error()));
        }
        let api = Api {
            kllvm_init: lookup!(handle, kllvm_init),
            kore_error_new: lookup!(handle, kore_error_new),
            kore_error_is_success: lookup!(handle, kore_error_is_success),
            kore_error_message: lookup!(handle, kore_error_message),
            kore_error_free: lookup!(handle, kore_error_free),
            kore_pattern_parse: lookup!(handle, kore_pattern_parse),
            kore_pattern_free: lookup!(handle, kore_pattern_free),
            kore_composite_sort_new: lookup!(handle, kore_composite_sort_new),
            kore_composite_sort_add_argument: lookup!(handle, kore_composite_sort_add_argument),
            kore_sort_free: lookup!(handle, kore_sort_free),
            kore_simplify: lookup!(handle, kore_simplify),
        };
        // SAFETY: called once, before any other call into the runtime
        unsafe { (api.kllvm_init)() };
        Ok(Self { api })
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
