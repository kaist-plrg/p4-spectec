//! Text rendering shared across language stages
//!
//! `Print::print` writes to a `Printer`, which tracks indentation;
//! `to_string` renders into a fresh string.

use std::fmt;

// == Printing

/// Renders syntax through a shared printer.
pub trait Print {
    /// Writes this value using the current printer context.
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result;

    /// Renders this value using the default printer context.
    fn to_string(&self) -> String {
        let mut output = String::new();
        {
            let mut printer = Printer::new(&mut output);
            self.print(&mut printer)
                .expect("writing to a String cannot fail");
        }
        output
    }
}

// - Printer

/// Maintains output and layout state while rendering syntax.
pub struct Printer<'a> {
    /// Where text goes.
    output: &'a mut dyn fmt::Write,
    /// Current indentation depth, two spaces per level.
    level: usize,
}

impl<'a> Printer<'a> {
    /// Creates a printer at the outermost indentation level.
    pub fn new(output: &'a mut dyn fmt::Write) -> Self {
        Self { output, level: 0 }
    }

    /// Writes text without changing layout state.
    pub fn write(&mut self, text: &str) -> fmt::Result {
        self.output.write_str(text)
    }

    /// Writes formatted arguments without changing layout state.
    pub fn write_fmt(&mut self, args: fmt::Arguments<'_>) -> fmt::Result {
        self.output.write_fmt(args)
    }

    /// Starts a line at the current indentation level.
    pub fn newline(&mut self) -> fmt::Result {
        self.output.write_char('\n')?;
        self.output.write_str(&"  ".repeat(self.level))
    }

    /// Renders a nested value one indentation level deeper.
    pub fn indented(&mut self, print: impl FnOnce(&mut Self) -> fmt::Result) -> fmt::Result {
        self.level += 1;
        let result = print(self);
        self.level -= 1;
        result
    }

    /// Renders items separated by `sep`
    pub fn separated<T: Print>(&mut self, items: &[T], sep: &str) -> fmt::Result {
        for (index, item) in items.iter().enumerate() {
            if index != 0 {
                self.write(sep)?;
            }
            item.print(self)?;
        }
        Ok(())
    }
}

// - Output

impl fmt::Write for Printer<'_> {
    fn write_str(&mut self, text: &str) -> fmt::Result {
        self.output.write_str(text)
    }
}
