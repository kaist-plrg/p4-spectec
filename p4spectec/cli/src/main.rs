//! The `p4spectec` command

fn main() -> std::process::ExitCode {
    p4spectec_cli::main(std::env::args_os())
}
