use repl::REPL;

mod repl;

fn main() {
    let mut repl = REPL::new();
    repl.run();
}
