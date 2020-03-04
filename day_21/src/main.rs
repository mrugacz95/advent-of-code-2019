mod integer_computer;
use std::collections::HashMap;
use std::io::Write;
use std::{fs, io};

fn input() -> isize {
    print!("Provide input: ");
    io::stdout().flush().expect("Flash failed :<");
    let mut input = String::new();
    String::new();
    io::stdin()
        .read_line(&mut input)
        .expect("Failed to read line");
    return input
        .trim()
        .parse()
        .ok()
        .expect("Can't parse provided nubmer");
}

fn output(value: isize) {
    println!("output: {:?}", value);
}

fn main() {
    let memory: HashMap<usize, isize> = fs::read_to_string("day_21.in")
        .expect("Can't open input file")
        .split(",")
        .filter_map(|x| x.parse().ok())
        .enumerate()
        .collect::<HashMap<usize, isize>>();
    let mut ic = integer_computer::IntegerComputer::new(memory, input, output);
    ic.run();
}
