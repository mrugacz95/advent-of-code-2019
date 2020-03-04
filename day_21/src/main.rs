mod integer_computer;

use std::collections::{HashMap, VecDeque};
use std::{char, fs};

fn output(value: isize) {
    match char::from_u32(value as u32) {
        Some(c) => print!("{}", c),
        None => println!("{}", value),
    }
}

fn main() {
    let source = fs::read_to_string("part_1.springscript").expect("Can't read robot source code.");
    let queue: Vec<isize> = source.as_bytes().iter().map(|&x| x as isize).collect();
    let mut queue = VecDeque::from(queue);

    let memory: HashMap<usize, isize> = fs::read_to_string("day_21.in")
        .expect("Can't open input file")
        .split(",")
        .filter_map(|x| x.parse().ok())
        .enumerate()
        .collect::<HashMap<usize, isize>>();
    let mut ic =
        integer_computer::IntegerComputer::new(memory, || queue.pop_front().unwrap(), output);
    ic.run();
}
