mod integer_computer;

use std::collections::{HashMap, VecDeque};
use std::{char, fs};

fn output(value: isize) {
    print!("{}", value as u8 as char);
}

fn main() {
    let source = fs::read_to_string("main.springscript").expect("Can't read robot source code.");
    let queue: Vec<isize> = source.as_bytes().iter().map(|&x| x as isize).collect();
    let mut queue = VecDeque::from(queue);

    print!("{:?}", queue);
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
