use std::collections::HashMap;
use std::io::Write;
use std::{fs, io};

struct IntegerComputer {
    memory: HashMap<usize, isize>,
    running: bool,
    pointer: usize,
    relative_base: isize,
    input_fn: fn() -> isize,
    output_fn: fn(isize),
}

impl IntegerComputer {
    fn new(
        memory: HashMap<usize, isize>,
        input_fn: fn() -> isize,
        output_fn: fn(isize),
    ) -> IntegerComputer {
        return IntegerComputer {
            memory,
            running: true,
            pointer: 0,
            relative_base: 0,
            input_fn,
            output_fn,
        };
    }
    fn get_mode(&self, offset: usize) -> isize {
        match self.memory.get(&self.pointer) {
            Some(mode) => {
                let mut mode = mode / 100;
                for _ in 1..offset {
                    mode /= 10;
                }
                return mode % 10;
            }
            None => panic!("{:?} at memory is not set", self.pointer),
        }
    }
    fn read(&mut self, addr: usize) -> isize {
        *self.memory.entry(addr).or_insert(0)
    }
    fn write(&mut self, offset: usize, value: isize) {
        let mode = self.get_mode(offset);
        let addr = match mode {
            0 => self.read(self.pointer + offset) as usize,
            1 => self.pointer + offset,
            2 => {
                let addr = self.read(self.pointer + offset);
                (self.relative_base + addr) as usize
            }
            _ => panic!("Wrong write mode: {:?}", mode),
        };
        self.memory.insert(addr as usize, value);
    }
    fn get_param(&mut self, offset: usize) -> isize {
        let mode = self.get_mode(offset);
        let addr = match mode {
            0 => self.read(self.pointer + offset) as usize,
            1 => self.pointer + offset,

            2 => (self.relative_base + self.read(self.pointer + offset)) as usize,
            _ => panic!("Wrong param mode: {:?}", mode),
        };
        self.read(addr)
    }
    fn run(&mut self) {
        while self.running {
            let opcode = self.read(self.pointer) % 100;
            match opcode {
                1 => {
                    let a = self.get_param(1);
                    let b = self.get_param(2);
                    self.write(3, a + b);
                    self.pointer += 4;
                }
                2 => {
                    let a = self.get_param(1);
                    let b = self.get_param(2);
                    self.write(3, a * b);
                    self.pointer += 4;
                }
                3 => {
                    let input = (self.input_fn)();
                    self.write(1, input);
                    self.pointer += 2;
                }
                4 => {
                    let a = self.get_param(1);
                    (self.output_fn)(a);
                    self.pointer += 2;
                }
                5 => {
                    let a = self.get_param(1);
                    let b = self.get_param(2);
                    if a != 0 {
                        self.pointer = b as usize;
                    } else {
                        self.pointer += 3
                    }
                }
                6 => {
                    let a = self.get_param(1);
                    let b = self.get_param(2);
                    if a == 0 {
                        self.pointer = b as usize;
                    } else {
                        self.pointer += 3
                    }
                }
                7 => {
                    let a = self.get_param(1);
                    let b = self.get_param(2);
                    let value = if a < b { 1 } else { 0 };
                    self.write(3, value);
                    self.pointer += 4;
                }
                8 => {
                    let a = self.get_param(1);
                    let b = self.get_param(2);
                    let value = if a == b { 1 } else { 0 };
                    self.write(3, value);
                    self.pointer += 4;
                }
                9 => {
                    let a = self.get_param(1);
                    self.relative_base += a;
                    self.pointer += 2;
                }
                99 => self.running = false,
                _ => panic!("Wrong opcode: {:?}", opcode),
            }
        }
    }
}

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
    let mut ic = IntegerComputer::new(memory, input, output);
    ic.run();
}
