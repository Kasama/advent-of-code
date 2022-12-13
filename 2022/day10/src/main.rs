use std::collections::HashMap;
use std::io::Read;

fn main() {
    let mut buffer = vec![];
    std::io::stdin().read_to_end(&mut buffer).unwrap();
    let input = String::from_utf8(buffer).unwrap();
    let result_1 = part1(&input);

    println!("part1: {}", result_1);

    part2(&input);
}

#[derive(Debug)]
enum Instruction {
    Noop,
    Addx(i64),
}

impl Instruction {
    fn parse(input: &str) -> Option<Self> {
        match input {
            "noop" => Some(Self::Noop),
            _ => {
                let (_, n) = input.split_once(' ')?;
                Some(Self::Addx(n.parse::<i64>().ok()?))
            }
        }
    }
    fn duration(&self) -> usize {
        match self {
            Instruction::Noop => 1,
            Instruction::Addx(_) => 2,
        }
    }
}

#[derive(Clone, Debug)]
struct Machine {
    cycle: usize,
    instruction_counter: usize,
    delay: usize,
    x: i64,
}

impl Machine {
    fn new() -> Self {
        Machine {
            cycle: 1,
            instruction_counter: 0,
            delay: 0,
            x: 1,
        }
    }

    fn step(&mut self, program: &[Instruction]) -> Option<()> {
        let instruction = program.get(self.instruction_counter)?;
        self.cycle += 1;

        if self.delay > 0 {
            self.delay -= 1;
        } else {
            self.delay = instruction.duration() - 1;
        }

        if self.delay == 0 {
            match instruction {
                Instruction::Noop => (),
                Instruction::Addx(x) => {
                    self.x += x;
                }
            }

            self.instruction_counter += 1;
        }

        Some(())
    }

    fn char_for(&self) -> char {
        let pixel_pos = (self.cycle - 1) as i64 % 40;
        if self.x - 1 <= pixel_pos && pixel_pos <= self.x + 1 {
            '#'
        } else {
            ' ' //using ' ' instead of '.' because it makes it easier to see
        }
    }

    fn run(&mut self, program: Vec<Instruction>) -> HashMap<usize, (Machine, char)> {
        let mut results: HashMap<usize, (Machine, char)> = HashMap::new();

        results.insert(self.cycle, (self.clone(), self.char_for()));
        while let Some(()) = self.step(&program) {
            results.insert(self.cycle, (self.clone(), self.char_for()));
        }

        results
    }
}

fn part1(input: &str) -> i64 {
    let program = input
        .lines()
        .filter_map(Instruction::parse)
        .collect::<Vec<_>>();

    let inspect_cycles = vec![20, 60, 100, 140, 180, 220];

    let mut machine = Machine::new();
    let results = machine.run(program);

    inspect_cycles
        .into_iter()
        .filter_map(|c| Some((c, results.get(&c)?)))
        .fold(0, |acc, (c, (m, _))| acc + ((c as i64) * m.x))
}

fn part2(input: &str) {
    let program = input
        .lines()
        .filter_map(Instruction::parse)
        .collect::<Vec<_>>();

    let mut machine = Machine::new();
    let results = machine.run(program);

    print!("part 2:");
    {
        let len = results.len();
        let mut ms: Vec<(Machine, char)> = vec![(Machine::new(), ' '); len];
        for (k, v) in results.into_iter() {
            ms[k - 1] = v
        }

        for (i, (_, c)) in ms.into_iter().enumerate() {
            if i % 40 == 0 {
                println!();
            }
            print!("{}", c)
        }
    }
}

#[cfg(test)]
mod test {

    const INPUT: &str = include_str!("../input-example.txt");
    #[test]
    fn part1() {
        assert_eq!(super::part1(INPUT), 13140);
    }
}
