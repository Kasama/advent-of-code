use std::ops::{Div, Rem};

use nom::bytes::complete::tag;
use nom::character::complete::{digit1, line_ending, space0};
use nom::multi::separated_list1;
use nom::IResult;

// pub type Worry = num_bigint::BigUint;
pub type Worry = u64;

#[derive(Debug, PartialEq, Eq)]
pub enum Operation {
    Add(Worry),
    Mult(Worry),
    Square,
    Double,
    None,
}

impl Operation {
    fn parse(input: &str) -> IResult<&str, Self> {
        nom::combinator::map(
            nom::sequence::tuple((
                space0,
                tag("Operation: "),
                tag("new"),
                tag(" = "),
                nom::branch::alt((
                    nom::sequence::tuple((tag("old + "), tag("old"))),
                    nom::sequence::tuple((tag("old * "), tag("old"))),
                    nom::sequence::tuple((tag("old + "), digit1)),
                    nom::sequence::tuple((tag("old * "), digit1)),
                )),
            )),
            |(_, _, _, _, op)| match op {
                ("old + ", "old") => Self::Double,
                ("old * ", "old") => Self::Square,
                ("old + ", n) => Self::Add(n.parse().unwrap_or_default()),
                ("old * ", n) => Self::Mult(n.parse().unwrap_or_default()),
                _ => Self::None,
            },
        )(input)
    }
    fn execute(&self, target: Worry) -> Worry {
        match self {
            Operation::Add(x) => target + x,
            Operation::Mult(x) => target * x,
            Operation::Square => target.pow(2),
            Operation::Double => target * 2u64,
            Operation::None => target,
        }
    }
}

#[cfg(test)]
mod test_operation {
    use super::Operation;

    #[test]
    fn parse() {
        let input = "  Operation: new = old * 19\n";

        let operation = Operation::parse(input);
        assert_eq!(operation, Ok(("\n", Operation::Mult(19u64))))
    }
}

#[derive(Debug, Clone, Copy)]
pub enum WorryTransformer {
    FloorDiv(Worry),
    Rem(Worry),
    None,
}

impl WorryTransformer {
    fn execute(&self, worry: Worry) -> Worry {
        match self {
            WorryTransformer::FloorDiv(n) => worry.div(n),
            WorryTransformer::None => worry,
            WorryTransformer::Rem(n) => worry.rem(n),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Target {
    Monkey(usize),
}

impl Target {
    pub fn id(&self) -> usize {
        match self {
            Target::Monkey(i) => *i,
        }
    }
}

#[derive(Debug)]
pub struct Test {
    pub operation: TestOperation,
    pub on_true: Target,
    pub on_false: Target,
}

impl Test {
    fn parse(input: &str) -> IResult<&str, Self> {
        nom::combinator::map(
            nom::sequence::tuple((
                space0::<&str, _>,
                tag("Test: divisible by "),
                digit1,
                line_ending,
                space0,
                tag("If true: throw to monkey "),
                digit1,
                line_ending,
                space0,
                tag("If false: throw to monkey "),
                digit1,
            )),
            |(_, _, divisible, _, _, _, true_monkey, _, _, _, false_monkey)| Test {
                operation: TestOperation::Divisible(divisible.parse().unwrap_or_default()),
                on_true: Target::Monkey(true_monkey.parse().unwrap_or_default()),
                on_false: Target::Monkey(false_monkey.parse().unwrap_or_default()),
            },
        )(input)
    }
}

#[derive(Debug)]
pub enum TestOperation {
    Divisible(Worry),
}

impl TestOperation {
    fn execute(&self, target: &Worry) -> bool {
        match self {
            TestOperation::Divisible(x) => target % x == 0u64,
        }
    }
}

#[derive(Debug)]
pub struct Monkey {
    pub id: usize,
    pub inspected: usize,
    pub inventory: Vec<Worry>,
    pub operation: Operation,
    pub test: Test,
    pub worry_transformer: WorryTransformer,
}

impl Monkey {
    pub fn parse(transformer: WorryTransformer) -> impl FnMut(&str) -> IResult<&str, Self> {
        move |input: &str| {
            nom::combinator::map(
                nom::sequence::tuple((
                    tag("Monkey "),
                    digit1,
                    tag(":"),
                    line_ending,
                    space0,
                    tag("Starting items: "),
                    separated_list1(tag(", "), digit1),
                    line_ending,
                    Operation::parse,
                    line_ending,
                    Test::parse,
                    line_ending,
                )),
                |(_, id, _, _, _, _, inventory, _, op, _, test, _)| Self {
                    id: id.parse().unwrap_or_default(),
                    inspected: 0,
                    inventory: inventory
                        .into_iter()
                        .filter_map(|i| i.parse::<Worry>().ok())
                        .collect(),
                    operation: op,
                    worry_transformer: transformer,
                    test,
                },
            )(input)
        }
    }

    pub fn play_turn(&mut self) -> Vec<(Worry, Target)> {
        self.inventory
            .drain(0..)
            .map(|item_worry| {
                let new_worry = self
                    .worry_transformer
                    .execute(self.operation.execute(item_worry));
                let target = if self.test.operation.execute(&new_worry) {
                    self.test.on_true
                } else {
                    self.test.on_false
                };
                (new_worry, target)
            })
            .collect()
    }

    pub fn monkey_business(&self, other: &Monkey) -> usize {
        self.inspected * other.inspected
    }
}
