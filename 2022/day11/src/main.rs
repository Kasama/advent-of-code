mod monkey;
use std::collections::HashMap;
use std::io::Read;

use nom::character::complete::line_ending;
use nom::multi::separated_list1;

use self::monkey::{Monkey, Worry, WorryTransformer};

fn main() {
    let mut buffer = vec![];
    std::io::stdin().read_to_end(&mut buffer).unwrap();
    let input = String::from_utf8(buffer).unwrap();
    let result_1 = part1(&input);

    println!("part1: {}", result_1);

    let result_2 = part2(&input);
    println!("part2: {}", result_2);
}

fn round(monkeys: &mut [monkey::Monkey]) {
    let mut loose_items: HashMap<usize, Vec<Worry>> = HashMap::new();
    for mut monkey in monkeys.iter_mut() {
        let mut extra_items = loose_items.remove(&monkey.id).unwrap_or_default();

        monkey.inventory.append(&mut extra_items);

        let items = monkey.play_turn();
        monkey.inspected += items.len();

        for (item, target_monkey) in items {
            if let Some(mut items) = loose_items.remove(&target_monkey.id()) {
                items.push(item);
                loose_items.insert(target_monkey.id(), items);
            } else {
                loose_items.insert(target_monkey.id(), vec![item]);
            }
        }

        monkey.inventory = vec![];
    }

    for (monkey, mut items) in loose_items.drain() {
        monkeys[monkey].inventory.append(&mut items)
    }
}

fn part1(input: &str) -> i64 {
    let (_, mut monkeys) = separated_list1(
        line_ending,
        Monkey::parse(WorryTransformer::FloorDiv(3u32.into())),
    )(input)
    .unwrap();

    for _ in 1..=20 {
        round(&mut monkeys);
    }

    monkeys.sort_by(|m, m2| m2.inspected.cmp(&m.inspected));

    // monkey business
    monkeys[0].monkey_business(&monkeys[1]) as i64
}

fn part2(input: &str) -> i64 {
    let (_, monkeys) =
        separated_list1(line_ending, Monkey::parse(WorryTransformer::None))(input).unwrap();

    let common_multiple = monkeys.iter().fold(1, |acc, m| {
        acc * match m.test.operation {
            monkey::TestOperation::Divisible(x) => x,
        }
    });

    let mut new_monkeys = monkeys
        .into_iter()
        .map(|mut m| {
            m.worry_transformer = WorryTransformer::Rem(common_multiple);
            m
        })
        .collect::<Vec<_>>();

    for _ in 1..=10000 {
        round(&mut new_monkeys);
    }

    new_monkeys.sort_by(|m, m2| m2.inspected.cmp(&m.inspected));

    new_monkeys[0].monkey_business(&new_monkeys[1]) as i64
}

#[cfg(test)]
mod test {

    const INPUT: &str = include_str!("../input-example.txt");
    #[test]
    fn part1() {
        assert_eq!(super::part1(INPUT), 10605);
    }

    #[test]
    fn part2() {
        assert_eq!(super::part2(INPUT), 2713310158);
    }
}
