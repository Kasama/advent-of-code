use std::collections::HashSet;
use std::io::Read;

fn main() {
    let mut buffer = vec![];
    std::io::stdin().read_to_end(&mut buffer).unwrap();
    let input = String::from_utf8(buffer).unwrap();
    let result_1 = part1(&input);

    println!("part1: {}", result_1);

    let result_2 = part2(&input);
    println!("part2: {}", result_2);
}

fn priority(value: char) -> u32 {
    match value {
        // a..=z has values 1..27
        'a'..='z' => value as u32 - 'a' as u32 + 1,
        // A..=Z has values 27..53
        'A'..='Z' => value as u32 - 'A' as u32 + 27,
        // other values are invalid
        _ => 0,
    }
}

fn part1(input: &str) -> u32 {
    input
        .lines()
        .map(|bag| {
            // each input line is a single bag
            bag.chars()
                .collect::<Vec<_>>()
                // split each bag in the middle to get 2 sides
                .chunks(bag.len() / 2)
                .map(|side| {
                    // turn each side into a set of items
                    side.iter().fold(HashSet::new(), |mut bag_map, item| {
                        bag_map.insert(*item);
                        bag_map
                    })
                })
                .reduce(|mut items_in_both_sides, side| {
                    // retain only items that exist on both sides
                    // (if input is consistent, there should only be 1)
                    items_in_both_sides.retain(|item| side.contains(item));
                    items_in_both_sides
                })
                .unwrap_or_default()
        })
        // get priority of the remaining item for each bag
        .map(|mut bag| bag.drain().map(priority).sum::<u32>())
        // and add them up
        .sum::<u32>()
}

fn part2(input: &str) -> u32 {
    const BAGS_IN_A_GROUP: usize = 3;
    input
        // each input line is a single bag
        .lines()
        .collect::<Vec<_>>()
        // every 3 bags we have an elf group
        .chunks(BAGS_IN_A_GROUP)
        .map(|group| {
            group
                .iter()
                .map(|bag| {
                    // turns each bag into a set of items
                    bag.chars().fold(HashSet::new(), |mut bag, item| {
                        bag.insert(item);
                        bag
                    })
                })
                .reduce(|mut possible_badges, bag| {
                    // filter down the set of items to retain only
                    // items that exist on every bag (aka possible badges)
                    possible_badges.retain(|badge| bag.contains(badge));
                    possible_badges
                })
                .unwrap_or_default()
                .drain()
                // gets the priority of each possible badge
                .map(priority)
                // then adds them up
                // (if the input is consistent, there should only be 1 item to be added)
                .sum::<u32>()
        })
        .sum()
}

#[cfg(test)]
mod test {
    const INPUT: &str = include_str!("../input-example.txt");
    #[test]
    fn part1() {
        assert_eq!(super::part1(INPUT), 157);
    }

    #[test]
    fn part2() {
        assert_eq!(super::part2(INPUT), 70);
    }
}
