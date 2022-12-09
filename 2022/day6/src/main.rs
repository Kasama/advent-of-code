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

fn first_occurence_of_distinct_n_chars(input: &str, n: usize) -> usize {
    let (pos, _) = input
        .chars()
        .collect::<Vec<_>>()
        .windows(n)
        .enumerate()
        .find(|(_, window)| {
            let mut uniq = HashSet::new();

            window.iter().all(|i| uniq.insert(i))
        })
        .unwrap();

    pos + n
}

fn part1(input: &str) -> usize {
    first_occurence_of_distinct_n_chars(input, 4)
}

fn part2(input: &str) -> usize {
    first_occurence_of_distinct_n_chars(input, 14)
}

#[cfg(test)]
mod test {

    const INPUT: &str = include_str!("../input-example.txt");
    #[test]
    fn part1() {
        assert_eq!(super::part1(INPUT), 7);
    }

    #[test]
    fn part2() {
        assert_eq!(super::part2(INPUT), 19);
    }
}
