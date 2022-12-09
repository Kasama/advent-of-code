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

fn part1(input: &str) -> i64 {
    0
}

fn part2(input: &str) -> i64 {
    0
}

#[cfg(test)]
mod test {

    const INPUT: &str = include_str!("../input-example.txt");
    #[test]
    fn part1() {
        assert_eq!(super::part1(INPUT), 0);
    }

    #[test]
    fn part2() {
        assert_eq!(super::part2(INPUT), 0);
    }
}
