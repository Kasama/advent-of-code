use std::io::Read;
use std::ops::RangeInclusive;

fn main() {
    let mut buffer = vec![];
    std::io::stdin().read_to_end(&mut buffer).unwrap();
    let input = String::from_utf8(buffer).unwrap();
    let result_1 = part1_count_fully_overlapping_pairs(&input);

    println!("part1: {}", result_1);

    let result_2 = part2_count_overlapping_pairs(&input);
    println!("part2: {}", result_2);
}

fn parse_section(range: &str) -> RangeInclusive<i64> {
    let (section_start_str, section_end_str) = range.split_once('-').unwrap_or_default();
    let (section_start, section_end) = (
        section_start_str.parse::<i64>().unwrap_or_default(),
        section_end_str.parse::<i64>().unwrap_or_default(),
    );

    section_start..=section_end
}
fn parse_pair(pair: &str) -> (RangeInclusive<i64>, RangeInclusive<i64>) {
    let (section1_str, section2_str) = pair.split_once(',').unwrap_or_default();

    (parse_section(section1_str), parse_section(section2_str))
}

fn part1_count_fully_overlapping_pairs(input: &str) -> usize {
    input
        .lines()
        .map(parse_pair)
        .filter(|(section1, section2)| {
            (section1.start() <= section2.start() && section1.end() >= section2.end())
                || (section2.start() <= section1.start() && section2.end() >= section1.end())
        })
        .count()
}

fn part2_count_overlapping_pairs(input: &str) -> usize {
    input
        .lines()
        .map(parse_pair)
        .filter(|(section1, section2)| {
            (section1.start() <= section2.start() && section1.end() >= section2.start())
                || (section2.start() <= section1.start() && section2.end() >= section1.start())
        })
        .count()
}

#[cfg(test)]
mod test {
    const INPUT: &str = include_str!("../input-example.txt");

    #[test]
    fn part1() {
        assert_eq!(super::part1_count_fully_overlapping_pairs(INPUT), 2);
    }

    #[test]
    fn part2() {
        assert_eq!(super::part2_count_overlapping_pairs(INPUT), 4);
    }
}
