use std::collections::HashMap;
use std::io::Read;

use nom::bytes::complete::tag;
use nom::character::complete::{digit1, newline, space1};
use nom::combinator::map_res;
use nom::multi::many1;
use nom::IResult;

fn main() {
    let mut buffer = vec![];
    std::io::stdin().read_to_end(&mut buffer).unwrap();
    let input = String::from_utf8(buffer).unwrap();
    let result_1 = part1(&input).unwrap();

    println!("part1: {}", result_1);

    let result_2 = part2(&input).unwrap();
    println!("part2: {}", result_2);
}

#[derive(Debug, PartialEq, Eq)]
struct Move {
    amount: usize,
    from: usize,
    to: usize,
}

type Bays = HashMap<usize, Vec<char>>;

fn parse_bay<I, S>(input: I) -> Bays
where
    I: Iterator<Item = S>,
    S: ToString,
{
    input
        .map(|line| {
            line.to_string()
                .chars()
                .collect::<Vec<_>>()
                .chunks(4)
                .map(|char_crate| match char_crate {
                    ['[', c, ']'] | ['[', c, ']', ' '] => Some(c),
                    _ => None,
                })
                .enumerate()
                .fold(
                    HashMap::new(),
                    |mut bays: HashMap<usize, Vec<char>>, (bay_number_zeroth, value)| {
                        let bay_number = bay_number_zeroth + 1;
                        if let Some(v) = value {
                            if let Some(mut things) = bays.remove(&bay_number) {
                                things.push(*v);
                                bays.insert(bay_number, things);
                            } else {
                                bays.insert(bay_number, vec![*v]);
                            }
                        }
                        bays
                    },
                )
        })
        .fold(HashMap::new(), |bays: HashMap<usize, Vec<_>>, mut bay| {
            bay.drain().fold(bays, |mut b, (k, mut v)| {
                if let Some(mut existing) = b.remove(&k) {
                    existing.append(&mut v);
                    b.insert(k, existing);
                } else {
                    b.insert(k, v);
                }
                b
            })
        })
        .drain()
        .fold(HashMap::new(), |mut bays, (k, mut v)| {
            v.reverse();
            bays.insert(k, v);
            bays
        })
}

fn move_parser(line: &str) -> IResult<&str, Move> {
    nom::combinator::map(
        nom::sequence::tuple((
            tag("move"),
            space1,
            map_res(digit1, |n: &str| n.parse::<usize>()),
            space1,
            tag("from"),
            space1,
            map_res(digit1, |n: &str| n.parse::<usize>()),
            space1,
            tag("to"),
            space1,
            map_res(digit1, |n: &str| n.parse::<usize>()),
            newline,
        )),
        |(_, _, m, _, _, _, f, _, _, _, t, _)| Move {
            amount: m,
            from: f,
            to: t,
        },
    )(line)
}

fn moves_parser(input: &str) -> IResult<&str, Vec<Move>> {
    many1(move_parser)(input)
}

fn parse_input(input: &str) -> Option<(Bays, Vec<Move>)> {
    let (crates, moves_raw) = input.split_once("\n\n")?;

    let bays = parse_bay(crates.lines());

    let (_, moves) = moves_parser(moves_raw).ok()?;

    Some((bays, moves))
}

fn first_crate_of_each_bay(mut bays: Bays) -> String {
    let mut first_crate_of_each_bay = bays
        .drain()
        .filter_map(|(k, mut v)| v.pop().map(|a| (k, a)))
        .collect::<Vec<_>>();

    first_crate_of_each_bay.sort_by(|(k1, _), (k2, _)| k1.cmp(k2));

    first_crate_of_each_bay
        .into_iter()
        .map(|(_, v)| v)
        .collect::<String>()
}

enum ProblemPart {
    Part1,
    Part2,
}

fn execute_move(mut b: Bays, m: Move, part: &ProblemPart) -> Bays {
    let (repetitions, move_amount) = match part {
        ProblemPart::Part1 => (m.amount, 1),
        ProblemPart::Part2 => (1, m.amount),
    };

    for _ in 1..=repetitions {
        if let Some(bay) = b.get_mut(&m.from) {
            let mut crates: Vec<_> = bay.drain(bay.len() - move_amount..bay.len()).collect();
            if let Some(b) = b.get_mut(&m.to) {
                b.append(&mut crates);
            }
        }
    }

    b
}

fn problem(input: &str, part: ProblemPart) -> Option<String> {
    let (bays, moves) = parse_input(input)?;

    let bays = moves
        .into_iter()
        .fold(bays, |b: HashMap<usize, Vec<char>>, m| {
            execute_move(b, m, &part)
        });

    Some(first_crate_of_each_bay(bays))
}

fn part1(input: &str) -> Option<String> {
    problem(input, ProblemPart::Part1)
}

fn part2(input: &str) -> Option<String> {
    problem(input, ProblemPart::Part2)
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use nom::bytes::complete::tag;
    use nom::character::complete::digit1;
    use nom::character::streaming::space1;
    use nom::combinator::map_res;
    use nom::IResult;

    use crate::{parse_input, Move};

    const INPUT: &str = include_str!("../input-example.txt");
    #[test]
    fn play() {
        let inn = r#"    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3

move 10 from 2 to 9
move 11 from 1 to 27
"#;
        let (bays, moves) = parse_input(inn).expect("no error");
        assert_eq!(
            bays,
            HashMap::from([
                (1, vec!['Z', 'N']),
                (2, vec!['M', 'C', 'D']),
                (3, vec!['P'])
            ])
        );

        assert_eq!(
            moves,
            vec![
                Move {
                    amount: 10,
                    from: 2,
                    to: 9
                },
                Move {
                    amount: 11,
                    from: 1,
                    to: 27
                },
            ]
        )
    }

    #[test]
    fn parser() {
        let tt = nom::sequence::tuple((
            tag("move"),
            space1,
            map_res(digit1, |n: &str| n.parse::<usize>()),
            space1,
            tag("from"),
            space1,
            map_res(digit1, |n: &str| n.parse::<usize>()),
            space1,
            tag("to"),
            space1,
            map_res(digit1, |n: &str| n.parse::<usize>()),
        ));
        let mut ttt = nom::combinator::map(tt, |(_, _, m, _, _, _, f, _, _, _, t)| Move {
            amount: m,
            from: f,
            to: t,
        });
        let tn: IResult<&str, Move> = ttt("move 10 from 2 to 3");

        assert_eq!(
            tn,
            Ok((
                "",
                Move {
                    amount: 10,
                    from: 2,
                    to: 3
                }
            ))
        );
    }

    #[test]
    fn part1() {
        assert_eq!(super::part1(INPUT), Some("CMZ".to_owned()));
    }

    #[test]
    fn part2() {
        assert_eq!(super::part2(INPUT), Some("MCD".to_owned()));
    }
}
