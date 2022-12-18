use std::fmt::Display;
use std::io::Read;

use nom::bytes::complete::tag;
use nom::character::complete::digit1;
use nom::multi::separated_list0;
use nom::sequence::delimited;
use nom::IResult;

fn main() {
    let mut buffer = vec![];
    std::io::stdin().read_to_end(&mut buffer).unwrap();
    let input = String::from_utf8(buffer).unwrap();
    let result_1 = part1(&input);

    println!("part1: {}", result_1);

    let result_2 = part2(&input);
    println!("part2: {}", result_2);
}

#[derive(Debug, Eq, Clone)]
enum SignalElement {
    Single(usize),
    Multiple(Vec<SignalElement>),
}

impl Display for SignalElement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SignalElement::Single(n) => f.write_fmt(format_args!("{}", n)),
            SignalElement::Multiple(v) => f.write_fmt(format_args!("{:?}", v)),
        }
    }
}

impl PartialEq for SignalElement {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Single(l0), Self::Single(r0)) => l0 == r0,
            (Self::Multiple(l0), Self::Multiple(r0)) => l0 == r0,
            (SignalElement::Multiple(m), SignalElement::Single(s))
            | (SignalElement::Single(s), SignalElement::Multiple(m)) => {
                vec![SignalElement::Single(*s)].eq(m)
            }
        }
    }
}

impl PartialOrd for SignalElement {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (SignalElement::Single(a), SignalElement::Single(b)) => a.partial_cmp(b),
            (m @ SignalElement::Multiple(_), SignalElement::Single(s)) => {
                m.partial_cmp(&SignalElement::Multiple(vec![SignalElement::Single(*s)]))
            }
            (SignalElement::Single(s), m @ SignalElement::Multiple(_)) => {
                SignalElement::Multiple(vec![SignalElement::Single(*s)]).partial_cmp(m)
            }
            (SignalElement::Multiple(left), SignalElement::Multiple(right)) => left
                .iter()
                .zip(right.iter())
                // Compare inner elements recursively
                .map(|(l, r)| l.cmp(r))
                // Check if there are any unequal elements, return the Ordering of the first
                // unequal element found.
                .find(|cmp| cmp.is_ne())
                // If all checked elements are the same, return the Ordering of the length of the
                // contents, as [] < [1] in our case
                .or_else(|| Some(left.len().cmp(&right.len()))),
        }
    }
}

impl Ord for SignalElement {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // PartialOrd implementation is already total, so we can simply unwrap it's result
        self.partial_cmp(other)
            .expect("PartialOrd implementation to be total")
    }
}

#[cfg(test)]
mod test_ord {
    use crate::SignalElement;

    #[test]
    fn test_cmp() {
        assert_eq!(
            SignalElement::Single(2),
            SignalElement::Multiple(vec![SignalElement::Single(2)])
        );
        assert_eq!(
            SignalElement::Multiple(vec![SignalElement::Single(2)]),
            SignalElement::Single(2)
        );
        assert_eq!(SignalElement::Single(2), SignalElement::Single(2));
        assert_eq!(
            SignalElement::Multiple(vec![
                SignalElement::Single(2),
                SignalElement::Single(2),
                SignalElement::Single(3),
                SignalElement::Single(3)
            ]),
            SignalElement::Multiple(vec![
                SignalElement::Single(2),
                SignalElement::Single(2),
                SignalElement::Single(3),
                SignalElement::Single(3)
            ])
        );

        assert!(
            SignalElement::Single(3)
                > SignalElement::Multiple(vec![
                    SignalElement::Single(2),
                    SignalElement::Single(1),
                    SignalElement::Single(2)
                ])
        );
        assert!(
            SignalElement::Single(2)
                < SignalElement::Multiple(vec![
                    SignalElement::Single(2),
                    SignalElement::Single(1),
                    SignalElement::Single(2)
                ])
        );
        assert!(
            SignalElement::Single(4)
                > SignalElement::Multiple(vec![
                    SignalElement::Single(3),
                    SignalElement::Single(1),
                    SignalElement::Single(2)
                ])
        );
        assert!(
            SignalElement::Single(3)
                > SignalElement::Multiple(vec![
                    SignalElement::Single(2),
                    SignalElement::Single(1),
                    SignalElement::Single(2)
                ])
        );

        assert!(
            SignalElement::Multiple(vec![
                SignalElement::Single(2),
                SignalElement::Single(1),
                SignalElement::Single(2),
                SignalElement::Single(2)
            ]) > SignalElement::Multiple(vec![
                SignalElement::Single(2),
                SignalElement::Single(1),
                SignalElement::Single(2)
            ])
        );
        assert!(
            SignalElement::Multiple(vec![
                SignalElement::Single(2),
                SignalElement::Single(1),
                SignalElement::Single(2)
            ]) < SignalElement::Multiple(vec![
                SignalElement::Single(2),
                SignalElement::Single(1),
                SignalElement::Single(2),
                SignalElement::Single(2)
            ])
        );

        assert_eq!(
            SignalElement::Multiple(vec![
                SignalElement::Multiple(vec![
                    SignalElement::Single(2),
                    SignalElement::Single(3),
                    SignalElement::Single(4)
                ]),
                SignalElement::Single(1),
                SignalElement::Single(2)
            ]),
            SignalElement::Multiple(vec![
                SignalElement::Multiple(vec![
                    SignalElement::Single(2),
                    SignalElement::Single(3),
                    SignalElement::Single(4)
                ]),
                SignalElement::Single(1),
                SignalElement::Single(2)
            ])
        );
    }
}

impl SignalElement {
    fn parse_single(input: &str) -> IResult<&str, Self> {
        nom::combinator::map_res(digit1, |v: &str| v.parse().map(Self::Single))(input)
    }

    fn parse_multiple(input: &str) -> IResult<&str, Self> {
        nom::combinator::map(
            delimited(
                tag("["),
                separated_list0(tag(","), SignalElement::parse),
                tag("]"),
            ),
            SignalElement::Multiple,
        )(input)
    }

    pub fn parse(input: &str) -> IResult<&str, Self> {
        nom::branch::alt((Self::parse_single, Self::parse_multiple))(input)
    }
}

#[cfg(test)]
mod test_parse {
    use crate::SignalElement;

    #[test]
    fn parse_single() {
        let input = "2";
        assert_eq!(
            SignalElement::parse_single(input),
            Ok(("", SignalElement::Single(2)))
        );

        let input_failed = "[2]";
        assert!(SignalElement::parse_single(input_failed).is_err());
    }

    #[test]
    fn parse_multiple() {
        let input = "[2,3,4]";
        assert_eq!(
            SignalElement::parse_multiple(input),
            Ok((
                "",
                SignalElement::Multiple(vec![
                    SignalElement::Single(2),
                    SignalElement::Single(3),
                    SignalElement::Single(4)
                ])
            ))
        );
    }

    #[test]
    fn parse_recursive() {
        let input = "[2,[2,3],[4,5,[6,7]]]";
        assert_eq!(
            SignalElement::parse_multiple(input),
            Ok((
                "",
                SignalElement::Multiple(vec![
                    SignalElement::Single(2),
                    SignalElement::Multiple(vec![
                        SignalElement::Single(2),
                        SignalElement::Single(3),
                    ]),
                    SignalElement::Multiple(vec![
                        SignalElement::Single(4),
                        SignalElement::Single(5),
                        SignalElement::Multiple(vec![
                            SignalElement::Single(6),
                            SignalElement::Single(7),
                        ]),
                    ]),
                ])
            ))
        );
    }
}

#[derive(Debug)]
struct Signal {}

fn part1(input: &str) -> usize {
    input
        .split("\n\n")
        .enumerate()
        .map(|(i, pair)| {
            if let [left, right] = &pair
                .lines()
                .map(SignalElement::parse)
                .filter_map(Result::ok)
                .map(|(_, a)| a)
                .collect::<Vec<_>>()[..]
            {
                if left <= right {
                    return i + 1;
                }
            }

            0
        })
        .sum()
}

fn part2(input: &str) -> usize {
    let mut signal = input
        .lines()
        .map(SignalElement::parse)
        .filter_map(Result::ok)
        .map(|(_, a)| a)
        .collect::<Vec<_>>();

    let divider_packet_2 =
        SignalElement::Multiple(vec![SignalElement::Multiple(vec![SignalElement::Single(
            2,
        )])]);
    let divider_packet_6 =
        SignalElement::Multiple(vec![SignalElement::Multiple(vec![SignalElement::Single(
            6,
        )])]);

    signal.push(divider_packet_2.clone());
    signal.push(divider_packet_6.clone());

    signal.sort();

    signal
        .into_iter()
        .enumerate()
        .map(|(i, s)| (i + 1, s))
        .filter_map(|(index, pkt)| {
            if pkt.eq(&divider_packet_2) || pkt.eq(&divider_packet_6) {
                Some(index)
            } else {
                None
            }
        })
        .product()
}

#[cfg(test)]
mod test {

    const INPUT: &str = include_str!("../input-example.txt");
    #[test]
    fn part1() {
        assert_eq!(super::part1(INPUT), 13);
    }

    #[test]
    fn part2() {
        assert_eq!(super::part2(INPUT), 140);
    }
}
