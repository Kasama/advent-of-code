use std::io::Read;

fn main() {
    let mut buffer = vec![];
    std::io::stdin().read_to_end(&mut buffer).unwrap();
    let input = String::from_utf8(buffer).unwrap();
    let result_1 = part1(&input).unwrap();

    println!("part1: {}", result_1);

    let result_2 = part2(&input).unwrap();
    println!("part2: {}", result_2);
}

enum Shape {
    Rock,
    Paper,
    Scissors,
}

impl Shape {
    fn play(self, other: &Shape) -> RockPaperScissorsRound {
        match (&self, other) {
            (Self::Rock, Self::Rock)
            | (Self::Paper, Self::Paper)
            | (Self::Scissors, Self::Scissors) => RockPaperScissorsRound {
                outcome: Outcome::Draw,
                shape: self,
            },
            (Shape::Rock, Shape::Scissors)
            | (Shape::Paper, Shape::Rock)
            | (Shape::Scissors, Shape::Paper) => RockPaperScissorsRound {
                outcome: Outcome::Win,
                shape: self,
            },
            (Shape::Rock, Shape::Paper)
            | (Shape::Paper, Shape::Scissors)
            | (Shape::Scissors, Shape::Rock) => RockPaperScissorsRound {
                outcome: Outcome::Lose,
                shape: self,
            },
        }
    }

    fn score(&self) -> i64 {
        match self {
            Shape::Rock => 1,
            Shape::Paper => 2,
            Shape::Scissors => 3,
        }
    }

    fn from(repr: &str) -> Result<Self, String> {
        match repr {
            "A" | "X" => Ok(Self::Rock),
            "B" | "Y" => Ok(Self::Paper),
            "C" | "Z" => Ok(Self::Scissors),
            _ => Err(format!("{} is invalid", repr)),
        }
    }
}

enum Outcome {
    Win,
    Draw,
    Lose,
}

impl Outcome {
    fn score(&self) -> i64 {
        match self {
            Outcome::Win => 6,
            Outcome::Draw => 3,
            Outcome::Lose => 0,
        }
    }

    fn from(repr: &str) -> Result<Self, String> {
        match repr {
            "X" => Ok(Self::Lose),
            "Y" => Ok(Self::Draw),
            "Z" => Ok(Self::Win),
            _ => Err(format!("{} is invalid", repr)),
        }
    }
}

struct RockPaperScissorsRound {
    outcome: Outcome,
    shape: Shape,
}

impl RockPaperScissorsRound {
    fn score(&self) -> i64 {
        self.outcome.score() + self.shape.score()
    }

    fn what_to_play_for(outcome: &Outcome, oponent: Shape) -> Self {
        match (outcome, oponent) {
            (Outcome::Win, Shape::Rock) => Self {
                outcome: Outcome::Win,
                shape: Shape::Paper,
            },
            (Outcome::Win, Shape::Paper) => Self {
                outcome: Outcome::Win,
                shape: Shape::Scissors,
            },
            (Outcome::Win, Shape::Scissors) => Self {
                outcome: Outcome::Win,
                shape: Shape::Rock,
            },
            (Outcome::Lose, Shape::Rock) => Self {
                outcome: Outcome::Lose,
                shape: Shape::Scissors,
            },
            (Outcome::Lose, Shape::Paper) => Self {
                outcome: Outcome::Lose,
                shape: Shape::Rock,
            },
            (Outcome::Lose, Shape::Scissors) => Self {
                outcome: Outcome::Lose,
                shape: Shape::Paper,
            },
            (Outcome::Draw, oponent) => Self {
                outcome: Outcome::Draw,
                shape: oponent,
            },
        }
    }
}

fn part1(input: &str) -> Option<i64> {
    let lines = input.lines();

    Some(
        lines
            .map(|line| {
                let (a, b) = line.split_once(' ').unwrap();
                let oponent = Shape::from(a).unwrap();
                let player = Shape::from(b).unwrap();

                player.play(&oponent)
            })
            .map(|m| m.score())
            .sum::<i64>(),
    )
}

fn part2(input: &str) -> Option<i64> {
    let lines = input.lines();

    Some(
        lines
            .map(|line| {
                let (a, b) = line.split_once(' ').unwrap();
                let oponent = Shape::from(a).unwrap();
                let outcome = Outcome::from(b).unwrap();

                RockPaperScissorsRound::what_to_play_for(&outcome, oponent)
            })
            .map(|m| m.score())
            .sum::<i64>(),
    )
}

#[cfg(test)]
mod test {
    use crate::Shape;

    const INPUT: &str = r#"A Y
B X
C Z"#;
    #[test]
    fn part1() {
        assert_eq!(super::part1(INPUT), Some(15));
    }

    #[test]
    fn part1_parsed() {
        let rounds = vec![
            Shape::Rock.play(&Shape::Paper),
            Shape::Paper.play(&Shape::Rock),
            Shape::Scissors.play(&Shape::Scissors),
        ];

        let score = rounds.into_iter().map(|m| m.score()).sum::<i64>();

        assert_eq!(score, 15);
    }

    #[test]
    fn part2() {
        assert_eq!(super::part2(INPUT), Some(12));
    }
}
