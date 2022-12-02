use std::error::Error;
use std::io::Read;

fn main() -> Result<(), Box<dyn Error>> {
    let mut buffer = Vec::new();
    std::io::stdin().read_to_end(&mut buffer)?;

    let input = std::str::from_utf8(&buffer).unwrap_or("");

    let result1 = run_part1(input);
    let result2 = run_part2(input);
    println!("part1 is {}", result1);
    println!("part2 is {}", result2);

    Ok(())
}

fn run_part1(input: &str) -> i64 {
    let mut val: i64 = 0;
    let mut tmp = 0i64;

    let lines = input.split('\n');

    for line in lines {
        if line.is_empty() {
            val = if tmp > val { tmp } else { val };
            tmp = 0;
        }
        tmp += line.parse::<i64>().unwrap_or(0);
    }

    val
}

fn run_part2(input: &str) -> i64 {
    let mut vals: Vec<i64> = vec![];
    let mut tmp = 0i64;

    let lines = input.split('\n');

    for line in lines {
        if line.is_empty() {
            vals.push(tmp);
            tmp = 0;
        }
        tmp += line.parse::<i64>().unwrap_or(0);
    }

    vals.sort_by(|a, b| b.cmp(a));

    vals.into_iter()
        .take(3)
        .reduce(|acc, v| acc + v)
        .unwrap_or(0)
}

#[cfg(test)]
mod test {
    use crate::{run_part1, run_part2};

    #[test]
    fn test_case1() {
        let input = r#"1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"#;

        assert_eq!(run_part1(input), 24000);
    }

    #[test]
    fn test_case2() {
        let input = r#"1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"#;

        assert_eq!(run_part2(input), 45000);
    }
}
