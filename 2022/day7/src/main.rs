mod filesystem;
mod parser;
use std::cell::RefCell;
use std::io::Read;
use std::rc::Rc;

use nom::multi::many1;
use nom::IResult;

use self::filesystem::{CommandWithOutput, Directory, Filesystem};

fn main() {
    let mut buffer = vec![];
    std::io::stdin().read_to_end(&mut buffer).unwrap();
    let input = String::from_utf8(buffer).unwrap();
    let result_1 = part1(&input);

    println!("part1: {}", result_1);

    let result_2 = part2(&input);
    println!("part2: {}", result_2);
}

fn parse_input(input: &str) -> IResult<&str, Vec<CommandWithOutput>> {
    many1(parser::parse_command_line)(input)
}

fn parse_fs(input: &str) -> Filesystem {
    let (_, commands) = parse_input(input).expect("couldn't parse input");

    filesystem::Filesystem::build(commands.into_iter())
}

fn rec_dir_sizes(mut dirs: Vec<(String, i64)>, dir: Rc<RefCell<Directory>>) -> Vec<(String, i64)> {
    let d = dir.borrow();
    dirs.push((d.name.clone(), d.total_size()));

    let inner_dirs = d.entities.iter().filter_map(|e| match &*e.borrow() {
        filesystem::DirectoryEntity::File(_) => None,
        filesystem::DirectoryEntity::Dir(dir) => Some(dir.clone()),
    });

    inner_dirs.fold(dirs, rec_dir_sizes)
}

fn part1(input: &str) -> i64 {

    let max_size = 100000i64;

    let fs = parse_fs(input);

    let directory_sizes: Vec<(String, i64)> = rec_dir_sizes(vec![], fs.root);

    directory_sizes
        .iter()
        .filter(|dir| dir.1 < max_size)
        .fold(0, |acc, (_name, size)| acc + size)
}

fn part2(input: &str) -> i64 {
    let total_space = 70000000i64;
    let needed_space = 30000000i64;

    let fs = parse_fs(input);

    let total_used_space = {
        fs.root.borrow().total_size()
    };

    let directory_sizes: Vec<(String, i64)> = rec_dir_sizes(vec![], fs.root);

    let target_total_space = total_space - needed_space;

    directory_sizes
        .iter()
        .filter(|dir| {
            let remaining_space = total_used_space - dir.1;
            remaining_space <= target_total_space
        })
        .fold(total_space, |current_min, (_name, size)| {
            if current_min > *size {
                *size
            } else {
                current_min
            }
        })
}

#[cfg(test)]
mod test {

    const INPUT: &str = include_str!("../input-example.txt");
    #[test]
    fn part1() {
        assert_eq!(super::part1(INPUT), 95437);
    }

    #[test]
    fn part2() {
        assert_eq!(super::part2(INPUT), 24933642);
    }
}
