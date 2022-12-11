use nom::bytes::complete::{tag, take_till};
use nom::character::complete::{alpha1, digit1, line_ending, not_line_ending, space1};
use nom::combinator::map_res;
use nom::multi::many0;
use nom::IResult;

use crate::filesystem::{self, Command, OutputDirectoryEntity};

fn parse_cd(input: &str) -> IResult<&str, Command> {
    nom::combinator::map(
        nom::sequence::tuple((
            tag("cd"),
            space1,
            nom::branch::alt((tag(".."), tag("/"), alpha1)),
        )),
        |(_, _, path)| match path {
            "/" => Command::CD(filesystem::CDDestination::Root),
            ".." => Command::CD(filesystem::CDDestination::Parent),
            p => Command::CD(filesystem::CDDestination::Child(p.to_owned())),
        },
    )(input)
}

fn parse_ls(input: &str) -> IResult<&str, Command> {
    nom::combinator::map(tag("ls"), |_| Command::LS)(input)
}

fn parse_command(input: &str) -> IResult<&str, Command> {
    nom::branch::alt((parse_ls, parse_cd))(input)
}

pub fn parse_command_line(input: &str) -> IResult<&str, filesystem::CommandWithOutput> {
    nom::combinator::map(
        nom::sequence::tuple((
            tag("$"),
            space1,
            parse_command,
            line_ending,
            nom::combinator::map_parser(take_till(|c| c == '$'), parse_command_output),
        )),
        |(_, _, cmd, _, output)| match cmd {
            Command::LS => filesystem::CommandWithOutput::LS(output),
            Command::CD(d) => filesystem::CommandWithOutput::CD(d),
        },
    )(input)
}

fn parse_ls_dir_entry(input: &str) -> IResult<&str, OutputDirectoryEntity> {
    nom::combinator::map(
        nom::sequence::tuple((tag::<&str, &str, _>("dir"), space1, alpha1, line_ending)),
        |(_, _, name, _)| OutputDirectoryEntity::Dir(name.to_owned()),
    )(input)
}

fn parse_ls_file_entry(input: &str) -> IResult<&str, OutputDirectoryEntity> {
    nom::combinator::map(
        nom::sequence::tuple((
            map_res(digit1, |n: &str| n.parse::<i64>()),
            space1,
            not_line_ending,
            line_ending,
        )),
        |(size, _, name, _)| OutputDirectoryEntity::File((size, name.to_owned())),
    )(input)
}

fn parse_command_output(input: &str) -> IResult<&str, Vec<OutputDirectoryEntity>> {
    many0(nom::branch::alt((parse_ls_dir_entry, parse_ls_file_entry)))(input)
}

#[cfg(test)]
mod test {
    use crate::filesystem::{CommandWithOutput, OutputDirectoryEntity};

    use super::parse_command_line;

    #[test]
    fn parse_ls() {
        let input = "$ ls\ndir b\n19283 text.txt\n$ ls";

        assert_eq!(
            parse_command_line(input),
            Ok((
                "$ ls",
                (CommandWithOutput::LS(vec![
                    OutputDirectoryEntity::Dir("b".to_owned()),
                    OutputDirectoryEntity::File((19283, "text.txt".to_owned()))
                ]))
            ))
        )
    }

    #[test]
    fn parse_cd() {
        let input = "$ cd batatinha\n$ ls";

        assert_eq!(
            parse_command_line(input),
            Ok((
                "$ ls",
                CommandWithOutput::CD(crate::filesystem::CDDestination::Child(
                    "batatinha".to_owned()
                ))
            ))
        )
    }

    #[test]
    fn parse_cd_root() {
        let input = "$ cd /\n$ ls";

        assert_eq!(
            parse_command_line(input),
            Ok((
                "$ ls",
                CommandWithOutput::CD(crate::filesystem::CDDestination::Root)
            ))
        )
    }

    #[test]
    fn parse_error() {
        let input = "1293843 a.txt\n";

        let output = parse_command_line(input);
        assert!(output.is_err(), "expected err, got {:?}", output)
    }
}
