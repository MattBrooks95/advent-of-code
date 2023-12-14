use nom::IResult;
use nom::character::complete::newline;
use nom::combinator::eof;
use nom::bytes::complete::tag;
use nom::character::complete::char;

pub fn run(fps: [&str; 2]) {
    do_file(fps[0]);
}

fn do_file(fp: &str) {
    let contents = std::fs::read_to_string(fp)
        .expect("read file");
    let (_, parsed) = nom::combinator::all_consuming(parse)(&contents)
        .expect("parsed input file");
        
}

enum Direction {
    Left,
    Right
}

type ThreeLetters = (char, char, char);

struct Simulation {
    pattern: Vec<Direction>,
    graph: std::collections::HashMap<ThreeLetters, (ThreeLetters, ThreeLetters)>,
}

fn parse(i: &str) -> IResult<&str, Simulation> {
    let (rem, directions) = nom::multi::many1(
        parse_direction
    )(i)?;

    Ok((rem, Simulation {
        pattern: directions,
        graph: std::collections::HashMap::new()
    }))
}

fn parse_direction(i: &str) -> IResult<&str, Direction> {
    let (rem, c) = nom::branch::alt((
            char('L'),
            char('R'),
        ))(i)?;
    let direction = match c {
        'L' => Direction::Left,
        'R' => Direction::Right,
        _ => panic!("should only match L or R"),
    };

    Ok((rem, direction))
}
