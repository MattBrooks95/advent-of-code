use nom::IResult;
use nom::bytes::complete::tag;

pub fn run(file_paths: [&str; 2]) {
    println!("day 2");
    file_paths
        .into_iter()
        .for_each(do_file)
        ;
}

enum Count {
    RedCount(i32),
    BlueCount(i32),
    GreenCount(i32),
}

const RED_LIMIT: usize = 12;
const GREEN_LIMIT: usize = 13;
const BLUE_LIMIT: usize = 14;

/**
 * TRUE -> Possible
 * FALSE -> Impossible
 **/
fn check_limit(count: &Count) -> bool {
    let (limit, val) = match count {
        Count::RedCount(val) => (RED_LIMIT, val),
        Count::GreenCount(val) => (GREEN_LIMIT, val),
        Count::BlueCount(val) => (BLUE_LIMIT, val),
    };

    *val <= limit
}

struct Hint {
    red: Count,
    blue: Count,
    green: Count,
}

fn add_red(h: &mut Hint, v: i32) {
    let (Count::RedCount = h.red;
}

fn add_color(h: &mut Hint, c: &Count) {
    match c {
        Count::RedCount(val) => add_red(h, val),
        Count::BlueCount(val) => ,
        Count::GreenCount(val) => ,
    }
}

struct Game {
    id: usize,
    hints: Vec<Hint>,
}

fn mk_color((color, num): (&str, i32)) -> Option<Count> {
    match color {
        "blue" => Some(Count::BlueCount(num)),
        "red" => Some(Count::RedCount(num)),
        "green" => Some(Count::GreenCount(num)),
        _ => None
    }
}

fn do_file(file_path: &str) {
    println!("file:{}", file_path);
}

fn parse_line(input: &str) -> IResult<&str, Game> {
    let (rem, id) = parse_game_id(input)?;
}

fn parse_game_id(input: &str) -> IResult<&str, i32> {
    let (rem, _) = tag("Game ")(input)?;
    let (rem, id) = nom::character::complete::i32(input)?;
    //throw away the colon and the space
    let (rem, _) = tag(": ")(input)?;
    Ok((rem, id))
}

fn parse_hints(input: &str) -> IResult<&str, Vec<Hint>> {
    nom::multi::separated_list1(
        tag("; "),
        parse_hint_instance
    )(input)
}

fn parse_hint_instance(input: &str) -> IResult<&str, Hint> {
    let (rem, tupes) = nom::multi::separated_list1(
        tag(", "),
        parse_color_hint
    )(input)?;
    let color_counts: Vec<Count> = tupes
        .into_iter()
        .map(mk_color)
        .filter(Option::is_some)
        .map(Option::unwrap)
        .collect()
        ;
}

fn mk_hint(colors: &Vec<Count>) -> Hint {
    let mut h = Hint {
        red: Count::RedCount(0),
        blue: Count::BlueCount(0),
        green: Count::GreenCount(0),
    };

    colors
        .iter()
        .for_each(|c| add_color(&mut h, c));
    h
}

fn parse_color_hint(input: &str) -> IResult<&str, (&str, i32)> {
    let (rem, num) = nom::character::complete::i32(input)?;
    //throw away the space in between the number and the color
    let (rem, _) = nom::character::complete::multispace1(rem)?;
    let (rem, color) = nom::branch::alt((tag("blue"), tag("red"), tag("green")))(rem)?;
    Ok((rem, (color, num)))
}
