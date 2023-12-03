use nom::IResult;
use nom::bytes::complete::tag;

pub fn run(file_paths: [&str; 2]) {
    println!("day 2");
    file_paths
        .into_iter()
        .for_each(do_file)
        ;
}

#[derive(Debug)]
enum Count {
    RedCount(i32),
    BlueCount(i32),
    GreenCount(i32),
}

const RED_LIMIT: i32 = 12;
const GREEN_LIMIT: i32 = 13;
const BLUE_LIMIT: i32 = 14;

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

#[derive(Debug)]
struct Hint {
    red: Count,
    blue: Count,
    green: Count,
}

/** how to de-duplicate these add methods???? */
fn add_red(h: &mut Hint, v: &i32) {
    let Count::RedCount(prev) = h.red else { panic!("how do I handle this, I know it's red") };
    h.red = Count::RedCount(prev + v);
}

fn add_blue(h: &mut Hint, v: &i32) {
    let Count::BlueCount(prev) = h.blue else { panic!("how do I handle this, I know it's blue") };
    h.blue = Count::BlueCount(prev + v);
}

fn add_green(h: &mut Hint, v: &i32) {
    let Count::GreenCount(prev) = h.green else { panic!("how do I handle this, I know it's green") };
    h.green = Count::GreenCount(prev + v);
}

fn add_color(h: &mut Hint, c: &Count) {
    match c {
        Count::RedCount(val) => add_red(h, val),
        Count::BlueCount(val) => add_blue(h, val),
        Count::GreenCount(val) => add_green(h, val),
    }
}

#[derive(Debug)]
struct Game {
    id: i32,
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
    let contents = std::fs::read_to_string(file_path)
        .expect("read file");
    let (_, games) = nom::multi::many1(parse_line)(&contents)
        .expect("parsed into games");
    println!("{:?}", games);
}

fn parse_line(input: &str) -> IResult<&str, Game> {
    let (rem, id) = parse_game_id(input)?;
    let (rem, hints) = parse_hints(rem)?;
    //remove the newline
    let (rem, _) = nom::character::complete::line_ending(rem)?;
    Ok((
        rem,
        Game {
            id,
            hints
        }
    ))
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
    let new_hint = mk_hint(&color_counts);
    Ok((rem, new_hint))
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
