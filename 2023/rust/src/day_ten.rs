use nom::IResult;
use std::collections::HashMap;

pub fn run(fps: [&str; 2]) {
    //fps.iter().for_each(do_file);
    do_file(fps[0]);
}

fn do_file(fp: &str) {
    let contents = std::fs::read_to_string(fp)
        .expect("read file");

    let (_, parsed) = nom::combinator::all_consuming(parse)(&contents)
        .expect("parsed");

    let board: Board = parsed_to_board(&parsed);
    println!("board {:?}", board);
}

type Board = HashMap<(usize, usize), Pipe>;

fn parsed_to_board(cs: &Vec<Vec<char>>) -> Board {
    let mut board: Board = HashMap::new();

    for (r_idx, row) in cs.iter().enumerate() {
        println!("row {:?}", row);
        for (c_idx, col) in row.iter().enumerate() {
            match c_to_pipe(col) {
                None => continue,
                Some(p) => {
                    board.insert((r_idx, c_idx), p);
                }
            }
        }
    }

    board
}

#[derive(Debug)]
enum Pipe {
    NorthSouth,
    EastWest,
    NorthEast,
    NorthWest,
    SouthWest,
    SouthEast,
    Start,
}

fn c_to_pipe(c: &char) -> Option<Pipe> {
    match c {
		'|' => Some(Pipe::NorthSouth),
		'-' => Some(Pipe::EastWest),
		'L' => Some(Pipe::NorthEast),
		'J' => Some(Pipe::NorthWest),
		'7' => Some(Pipe::SouthWest),
		'F' => Some(Pipe::SouthEast),
		'S' => Some(Pipe::Start),
        _ => None
    }
}


fn parse(i: &str) -> IResult<&str, Vec<Vec<char>>> {
    let (rem, chars_table) = nom::multi::separated_list1(
        nom::character::complete::newline,
        nom::multi::many1(nom::character::complete::one_of(".F7|SJL-")),
    )(i)?;
    println!("parsed body ({})", rem);

    let (rem, _) = nom::sequence::tuple((
            nom::character::complete::newline,
            nom::combinator::eof,
    ))(rem)?;

    Ok((rem, chars_table))
}
