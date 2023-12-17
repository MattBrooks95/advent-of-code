use nom::IResult;
use std::collections::HashMap;

pub fn run(fps: [&str; 2]) {
    do_file(fps[0]);
    //fps.iter().for_each(do_file);
}

type Loc = (usize, usize);
type Board = HashMap<Loc, Entity>;

#[derive(Debug, Clone)]
enum Entity {
    Galaxy,
    EmptySpace
}

fn char_to_entity(c: char) -> Entity {
    match c {
        '#' => Entity::Galaxy,
        '.' => Entity::EmptySpace,
        _ => panic!("illegal char {}", c)
    }
}

fn do_file(fp: &str) {
    let contents = std::fs::read_to_string(fp)
        .expect("read file");
    let (_, parsed) = nom::combinator::all_consuming(parse)(&contents)
        .expect("parsed");
    println!("parsed {:?}", parsed);
    part_one(parsed);
}

fn part_one(parsed: Vec<Vec<char>>) -> () {
    let as_board = parsed_to_board(&parsed);
    println!("part1");
}

fn parsed_to_board(parsed: &Vec<Vec<char>>) -> Board {
    let mut as_map: Board = HashMap::new();

    let mut expanded_rows: Vec<Vec<char>> = Vec::new();
    //I can expand the rows easily, let's do that first
    for (r_idx, row) in parsed.iter().enumerate() {
        //if a row is empty, push in an extra empty row
        if is_all_empty_space(row) {
            println!("expanded row {}", r_idx);
            expanded_rows.push(row.clone());
        }
        expanded_rows.push(row.clone());
    }

    let mut column_order: Vec<Vec<char>> = Vec::new();
    //loop over the chars in the first row
    for (column_idx, _) in expanded_rows.first().unwrap().iter().enumerate() {
    }
    //for (row_idx, row) in expanded_rows.iter().enumerate() {
    //    for (col_idx, col) in row.iter().enumerate() {
    //    }
    //}

    as_map
}

fn is_all_empty_space(v: &Vec<char>) -> bool {
    match v.iter().find(|x| match char_to_entity(**x) { Entity::Galaxy => true, _ => false }) {
        Some(_) => false,
        _ => true
    }
}

fn parse(i: &str) -> IResult<&str, Vec<Vec<char>>> {
    let (rem, chars) = nom::multi::separated_list1(
        nom::character::complete::newline,
        nom::multi::many1(nom::character::complete::one_of("#."))
    )(i)?;

    let (rem, _) = nom::sequence::tuple((
        nom::character::complete::newline,
        nom::combinator::eof,
    ))(rem)?;

    Ok((rem, chars))
}
