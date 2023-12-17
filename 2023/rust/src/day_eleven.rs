use nom::IResult;
use std::collections::HashMap;

pub fn run(fps: [&str; 2]) {
    do_file(fps[0]);
    //fps.iter().for_each(do_file);
}

type Loc = (usize, usize);
#[derive(Debug, Clone)]
struct Galaxy {
    id: usize,
    loc: Loc,
}

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
    println!("part1");
    let galaxies = get_galaxies(&parsed);

    println!("galaxies {:?}", galaxies.values());
}

fn get_galaxies(parsed: &Vec<Vec<char>>) -> HashMap<Loc, Galaxy> {
    let mut galaxies: HashMap<Loc, Galaxy> = HashMap::new();
    let mut galaxy_id: usize = 0;

    for (r_idx, row) in parsed.iter().enumerate() {
        for (c_idx, char) in row.iter().enumerate() {
            match char {
                '#' => {
                    let loc = (r_idx, c_idx);
                    galaxies.insert(loc, Galaxy { id: galaxy_id, loc });
                    galaxy_id += 1
                },
                _ => ()
            }

        }
    }

    galaxies
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
