use nom::IResult;
use std::collections::HashMap;
use itertools::Itertools;

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
    let mut galaxies = get_galaxies(&parsed);
    let expand_rows = get_expand_rows(&parsed);
    let expand_cols = get_expand_cols(&parsed);

    println!(
        "galaxies {:?}\nexpand rows {:?}\nexpand cols:{:?}",
        galaxies.values(),
        expand_rows,
        expand_cols,
    );

    expand_rows.iter().for_each(|expand_row_index| {
        galaxies
            .values_mut()
            .for_each(|Galaxy { loc: (r_idx, _), .. }| {
                if *r_idx > *expand_row_index {
                    *r_idx += 1
                }
            });
    });
    expand_cols.iter().for_each(|expand_col_index| {
        galaxies
            .values_mut()
            .for_each(|Galaxy { loc: (_, c_idx), .. }| {
                if *c_idx > *expand_col_index {
                    *c_idx += 1
                }
            });
    });
    println!("adjusted galaxies {:?}", galaxies);
    let galaxy_ids: Vec<usize> = galaxies
        .values()
        .map(|Galaxy { id, .. }| *id).collect();
    //map of the distances between galaxies
    //when the distance from g1 to g2 is calculated, the answer will be
    //put into the map twice, at (g1, g2) and at (g2, g1) to prevent duplicating
    //the distance calculations
    //the distance from g1 to g2 is the same as g2 to g1
    let mut distances_map: HashMap<(usize, usize), usize> = HashMap::new();
    let g_id_combinations = galaxy_ids.iter().combinations(2);
    for ids in g_id_combinations {
        let id1: usize = **ids.first().unwrap();
        let id2: usize = **ids.last().unwrap();

        if distances_map.contains_key(&(id1, id2)) {
            continue
        }
        let loc1 = 

        let dist = distance(
    }
}

fn distance((r1, c1): Loc, (r2, c2): Loc) -> i64 {
    let y_dist: i64 = r2 as i64 - r1 as i64;
    let x_dist: i64 = c2 as i64 - c1 as i64;


    (y_dist + x_dist).abs()

}

fn get_expand_cols(parsed: &Vec<Vec<char>>) -> Vec<usize> {
    let mut needs_expand_columns: Vec<usize> = Vec::new();
    let first_row_len = parsed.first().unwrap().len();
    for idx in 0..first_row_len {
        let chars_in_col: Vec<char> = parsed
            .iter()
            .map(|r| r[idx])
            .collect();
        if is_all_empty_space(&chars_in_col) {
            needs_expand_columns.push(idx)
        }
    }

    needs_expand_columns
}

fn get_expand_rows(parsed: &Vec<Vec<char>>) -> Vec<usize> {
    let needs_expanded: Vec<usize> = parsed
        .iter()
        .enumerate()
        .filter(|(_, r)| is_all_empty_space(&r))
        .map(|(idx, _)| idx)
        .collect();
    needs_expanded
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
