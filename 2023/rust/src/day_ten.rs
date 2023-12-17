use nom::IResult;
use std::collections::HashMap;
use std::collections::HashSet;

pub fn run(fps: [&str; 2]) {
    fps.into_iter().for_each(do_file);
    //do_file(fps[0]);
}

fn do_file(fp: &str) {
    let contents = std::fs::read_to_string(fp)
        .expect("read file");

    let (_, parsed) = nom::combinator::all_consuming(parse)(&contents)
        .expect("parsed");

    let (board, opt_s_loc): (Board, Option<Loc>) = parsed_to_board(&parsed);
    println!("board {:?} start location {:?}", board, opt_s_loc);
    let s_loc = match opt_s_loc {
        None => panic!("couldn't find starting node"),
        Some(s) => s
    };

    let mut visit_list: HashSet<Loc> = HashSet::new();
    let mut distances: PipeDistances = HashMap::new();
    let pipe_distances = decorate_distances(
        &board,
        &mut visit_list,
        &mut distances,
        s_loc,
        0
    );
    println!("pipe distances {:?}", pipe_distances);
    let max_dist = pipe_distances
        .values()
        .max()
        .unwrap();
    let as_float: f64 = *max_dist as f64;

    //+1 is because decorate_distances stops before looking at the start again
    println!("half distance {}", ((as_float + 1.0) / 2_f64));
}

type Loc = (i64, i64);

type Board = HashMap<Loc, Pipe>;

type PipeDistances = HashMap<Loc, u64>;

fn decorate_distances<'a>(
    b: &Board,
    visit_list: &mut HashSet<Loc>,
    curr_distances: &'a mut PipeDistances,
    curr_loc: Loc,
    curr_dist: usize
) -> &'a PipeDistances {
    println!("curr loc {:?} curr dist {:?}", curr_loc, curr_dist);
    let node = match b.get(&curr_loc) {
        None => panic!("pushed an illegal index into the visit queue"),
        Some(x) => x,
    };

    let is_start = match node { Pipe::Start => true, _ => false };

    //found start again, leave
    if is_start && curr_dist != 0 {
        println!("found start {:?}", curr_loc);
        return curr_distances;
    }

    let possible_neighbors = get_neighbors(b, &curr_loc);
    //we are in a loop (the puzzle input map), so we can pick one
    let can_go_to: Vec<_> = can_go(&possible_neighbors, node.clone())
        .into_iter()
        .filter(|x| {
            !visit_list.contains(x)
        })
        .collect();

    curr_distances.insert(curr_loc, curr_dist as u64);

    //nowhere to go
    if can_go_to.is_empty() {
        println!("nowhere to go {:?} || nowhere to go except start", curr_loc);
        return curr_distances;
    }

    //mark this node as visited
    visit_list.insert(curr_loc);

    decorate_distances(
        b,
        visit_list,
        curr_distances,
        can_go_to.first().unwrap().clone(),
        curr_dist + 1
    )
}

type Neighbors = (Option<Neighbor>, Option<Neighbor>, Option<Neighbor>, Option<Neighbor>);
type Neighbor = (Loc, Pipe);
/** returns the 4 indexes adjacent to the specified location
 * in (north, east, south, west) order
 **/
fn get_neighbors(
    b: &Board,
    (r, c): &Loc
) -> (Option<Neighbor>, Option<Neighbor>, Option<Neighbor>, Option<Neighbor>) {
    let north_i = (r - 1, *c);
    let south_i = (r + 1, *c);
    let east_i = (*r, c + 1);
    let west_i = (*r, c - 1);

    let north = b.get(&north_i);
    let south = b.get(&south_i);
    let east = b.get(&east_i);
    let west = b.get(&west_i);

    (
        option_tuple(north_i, north),
        option_tuple(east_i, east),
        option_tuple(south_i, south),
        option_tuple(west_i, west)
    )
}

fn can_go((n, e, s, w): &Neighbors, node: Pipe) -> Vec<Loc> {
    let mut possible: Vec<Option<(Loc, Pipe)>> = Vec::new();
    match node {
        Pipe::NorthSouth => {
            //vec!(n.map(|x| x.0), s.map(|x| x.0))
            possible.push(n.clone());
            possible.push(s.clone());
        },
        Pipe::NorthEast => {
            possible.push(n.clone());
            possible.push(e.clone());
        },
        Pipe::NorthWest => {
            possible.push(n.clone());
            possible.push(w.clone());
        },
        Pipe::EastWest => {
            possible.push(e.clone());
            possible.push(w.clone());
        },
        Pipe::SouthEast => {
            possible.push(s.clone());
            possible.push(e.clone());
        },
        Pipe::SouthWest => {
            possible.push(s.clone());
            possible.push(w.clone());
        },
        Pipe::Start => {
            //let mut from_start: Vec<Loc> = Vec::new();
            match n {
                None => (),
                Some((loc, p)) => match p {
                    Pipe::NorthSouth | Pipe::SouthEast | Pipe::SouthWest => {
                        possible.push(n.clone());
                    },
                    _ => (),
                },
            };
            match e {
                None => (),
                Some((loc, p)) => match p {
                    Pipe::EastWest | Pipe::SouthEast | Pipe::NorthEast => {
                        possible.push(e.clone());
                    },
                    _ => (),
                }
            };
            match s {
                None => (),
                Some((loc, p)) => match p {
                    Pipe::NorthEast | Pipe::NorthSouth | Pipe::NorthWest => {
                        possible.push(s.clone())
                    }
                    _ => ()
                }
            };
            match w {
                None => (),
                Some((loc, p)) => match p {
                    Pipe::EastWest | Pipe::SouthEast | Pipe::NorthEast => {
                        possible.push(w.clone());
                    }
                    _ => ()
                }
            };
        },
    };
    possible
        .into_iter()
        .filter(|x| Option::is_some(x))
        .map(Option::unwrap)
        .map(|(loc, _)| {
            loc
        })
        .collect()
}

fn option_tuple<A, B: Clone>(x: A, y: Option<&B>) -> Option<(A, B)> {
    match y {
        None => None,
        Some(just_y) => Some((x, just_y.clone()))
    }
}

/** returns a map of (row, col) => pipe type, and the location of the
 * start pipe */
fn parsed_to_board(cs: &Vec<Vec<char>>) -> (Board, Option<Loc>) {
    let mut board: Board = HashMap::new();
    let mut s_loc: Option<Loc> = None;

    for (r_idx, row) in cs.iter().enumerate() {
        //println!("row {:?}", row);
        for (c_idx, col) in row.iter().enumerate() {
            let this_loc: Loc = (r_idx as i64, c_idx as i64);
            match c_to_pipe(col) {
                None => continue,
                Some(s@Pipe::Start) => {
                    s_loc = Some(this_loc.clone());
                    board.insert(this_loc, s);
                },
                Some(p) => {
                    board.insert(this_loc, p);
                }
            }
        }
    }

    (board, s_loc)
}

#[derive(Debug, Clone)]
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
