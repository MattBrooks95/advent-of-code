use nom::IResult;
use std::collections::HashMap;
use std::collections::HashSet;

pub fn run(fps: [&str; 2]) {
    //fps.iter().for_each(do_file);
    do_file(fps[0]);
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
    let pipe_distances = decorate_distances(&board, s_loc);
    println!("pipe distances {:?}", pipe_distances);
}

type Loc = (i64, i64);

type Board = HashMap<Loc, Pipe>;

type PipeDistances = HashMap<Loc, u64>;

fn decorate_distances(b: &Board, start_loc: Loc) -> PipeDistances {
    let mut p_dist = HashMap::new();

    let mut visit_list: HashSet<Loc> = HashSet::new();
    let mut visit_q: Vec<Loc> = Vec::new();
    visit_q.push(start_loc);

    let mut distance: u64 = 0;

    let mut found_loop = false;

    while !found_loop {
        //process all the nodes at the current distance, adding their
        //neighbors to the queue
        let mut visit_nodes: Vec<_> = visit_q.into_iter().collect();
        visit_q = Vec::new();
        while !visit_nodes.is_empty() {
            //get the next node off the process queue
            let this_loc = match visit_nodes.pop() {
                None => panic!("popped off an empty q"),
                Some(x) => x,
            };

            println!("visiting {:?}", this_loc);
            if visit_list.contains(&this_loc) {
                found_loop = true;
                println!("found loop");
            } else {
                visit_list.insert(this_loc);
            }

            //mark the node we're visitting with its distance
            p_dist.insert(this_loc, distance);

            let node = match b.get(&this_loc) {
                None => panic!("pushed an illegal index into the visit queue"),
                Some(x) => x,
            };

            let neighbors = get_neighbors(b, &this_loc);
            //push the available neighbors into the visit queue
            can_go(&neighbors, node.clone())
                .iter()
                .for_each(|x| {
                    if !visit_list.contains(&x.0) {
                        visit_q.push(x.0);
                    }
                });
        }
        distance += 1;
    }

    p_dist
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

fn can_go((n, e, s, w): &Neighbors, node: Pipe) -> Vec<(Loc, Pipe)> {
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
        .iter()
        .filter(|x| Option::is_some(*x))
        .map(|x| Option::unwrap(x.clone()))
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
