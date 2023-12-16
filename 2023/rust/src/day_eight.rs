use std::cell::Cell;
use std::cell::RefCell;

use nom::IResult;
use nom::character::complete::multispace1;
use nom::character::complete::newline;
use nom::combinator::eof;
use nom::bytes::complete::tag;
use nom::character::complete::char;
use nom::character::complete::alpha1;
use nom::sequence::tuple;

pub fn run(fps: [&str; 2]) {
    //do_file(fps[0]);
    fps
        .into_iter()
        .for_each(do_file);
}

fn do_file(fp: &str) {
    let contents = std::fs::read_to_string(fp)
        .expect("read file");
    let (_, parsed) = nom::combinator::all_consuming(parse)(&contents)
        .expect("parsed input file");
    //println!("parsed {:?}", parsed);

    let target: String = "ZZZ".into();
        
    let final_sim = solve_p1(&parsed, &target);
    println!("made {} moves", final_sim.pattern_index.get());
    println!("part 2");
    let start_nodes: Vec<&String> = parsed.graph
        .iter()
        .filter(|n| {
            match n.0.chars().last() {
                None => panic!("empty location string"),
                Some(last_c) => last_c == 'A'
            }
        }).map(|n| n.0)
        .collect();

    let part2_sim_start = parsed.clone();
    println!("part2 start locs {:?}", start_nodes);
    //let part2_final_sim = solve_p2(&part2_sim_start, start_nodes);
    //println!("p2 answer {}", part2_final_sim.pattern_index.get());
}

fn solve_p1<'a>(s: &'a Simulation, final_dest: &String) -> &'a Simulation {
    let (curr_index, mod_index) = get_pattern_indices(&s);
    let current_node_name = s.current_node.borrow().clone();
    if *current_node_name == *final_dest {
        return s;
    }

    let direction = s.pattern.get(mod_index)
        .expect("was able to index into pattern");

    let Node(_, (left_edge, right_edge)): &Node = s.graph.get(&current_node_name)
        .expect("node existed");
    let next_node = match direction {
        Direction::Left => left_edge,
        Direction::Right => right_edge,
    };

    s.pattern_index.replace(curr_index + 1);
    s.current_node.replace(next_node.clone());
    solve_p1(s, final_dest)
}

fn solve_p2<'a>(s: &'a Simulation, locations: Vec<&String>) -> &'a Simulation {
    let (curr_index, mod_index) = get_pattern_indices(&s);

    if all_locs_at_z(&locations) {
        return s
    };

    let direction = s.pattern.get(mod_index)
        .expect("was able to index into pattern");
    let updated_locs: Vec<&String> = locations
        .into_iter()
        .map(|node_n| {
            let Node(_, (left_edge, right_edge)): &Node = s.graph.get(node_n)
                .expect("node existed");
            let next_node = match direction {
                Direction::Left => left_edge,
                Direction::Right => right_edge,
            };
            next_node
        }).collect();

    s.pattern_index.replace(curr_index + 1);

    solve_p2(s, updated_locs)
}

fn all_locs_at_z(locations: &Vec<&String>) -> bool {
    for loc in locations {
        let c = match loc.chars().last() {
            None => panic!("string should be nonempty"),
            Some(_c) => _c,
        };

        if c != 'Z' {
            return false;
        }
    }
    true
}

/** gets the current index and the modulo of that index w/ the length of the vector */
fn get_pattern_indices(sim: &Simulation) -> (usize, usize) {
    let curr_pattern_index = sim.pattern_index.get();
    (curr_pattern_index, curr_pattern_index % sim.pattern.len())
}

#[derive(Debug, Clone)]
enum Direction {
    Left,
    Right
}

#[derive(Debug, Clone)]
struct Node(String, (String, String));

#[derive(Debug, Clone)]
struct Simulation {
    pattern: Vec<Direction>,
    graph: std::collections::HashMap<String, Node>,
    pattern_index: Cell<usize>,
    current_node: RefCell<String>,
}

fn parse(i: &str) -> IResult<&str, Simulation> {
    let (rem, directions) = nom::multi::many1(
        parse_direction
    )(i)?;
    let (rem, _) = tuple((newline, newline))(rem)?;

    let (rem, parsed_nodes) = parse_nodes(rem)?;

    let (rem, _) = tuple((newline, eof))(rem)?;


    let node_map: std::collections::HashMap<String, Node> = parsed_nodes
        .into_iter()
        .map(|node| {
            let name = node.0.clone();
            let edges = node.1;
            (name, Node(node.0, edges))
        })
        .collect();

    Ok((rem, Simulation {
        pattern: directions,
        graph: node_map,
        pattern_index: Cell::new(0),
        current_node: RefCell::new(String::from("AAA")),
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

fn parse_nodes(i: &str) -> IResult<&str, Vec<Node>> {
    nom::multi::separated_list1(
        newline,
        parse_node
    )(i)
}

fn parse_node(i: &str) -> IResult<&str, Node> {
    let (rem, name) = alpha1(i)?;
    let (rem, _) = tuple((
        multispace1,
        char('='),
        multispace1,
    ))(rem)?;
    let (rem, _) = char('(')(rem)?;
    let (rem, left) = alpha1(rem)?;
    let (rem, _) = tuple((
        char(','),
        multispace1,
    ))(rem)?;
    let (rem, right) = alpha1(rem)?;
    let (rem, _) = char(')')(rem)?;

    Ok((
        rem,
        Node(String::from(name), (String::from(left), String::from(right)))
    ))
}
