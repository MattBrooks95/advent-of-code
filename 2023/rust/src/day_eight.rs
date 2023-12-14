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
    println!("parsed {:?}", parsed);
        
}

#[derive(Debug)]
enum Direction {
    Left,
    Right
}

#[derive(Debug, Clone)]
struct Node(String, (String, String));

#[derive(Debug)]
struct Simulation {
    pattern: Vec<Direction>,
    graph: std::collections::HashMap<String, Node>,
    patternIndex: u64,
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
        patternIndex: 0,
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
