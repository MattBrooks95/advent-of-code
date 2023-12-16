use nom::IResult;
use nom::combinator::eof;

pub fn run(fps: [&str; 2]) {
    do_file(fps[0]);
    fps.into_iter().for_each(do_file);
}

fn do_file(fp: &str) -> () {
    println!("day 9 file {}", fp);

    let contents = std::fs::read_to_string(fp)
        .expect("read file");

    let (_, parsed) = nom::combinator::all_consuming(parse)(&contents)
        .expect("parsed");
    println!("parsed {:?}", parsed);
    let extrapolated_values: Vec<i64> = parsed
        .iter()
        .map(solve_step)
        .collect();
    let part1_sum: i64 = extrapolated_values.iter().sum();
    println!("part 1 sum {}", part1_sum);
}

fn solve_step(initial: &Vec<i64>) -> i64 {
    let mut all_steps: Vec<Vec<i64>> = Vec::new();

    let mut new_vec: Vec<i64> = initial.clone();
    all_steps.push(new_vec.clone());
    while !all_zero(&new_vec) {
        new_vec = get_differences(&new_vec);
        all_steps.push(new_vec.clone());
    }

    println!("all steps {:?}", all_steps);
    //let mut final_items: Vec<u64> = Vec::new();
    let mut val_from_lower: i64 = 0;
    //let mut next: i64 = 0;
    for (idx, step) in all_steps.iter().rev().enumerate() {
        if idx == 0 { continue; }
        let last = step.last().unwrap();
        //next = last + val_from_lower;
        val_from_lower = last + val_from_lower;
        println!("val from lower {}", val_from_lower);
    }
    val_from_lower //TODO
}

fn get_differences(vs: &Vec<i64>) -> Vec<i64> {
    let vlen = vs.len();
    let first = match vs.first() { None => panic!("should be nonempty"), Some(v) => v };
    if *first != 0 && vlen == 1 {
        panic!("single item vector was not of value 0");
    }

    let mut prev: &i64 = vs.first().unwrap();

    let mut differences: Vec<i64> = Vec::new();
    for (idx, v) in vs.iter().enumerate() {
        if idx == 0 { continue; }
        differences.push(*v - prev);
        prev = v;
    }

    differences
}

fn all_zero(vs: &Vec<i64>) -> bool {
    for v in vs {
        if *v != 0 {
            return false;
        }
    }
    true
}

fn parse(i: &str) -> IResult<&str, Vec<Vec<i64>>> {
    let (rem, nums): (&str, Vec<Vec<i64>>) = nom::multi::separated_list1(
        nom::character::complete::newline,
        nom::multi::separated_list1(
            nom::character::complete::space1,
            nom::character::complete::i64
        )
    )(i)?;
    let (rem, _) = nom::sequence::tuple((
            nom::character::complete::newline,
            nom::combinator::eof
    ))(rem)?;

    Ok((rem, nums))
}
