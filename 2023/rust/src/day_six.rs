use nom::IResult;
use nom::character::complete::newline;

pub fn run(fps: [&str; 2]) {
    //do_file(fps[0]);
    fps
        .into_iter()
        .for_each(do_file);
}

fn do_file(fp: &str) {
    println!("solving {}", fp);
    let contents = std::fs::read_to_string(fp)
        .expect("successfully read file");
    println!("{}", contents);

    let (_, parsed_result) = nom::combinator::all_consuming(parse)(&contents)
        .expect("successfully parsed file contents");
    println!("parsed res:{:?}", parsed_result);
    part1(
        &parsed_result,
    );
    part2(
        &parsed_result,
    );
}

fn part1(
    races: &Vec<(Time, Distance)>,
) -> () {
    let equation_results: Vec<(u64, u64)> = races
        .iter()
        .map(|(t, d)| {
            equation(t.clone(), d.clone())
        })
        .collect();
    println!("equation results {:?}", equation_results);
    let range_magnitudes: Vec<u64> = equation_results
        .iter()
        .map(|(high, low)| {
             high - low + 1
        }).collect();
    println!("range magnitudes {:?}", range_magnitudes);
    let product: u64 = range_magnitudes.iter().product();
    println!("range product {}", product);
}

fn part2(races: &Vec<(Time, Distance)>) -> () {
    println!("###### part 2 ######");
    let times: Vec<u64> = races.iter().map(|(Time(t), _)| *t).collect();
    let distances: Vec<u64> = races.iter().map(|(_, Distance(d))| *d).collect();

    let times_str: String = times
        .iter()
        .fold(
            String::new(),
            |acc: String, t: &u64| acc + &t.to_string()
        );
    let distances_str: String = distances
        .iter()
        .fold(
            String::new(),
            |acc: String, t: &u64| acc + &t.to_string()
        );

    let time: u64 = times_str.parse().unwrap();
    let dist: u64 = distances_str.parse().unwrap();
    let (max, min) = equation(Time(time), Distance(dist));
    let answer = max - min + 1;
    println!("part 2 answer {}", answer);
}

/** use the quadratic equation to solve for the (max, min) pair that expresses
 * the range of how long you can hold down the button and still beat the
 * record distance
 **/
fn equation(Time(time): Time, Distance(t_dist): Distance) -> (u64, u64) {
    let t = time as f64;
    let t_d = t_dist as f64;
    let square_root_section = ((-t).powi(2) - (4 as f64) * t_d).sqrt();
    //plus case
    let plus_result = (t + square_root_section) / 2.0;
    let minus_result = (t - square_root_section) / 2.0;

    let floor_result: u64 = plus_result.floor() as u64;
    let ceil_result: u64 = minus_result.ceil() as u64;

    println!(
        "plus result: {} plus floored: {}\nminus result: {} minus ceiled: {}",
        plus_result,
        floor_result,
        minus_result,
        ceil_result,
    );

    let floor_travels = can_travel(floor_result, time);
    let ceil_travels = can_travel(ceil_result, time);
    println!("floor travels: {} ceil travels: {}", floor_travels, ceil_travels);
    //this is so hacky how did this work
    //if we didn't travel far enough, increase the lower bound by one
    //and decrease the upper bound by 1
    //found empirically on the short input
    if floor_travels > t_dist && ceil_travels > t_dist {
        return (floor_result, ceil_result);
    } else {
        return (floor_result - 1, ceil_result + 1);
    }
}

fn can_travel(held_time: u64, available_time: u64) -> u64 {
    (available_time - held_time) * held_time
}

#[derive(Debug, Clone)]
struct Time(u64);
#[derive(Debug, Clone)]
struct Distance(u64);

fn parse(i: &str) -> IResult<&str, Vec<(Time, Distance)>> {
    let (rem, _) = nom::sequence::tuple((
        nom::bytes::complete::tag("Time:"),
        nom::character::complete::multispace1,
    ))(i)?;

    let (rem, times_nums) = nom::multi::separated_list1(
        nom::character::complete::multispace1,
        nom::character::complete::u64,
    )(rem)?;

    //throw away junk in between the times and the distance line
    let (rem, _) = nom::sequence::tuple((
        newline,
        nom::bytes::complete::tag("Distance:"),
        nom::character::complete::multispace1,
    ))(rem)?;

    let (rem, distances_nums) = nom::multi::separated_list1(
        nom::character::complete::multispace1,
        nom::character::complete::u64,
    )(rem)?;

    let (rem, _) = nom::sequence::tuple((newline, nom::combinator::eof))(rem)?;

    assert!(
        times_nums.len() == distances_nums.len(),
        "time and distance vectors are the same length",
    );

    let times: Vec<Time> = times_nums
        .into_iter()
        .map(Time)
        .collect();
    let distances: Vec<Distance> = distances_nums
        .into_iter()
        .map(Distance)
        .collect();
    let zipped: Vec<(Time, Distance)> = times
        .into_iter()
        .zip(distances.into_iter())
        .collect();
    Ok((rem, zipped))
}
