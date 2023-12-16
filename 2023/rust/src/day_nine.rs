use nom::IResult;
use nom::combinator::eof;

pub fn run(fps: [&str; 2]) {
    do_file(fps[0]);
}

fn do_file(fp: &str) -> () {
    println!("day 9 file {}", fp);

    let contents = std::fs::read_to_string(fp)
        .expect("read file");

    let (_, parsed) = nom::combinator::all_consuming(parse)(&contents)
        .expect("parsed");
    println!("parsed {:?}", parsed);
}

fn parse(i: &str) -> IResult<&str, Vec<Vec<u64>>> {
    let (rem, nums): (&str, Vec<Vec<u64>>) = nom::multi::separated_list1(
        nom::character::complete::newline,
        nom::multi::many1(nom::character::complete::u64)
    )(i)?;
    let (rem, _) = nom::sequence::tuple((
            nom::character::complete::newline,
            nom::combinator::eof
    ))(rem)?;

    Ok((rem, nums))
}
