use nom::IResult;

pub fn run(fps: [&str; 2]) {
    do_file(fps[0]);
    //fps.iter().for_each(do_file);
}

fn do_file(fp: &str) {
    let contents = std::fs::read_to_string(fp)
        .expect("read file");
    let (_, parsed) = nom::combinator::all_consuming(parse)(&contents)
        .expect("parsed");
    println!("parsed {:?}", parsed);
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
