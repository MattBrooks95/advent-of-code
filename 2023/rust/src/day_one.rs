use std::fs;

use nom::character::complete::digit1;
use nom::multi::many1;
use nom::error::context;
use nom::IResult;
use nom::bytes::complete::tag;

pub fn run(files: [String; 3]) {
    println!("Day One, running for files {} and {}", files[0], files[1]);
    //run just the short input from the example
    //run_file(&files[0]);
    //run for full input and short input
    files
        .iter()
        .for_each(run_file);
}

fn run_file(file: &String) {
    println!("running for file:{}", file);
    let contents = fs::read_to_string(file)
        .expect("read file");
    //println!("contents: {contents}");
    //let parsed_result = parse_file(&contents);

    match parse_file(&contents) {
        Ok((_, a)) => {
            solve_part_1(&a);
        },
        Err(err) => println!("some sort of error {:?}", err)
    }
    //println!("parsed result {:?}", parsed_result);
    solve_part_2(&contents);
}

type Vec2D<T> = Vec<Vec<T>>;

fn solve_part_2(step_one_parse_result: Vec2D<InputValue>) -> () {
    println!("part2");
    let with_additional_numbers: Vec2D<usize> =
        step_one_parse_result
            .iter()
            .map(make_part2_val_list)
            .collect();
    println!("part2 processed numbers list {:?}", with_additional_numbers);
}

fn make_part2_val_list(inputVals: &Vec<InputValue>) -> Vec<usize> {
}

fn get_nums_for_input_value(val: &InputValue) -> Vec<usize> {
    match val {
        InputValue::Number(val) => {
                let parsed = String::from(*val).parse::<usize>()
                        .expect("was able to parse char into number");
                vec!(parsed)
            },
        InputValue::Letters(parse_me) => letters_to_numbers(&parse_me),
    }
}

fn letters_to_numbers(val: &str) -> Vec<usize> {
}

fn parse_part2(contents: &str) -> IResult<&str, Vec2D<InputValue>> {
    //shamelessly copy and pasted
    //I think I could de-duplicate this code if I knew how to parameterize it on the
    //function in charge of parsing the characters (parse_alphabet), then I could just
    //pass in the part one version or the part 2 version

    //since part1 will parse & save the alphabet strings, I should just be able to
    //rip the numbers out of them as like a post-processing step
    //that's less code duplication than re-doing the parsing code
    let (rem, result) = context("parse_lines", many1(parse_line))(contents)?;

    let (rem, _) = context("end of file", nom::combinator::eof)(rem)?;

    return Ok((rem, result));
}

fn solve_part_1(contents: &Vec<Vec<InputValue>>) -> () {
    let (sums, errs): (Vec<_>, Vec<_>) = contents
        .iter()
        .map(sum_line_part_1)
        .partition(Result::is_ok);
    //println!("sums: {:?}", sums);
    let nums: Vec<usize> = sums.into_iter().map(Result::unwrap).collect();
    let final_sum: usize = nums.iter().sum();
    println!("part 1 final sum: {:?}", final_sum);
    println!("errs: {:?}", errs)
}

//fn sum_line_part_2(contents: &Vec<InputValue>) -> Result<usize, &str> {
//    let values: Vec<usize> = get_nums_and_nums_from_words(contents)
//        .iter()
//        .map(get_nums_and_nums_from_words)?;
//}
//
//fn get_nums_and_nums_from_words(contents: &Vec<InputValue>) -> Vec<usize> {
//    let values = contents
//        .iter()
//        .map(get_nums_from_input)
//        .filter(Option::is_some)
//        .collect();
//    values
//}
//
//fn get_nums_from_input(item: &InputValue) -> Option<Vec<NumParseRes>> {
//    match item {
//        InputValue::Number(val) => Some(vec!(String::from(*val).parse::<usize>())),
//        InputValue::Letters(chars) => {
//            let parse_res = parse_nums_from_words(chars);
//            match parse_res {
//                Ok((_, ok_result)) => Some(ok_result),
//                Err(err) => { println!("{}", err); None }
//            }
//        },
//    }
//}

type NumParseRes = Result<usize, std::num::ParseIntError>;

fn parse_nums_from_words(input: &str) -> IResult<&str, Vec<NumParseRes>> {
    let (rem, number_words) = nom::multi::many0(
        parse_num
    )(input)?;

    let parsed_nums = number_words
        .iter()
        .map(|w| w.parse::<usize>())
        .collect();

    Ok((rem, parsed_nums))
}

fn parse_num(input: &str) -> IResult<&str, &str> {
    //I might need a general "match alphabet characters" in here too
    //to skip over the gaps in between the number words, or something like that
    nom::branch::alt((
        tag("one"),
        tag("two"),
        tag("three"),
        tag("four"),
        tag("five"),
        tag("six"),
        tag("seven"),
        tag("eight"),
        tag("nine"),
        tag("zero"),
    ))(input)
}

fn sum_line_part_1(contents: &Vec<InputValue>) -> Result<usize, &str> {
    // this would be better as a reduce that returns the digits themselves and not
    // the enum type
    let digits: Vec<&InputValue> = contents
        .iter()
        .filter(|item| match item { InputValue::Letters(_) => false, InputValue::Number(numChar) => true })
        .collect()
        ;
    if digits.len() < 1 {
        Err("input line did not have at least one digit")
    } else {
        let digit_one = match digits[0] { InputValue::Number(val) => val, _ => panic!("failed match 1") };
        let digit_two = match digits[digits.len() - 1] { InputValue::Number(val) => val, _ => panic!("failed match 2")};
        //let mut digits_together = String::from("");
        let digits_together = format!("{}{}", *digit_one, *digit_two);
        match digits_together.parse::<usize>() {
            Ok(num) => Ok(num),
            // TODO how do I rip out the error message and add it to my own error message?
            Err(_) => Err("failed to parse the integer")
        }
    }
}

//fn parse_file(contents: &str) -> IResult<&str, Vec<Vec<(&str, &str)>>, &str> {
fn parse_file(contents: &str) -> IResult<&str, Vec<Vec<InputValue>>> {
    //let (rem, ans) = nom::multi::separated_list1(
    //    context("line ending", nom::character::complete::line_ending),
    //    parse_line
    //)(contents)?;
    let (rem, result) = context("parse_lines", many1(parse_line))(contents)?;

    let (rem, _) = context("end of file", nom::combinator::eof)(rem)?;

    return Ok((rem, result));
}

#[derive(Debug)]
enum InputValue {
    Letters(String),
    Number(char)
}

fn parse_line(input: &str) -> IResult<&str, Vec<InputValue>> {
    let (rem, (res, _)) = context(
        "parse line",
        nom::multi::many_till(
            context("branch", nom::branch::alt((parse_number, parse_alphabet))),
            nom::character::complete::line_ending
        )
    )(input)?;
    Ok((rem, res))
    //let (rem, _) = nom::character::complete::line_ending(rem)?;
    //Ok((rem, line_content))
}

//TODO how do I not bother feeding 'input' through?
//can't I just return the 'impl Fn _ -> ____' function and then apply it at the call site?
fn parse_alphabet(input: &str) -> IResult<&str, InputValue> {
    let (rem, letters) = context("alphabet", nom::character::complete::alpha0)(input)?;
    Ok((rem, InputValue::Letters(letters.to_string())))
}

fn parse_number(input: &str) -> IResult<&str, InputValue> {
    let (rem, digit) = context("single digit", nom::character::complete::one_of("0123456789"))(input)?;
    Ok((rem, InputValue::Number(digit)))
}

/* parse any number of characters followed by one or 0 single-digit characters
 * @returns (alpha numeric characters, number character
 **/
//fn parse_number(input: &str) -> IResult<&str, (&str, &str)> {
//    let (remainder, alpha_num) = context(
//        "parse 0 or more letters",
//        nom::character::complete::alpha0
//    )(input)?;
//
//    let (remainder, number) = context(
//        "parse a digit (technically, this could parse any number of digits, because I don't know how to tell it to match exactly one",
//        digit1
//    )(remainder)?;
//
//    return Ok((remainder, (alpha_num, number)));
//}
