use std::fs;

use lazy_static::lazy_static;
use nom::character::complete::digit1;
use nom::multi::many1;
use nom::error::context;
use nom::IResult;
use nom::bytes::complete::tag;

pub fn run(files: [String; 2]) {
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
            solve_part_2(&a);
        },
        Err(err) => println!("some sort of error {:?}", err)
    }
    //println!("parsed result {:?}", parsed_result);
}

type Vec2D<T> = Vec<Vec<T>>;

fn solve_part_2(step_one_parse_result: &Vec2D<InputValue>) -> () {
    println!("part2");
    let with_additional_numbers: Vec2D<usize> =
        step_one_parse_result
            .iter()
            .map(|item| {
                //println!("#########################");
                let x = make_part2_val_list(item);
                //println!("#########################");
                x
            })
            .collect();
    //println!("part2 processed numbers list {:?}", with_additional_numbers);
    let (valid_sums, errs): (Vec<_>, Vec<_>) = with_additional_numbers
        .iter()
        .map(|v| -> Option<usize> {
            let first = v.first()?;
            let last =  v.last()?;
            Some(first + last)
        })
        .partition(Option::is_some)
        ;
    if !errs.is_empty() {
        println!("had an error in the summation for part 2");
    }
    let unwrapped_sums: Vec<usize> = valid_sums.into_iter().map(Option::unwrap).collect();
    //10006 too low
    let final_answer: usize = unwrapped_sums.iter().sum();
    println!("part 2 final answer: {}", final_answer);
}

fn make_part2_val_list(input_vals: &Vec<InputValue>) -> Vec<usize> {
    //let with_new_vals: Vec2D<usize> = inputVals
    let with_new_vals: Vec<usize> = input_vals
        .iter()
        //unnecessary lambda, but I did this to check the argument type
        .flat_map(|s| get_nums_for_input_value(s))
        .collect();
    //concat!(with_new_vals)
    with_new_vals
}

fn get_nums_for_input_value(val: &InputValue) -> Vec<usize> {
    match val {
        InputValue::Number(val) => {
                let parsed = String::from(*val).parse::<usize>()
                        .expect("was able to parse char into number");
                vec!(parsed)
            },
        InputValue::Letters(parse_me) => {
            let (_, ans) = parse_nums_from_words(&parse_me)
                    .expect("expected to be able to pull values out of a string");
            //TODO filter out the empty arrays that occur when the regex doesn't match?
            ans
        },
    }
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

type NumParseRes = Result<usize, std::num::ParseIntError>;

fn parse_nums_from_words(input: &str) -> IResult<&str, Vec<usize>> {
    //let (rem, number_words) = nom::multi::fold_many0(
    //    take_next_number_word,
    //    Vec::new,
    //    |mut acc: Vec<_>, item| {
    //        acc.push(item);
    //        acc
    //    }
    //)(input)?;
    //take_next_number_word
    let (rem, number_words) = get_number_words(input);
    println!("number words: {:?}", number_words);
    Ok((rem, number_words))

    //let (failed, parsed_nums): (Vec<_>, Vec<_>) = number_words
    //    .into_iter()
    //    .map(number_letters_to_val)
    //    .partition(Option::is_some);
    //if !failed.is_empty() {
    //    println!("warning, parse_nums_from_words had {} failure cases", failed.len());
    //}

    //Ok((rem, parsed_nums.into_iter().map(Option::unwrap).collect()))
}

fn get_number_words(input: &str) -> (&str, Vec<usize>) {
    //need to use this macro to declare a regular expression
    //it gets compiled when it gets used, and being static means
    //that it should only be compiled once?
    lazy_static! {
        static ref NUMBER_REGEX: regex::Regex = regex::Regex::new(r"(one|two|three|four|five|six|seven|eight|nine|zero)").unwrap();
    }

    //yeah this ain't gonna work
    //let (remainder, (skipped, potential_word)) = nom::multi::many_till(
    //    nom::character::complete::alpha1,
    //    parse_num
    //)(input)?;
    //println!("skipped input:{:?}", skipped);
    //try regular expression
    //or find a way to say alt(number_word, singleAlphabetCharactor) and have it run until it
    //succeeds, which is probably slow
    //let (remainder, _) = nom::
    let (matches, errs): (Vec<Option<usize>>, Vec<_>) = NUMBER_REGEX
        .find_iter(input)
        .map(|m| number_letters_to_val(m.as_str()))
        .partition(Option::is_some)
        ;
    if !errs.is_empty() {
        println!("take_next_number_word had errors");
    }

    ("", matches.into_iter().map(Option::unwrap).collect())
}

fn number_letters_to_val(input: &str) -> Option<usize> {
    match input {
        "one" => Some(1),
        "two" => Some(2),
        "three" => Some(3),
        "four" => Some(4),
        "five" => Some(5),
        "six" => Some(6),
        "seven" => Some(7),
        "eight" => Some(8),
        "nine" => Some(9),
        "zero" => Some(0),
        _ => None
    }
}

fn parse_num(input: &str) -> IResult<&str, &str> {
    println!("parse_num called {}", input);
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
