use nom::IResult;
use nom::bytes::complete::tag;
use nom::character::complete::multispace1;
use nom::character::complete::newline;
use nom::combinator::all_consuming;
use std::collections::HashSet;
use std::collections::HashMap;

pub fn run(file_paths: [&str; 2]) {
    println!("file paths {:?}", file_paths);
    file_paths
        .into_iter()
        .for_each(do_file);
    //do_file(file_paths[0]);
}

//4259 too low
//Nom wasn't consuming all input because of a mistake in my parser
//that expected exactly one whitespace
//I need to learn how to tell NOM to fail if input is left over
//21568 was correct for day4 part 1
//part 2, 61209 was too low
//it'll be a pain to figure out where it went wrong
fn do_file(file_path: &str) -> () {
    println!("###### doing file {file_path} ######");
    let contents = std::fs::read_to_string(file_path)
        .expect("read file contents as string");
    let (rem, cards) = all_consuming(parse_file)(contents.as_str())
        .expect("parsed file and consumed all input");
    //println!("parse_result {:?}", cards);
    let card_match_counts: Vec<(u32, usize)> = cards
        .iter()
        .map(sum_card)
        .collect();
    //println!("card sums:{:?}, num cards:{:?} remaining input:{:?}", card_match_counts, card_match_counts.len(), rem.len());
    //println!("rem:{:?}", rem);
    let sum: u32 = card_match_counts.iter().map(|x| x.0).sum();
    println!("final answer:{sum}");
    part2(&card_match_counts);
}

fn part2(card_match_counts: &Vec<(u32, usize)>) -> () {
    println!("####### PART 2 #######");

    //card_match_counts.into_iter().enumerate().for_each(|(idx, (_, match_count))| {
    //    println!("card idx:{idx}, match_count:{match_count}");
    //});

    let mut answer_lookup: HashMap<usize, u32> = HashMap::new();
    card_match_counts
        .iter()
        .enumerate()
        .for_each(|(idx, _)| {
            process_original_card(
                &card_match_counts,
                &mut answer_lookup,
                idx,
            );
        })
        ;
    //println!("answer table\n{:?}", answer_lookup);

    //debug printing
    //let mut sorted_answers = answer_lookup
    //    .clone()
    //    .into_iter()
    //    .collect::<Vec<(usize, u32)>>()
    //    ;
    //sorted_answers.sort();
    //sorted_answers
    //    .iter()
    //    .for_each(|(key, val)| {
    //        println!("k:{} v:{}", key, val);
    //    });
    let answer_sum: u32 = card_match_counts
        .iter()
        .enumerate()
        .map(|(idx, _)| {
            match answer_lookup.get(&idx) {
                None => 1,
                Some(v) => *v,
            }
        })
        .sum()
        ;
    println!("answer sum:{}", answer_sum);

    //let copied_card_sum: u32 = copied_card_sums.iter().sum();
    //println!("original card point results:{:?}\n\tcopied card sum:{copied_card_sum}", copied_card_sums);
}

/**
 * fills up the solve table for the value of each card
 **/
fn process_original_card(
    card_match_counts: &Vec<(u32, usize)>,
    answer_lookup: &mut HashMap<usize, u32>,
    card_idx: usize,
    //(_, card_match_count): (u32, usize),
) -> () {
    let num_matched = match card_match_counts.get(card_idx) {
        None => return,
        Some((_, matched_count)) => matched_count,
    };

    match answer_lookup.get(&card_idx) {
        Some(_) => return,
        None => {
            if *num_matched == 0 {
                //solve(card) = 1 if it has no matched numbers
                //because it will be counted itself as one card
                answer_lookup.insert(card_idx, 1);
                //println!("no matches, inserted {} {}", card_idx, 1);
            }

            let sub_card_indices = get_indices_for_matched_number(card_idx, *num_matched);
            sub_card_indices
                .clone()
                .for_each(|sc| process_original_card(
                    card_match_counts,
                    answer_lookup,
                    sc
                ));
            //this means I don't get tail recursion but oh well
            let (successes, failures): (Vec<_>, Vec<_>) = sub_card_indices
                .clone()
                .map(|sc| answer_lookup.get(&sc))
                .partition(Option::is_some)
                ;
            if failures.len() != 0 {
                panic!(
                    "failed to look up the result of a sub problem\n\t{:?}\n\t{:?}",
                    sub_card_indices,
                    answer_lookup
                );
            }

            let sub_card_sum: u32 = successes
                .into_iter()
                .map(Option::unwrap)
                .sum();
            let step_sum = sub_card_sum + 1u32;
            //println!("inserted {} v:{}", card_idx, step_sum);
            answer_lookup.insert(card_idx, step_sum);
        }
    }
}

fn get_indices_for_matched_number(
    this_card_idx: usize,
    num_matched: usize,
) -> std::ops::Range<usize> {
    let start_idx = this_card_idx + 1;
    start_idx..(start_idx + num_matched)
}

fn parse_file(input: &str) -> IResult<&str, Vec<Card>> {
    let (rem, cards) = nom::multi::separated_list1(
        nom::character::complete::newline,
        parse_line
        )(input)?;
    let (rem, _) = newline(rem)?;
    let (rem, _) = nom::combinator::eof(rem)?;
    Ok((rem, cards))
}

/** returns the value of the card as it would have been evaluated in step1
 * and returs the # of matching numbers that led to that sum
 * @returns (part1 value, match count)
 **/
fn sum_card(card: &Card) -> (u32, usize) {
    let winning_numbers: HashSet<&u32> =
        HashSet::from_iter(card.winning_numbers.0.iter());
    let (sum, hit_count): (u32, usize) =
        card.my_numbers.0
            .iter()
            .fold((0, 0), |(sum, match_count), curr| {
                match winning_numbers.get(&curr) {
                    None => (sum, match_count),
                    //we matched a number, increase the counters
                    Some(_) => match match_count {
                        0 | 1 => (sum + 1, match_count + 1),
                        double_me => (sum + 2u32.pow(double_me.checked_sub(1).unwrap().try_into().unwrap()), double_me + 1)
                    },
                }
            })
            ;
    //println!("card {}: (sum:{sum}) (hit_count:{hit_count})", card.id);
    (sum, hit_count)
}

//list of winning numbers are the numbers on the left side
#[derive(Debug)]
struct WinningNumbers(Vec<u32>);
//list of my numbers are the numbers on the right side of the |
#[derive(Debug)]
struct MyNumbers(Vec<u32>);

#[derive(Debug)]
struct Card {
    id: u32,
    my_numbers: MyNumbers,
    winning_numbers: WinningNumbers,
}

fn parse_line(input: &str) -> IResult<&str, Card> {
    //throw out beginning label
    let (rem, _) = tag("Card")(input)?;
    //throw out spaces between beginning label and the card id
    let (rem, _) = multispace1(rem)?;
    let (rem, card_id) = nom::character::complete::u32(rem)?;
    //throw out whitespace before winning numbers list
    let (rem, _) = tag(":")(rem)?;
    let (rem, _) = multispace1(rem)?;
    let (rem, winning_numbers) = parse_number_list(rem)?;
    //throw away separator between number lists
    let (rem, _) = tag(" |")(rem)?;
    //could be more than one space after the |
    let (rem, _) = multispace1(rem)?;
    let (rem, my_numbers) = parse_number_list(rem)?;
    Ok((rem, Card {
        id: card_id,
        my_numbers: MyNumbers(my_numbers),
        winning_numbers: WinningNumbers(winning_numbers)
    }))
}

fn parse_number_list(input: &str) -> IResult<&str, Vec<u32>> {
    nom::multi::separated_list1(
        multispace1,
        nom::character::complete::u32,
        )(input)
}
