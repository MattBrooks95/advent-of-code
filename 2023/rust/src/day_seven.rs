use nom::IResult;
use nom::character::complete::newline;
use nom::bytes::complete::tag;
use nom::character::complete::char;
use std::collections::HashMap;

pub fn run(fps: [&str; 2]) {
    fps
        .into_iter()
        .for_each(do_file);
    //do_file(fps[0]);
}

fn do_file(fp: &str) -> () {
    println!("day 7 solving {}", fp);
    let contents = std::fs::read_to_string(fp)
        .expect("successfully read file");
    let (_, parse_result) = nom::combinator::all_consuming(parse)(&contents)
        .expect("successfully parsed all of the hands");
    println!("parse result: {:?}", parse_result);
    let hand_ranks: Vec<EvaluatedHand> = parse_result
        .iter()
        .map(|hand| {
            EvaluatedHand {
                hand: hand.clone(),
                kind: hand_type(hand)
            }
        })
        .collect();
    println!("hand ranks {:?}", hand_ranks);
    let mut sorted = hand_ranks.clone();
    sorted.sort();
    //println!("sorted:");
    //for x in sorted {
    //    println!("\t{:?}", x);
    //}
    let part1: u64 =sorted
        .iter()
        .enumerate()
        .map(|(idx, &EvaluatedHand { hand: Hand(_, bid), .. })| -> u64 {
            let rank: u64 = (idx as u64) + 1_u64;
            rank * (bid as u64)
        }).sum();
    println!("part 1 answer: {}", part1);
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
struct EvaluatedHand {
    kind: HandType,
    hand: Hand,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
enum HandType {
    FiveOfAKind=7,
    FourOfAKind=6,
    FullHouse=5,
    ThreeOfAKind=4,
    TwoPair=3,
    OnePair=2,
    HighCard=1
}

fn hand_type(Hand(h, _): &Hand) -> HandType {
    let mut char_map: HashMap<char, u32> = HashMap::new();
    for c in h {
        match char_map.get(c) {
            None => {
                char_map.insert(*c, 1);
            },
            Some(prev_value) => {
                char_map.insert(*c, prev_value + 1);
            },
        }
    }
    //five of a kind
    if char_map.values().find(|v| **v == 5).is_some() {
        return HandType::FiveOfAKind;
    }

    //four of a kind
    if char_map.values().find(|v| **v == 4).is_some() {
        return HandType::FourOfAKind;
    }

    //full house
    if char_map.values().find(|v| **v == 3).is_some()
        && char_map.values().find(|v| **v == 2).is_some() {
        return HandType::FullHouse;
    }

    //three of a kind
    if char_map.values().find(|v| **v == 3).is_some() {
        return HandType::ThreeOfAKind;
    }

    //two pair
    let two_matches: Vec<&u32> = char_map.values().filter(|v| **v == 2).collect();
    if two_matches.len() == 2 {
        return HandType::TwoPair;
    }

    //one pair
    if two_matches.len() == 1 {
        return HandType::OnePair;
    }

    //all characters are different, weakest hand
    HandType::HighCard
}

fn card_value(c: &char) -> Option<u32> {
    match c {
        'A' => Some(14),
        'K' => Some(13),
        'Q' => Some(12),
        'J' => Some(11),
        'T' => Some(10),
        '9' => Some(9),
        '8' => Some(8),
        '7' => Some(7),
        '6' => Some(6),
        '5' => Some(5),
        '4' => Some(4),
        '3' => Some(3),
        '2' => Some(2),
        _ => None,
    }
}

/** (cards, bid) */
#[derive(Clone, Debug)]
struct Hand(Vec<char>, u32);

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Hand {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let zipped: Vec<(&char, &char)> = self.0
            .iter()
            .zip(other.0.iter())
            .collect();
        for (my_c, their_c) in zipped {
            if my_c != their_c {
                return card_value(my_c).cmp(&card_value(their_c));
            }
        }
        std::cmp::Ordering::Equal
    }
}

impl Eq for Hand {
}

impl PartialEq for Hand {
    fn eq(&self, other: &Self) -> bool {
        let zipped: Vec<(&char, &char)> = self.0
            .iter()
            .zip(other.0.iter())
            .collect();
        for (myC, theirC) in zipped {
            if myC != theirC { return false }
        }
        true
    }
}

fn parse(i: &str) -> IResult<&str, Vec<Hand>> {
    let (rem, hands) = nom::multi::separated_list1(
        newline,
        parse_hand
    )(i)?;

    let (rem, _) = nom::sequence::tuple((newline, nom::combinator::eof))(rem)?;
    Ok((rem, hands))
}

fn parse_hand(i: &str) -> IResult<&str, Hand> {
    let (rem, hand) = nom::multi::count(
        parse_card,
        5
    )(i)?;

    let (rem, _) = nom::character::complete::multispace1(rem)?;

    let (rem, wager) = nom::character::complete::u32(rem)?;

    Ok((rem, Hand(hand, wager)))
}

fn parse_card(i: &str) -> IResult<&str, char> {
    nom::branch::alt((
        char('A'),
        char('K'),
        char('Q'),
        char('J'),
        char('T'),
        char('9'),
        char('8'),
        char('7'),
        char('6'),
        char('5'),
        char('4'),
        char('3'),
        char('2'),
    ))(i)
}
