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
    let (_, parse_result) = nom::combinator::all_consuming(|i| parse(i, false))(&contents)
        .expect("successfully parsed all of the hands");
    println!("parse result: {:?}", parse_result);
    let hand_ranks: Vec<EvaluatedHand> = parse_result
        .iter()
        .map(|hand| {
            EvaluatedHand {
                hand: hand.clone(),
                kind: hand_type_handle_wild(hand)
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
    let part1: u64 = sorted
        .iter()
        .enumerate()
        .map(|(idx, &EvaluatedHand { hand: Hand(_, bid), .. })| -> u64 {
            let rank: u64 = (idx as u64) + 1_u64;
            rank * (bid as u64)
        }).sum();
    println!("part 1 answer: {}", part1);

    println!("### part 2 ###");
    //250688931 too high
    //250387774 too high
    //250116757 no good
    let (_, parsed) = nom::combinator::all_consuming(|i| parse(i, true))(&contents)
        .expect("parsed for part 2");
    let part2_hand_ranks: Vec<EvaluatedHand> = parsed
        .iter()
        .map(|hand| {
            EvaluatedHand {
                hand: hand.clone(),
                kind: hand_type_handle_wild(hand)
            }
        })
        .collect();
    let mut part2_sorted = part2_hand_ranks.clone();
    part2_sorted.sort();
    for (idx, hand) in part2_sorted.iter().enumerate() {
        println!("idx {}, hand:{:?}", idx, hand);
    }
    let part2: u64 = part2_sorted
        .iter()
        .enumerate()
        .map(|(idx, &EvaluatedHand { hand: Hand(_, bid), ..})| -> u64 {
            let rank: u64 = (idx as u64) + 1_u64;
            rank * (bid as u64)
        }).sum();
    println!("part 2 answer: {}", part2);
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

fn hand_type_handle_wild(hand@Hand(h, _): &Hand) -> HandType {
    let (part1_hand_type, char_map) = hand_type(hand);

    //if it's part2, we have some wild jokers
    let wilds = char_map.get(&PuzzleCard::Wild('J'));
    let return_hand = match wilds {
        None => part1_hand_type.clone(),
        //note that there are hands like this that are all jokers
        //JJJJJ 996
        Some(num_jokers) => {
            match num_jokers {
                //five of a kind is the best you can do with 4 or 5 jokers
                5 | 4 => HandType::FiveOfAKind,
                3 => {
                    //three jokers and a pair of any two other cards could be made
                    //into a five of a kind
                    //unless the jokers were the only pair...
                    //three jokers is at least a three of a kind,
                    //and could be a full house already
                    match part1_hand_type {
                        //if we have a three of a kind and three jokers,
                        //then the jokers were the three of a kind because
                        //3jokers + 3 other cards would be greater than 5 cards
                        //3 jokers and any other 2 unique cards could become a four of a kind
                        HandType::ThreeOfAKind => HandType::FourOfAKind,
                        //3J + 1 pair is a full house, could become a four of a kind
                        HandType::FullHouse => HandType::FourOfAKind,
                        _ => part1_hand_type.clone()
                    }
                },
                2 => {
                    match part1_hand_type {
                        //a full house with 2 jokers means that one of the jokers could
                        //become the same type as the triple, making a four of a kind
                        //^ no, that would be a five of a kind
                        HandType::FullHouse => HandType::FiveOfAKind,
                        //2J and 2 others could become a four of a kind
                        HandType::TwoPair => HandType::FourOfAKind,
                        //if I had a single 2J pair and 3 distinct other cards
                        //I could combine the jokers with one of them to make a three of a kind
                        HandType::OnePair => HandType::ThreeOfAKind,
                        _ => part1_hand_type.clone()
                    }
                },
                1 => {
                    match part1_hand_type {
                        HandType::FourOfAKind => HandType::FiveOfAKind,
                        HandType::ThreeOfAKind => HandType::FourOfAKind,
                        HandType::TwoPair => HandType::FullHouse,
                        HandType::OnePair => HandType::ThreeOfAKind,
                        HandType::HighCard => HandType::OnePair,
                        _ => part1_hand_type.clone()
                    }
                }
                0 => part1_hand_type.clone(),
                _ => panic!("had more than 5 jokers somehow")
            }
        }
    };

    //if return_hand != part1_hand_type {
    //    println!("hand: {:?} promoted type: {:?} to type:{:?}", hand, part1_hand_type, return_hand);
    //}
    return_hand
}

type CharCountMap = HashMap<PuzzleCard, u32>;
fn hand_type(Hand(h, _): &Hand) -> (HandType, CharCountMap) {
    let mut char_map: CharCountMap = HashMap::new();
    for c in h {
        match char_map.get(c) {
            None => {
                char_map.insert(c.clone(), 1);
            },
            Some(prev_value) => {
                char_map.insert(c.clone(), prev_value + 1);
            },
        }
    }
    //five of a kind
    let hand_type = if char_map.values().find(|v| **v == 5).is_some() {
        HandType::FiveOfAKind

    //four of a kind
    } else if char_map.values().find(|v| **v == 4).is_some() {
        HandType::FourOfAKind

    //full house
    } else if char_map.values().find(|v| **v == 3).is_some()
        && char_map.values().find(|v| **v == 2).is_some() {
        HandType::FullHouse

    //three of a kind
    } else if char_map.values().find(|v| **v == 3).is_some() {
        HandType::ThreeOfAKind

    //handle three weakest hands 
    } else {
        handle_weak_cards(&char_map)
    };

    (hand_type, char_map)
}

fn handle_weak_cards(char_map: &HashMap<PuzzleCard, u32>) -> HandType {
    //two pair
    let two_matches: Vec<&u32> = char_map.values().filter(|v| **v == 2).collect();
    if two_matches.len() == 2 {
        return HandType::TwoPair
    }

    //one pair
    if two_matches.len() == 1 {
        return HandType::OnePair
    }

    //all characters are different, weakest hand
    HandType::HighCard
}

fn card_value(c: &PuzzleCard) -> Option<u32> {
    match c {
        PuzzleCard::Normal('A') => Some(14),
        PuzzleCard::Normal('K') => Some(13),
        PuzzleCard::Normal('Q') => Some(12),
        //in part 1, a normal joker is of a high value
        PuzzleCard::Normal('J') => Some(11),
        //in a normal card-to-card comparison, in part 2,
        //the joker is the weakest
        PuzzleCard::Wild('J') => Some(1),
        PuzzleCard::Normal('T') => Some(10),
        PuzzleCard::Normal('9') => Some(9),
        PuzzleCard::Normal('8') => Some(8),
        PuzzleCard::Normal('7') => Some(7),
        PuzzleCard::Normal('6') => Some(6),
        PuzzleCard::Normal('5') => Some(5),
        PuzzleCard::Normal('4') => Some(4),
        PuzzleCard::Normal('3') => Some(3),
        PuzzleCard::Normal('2') => Some(2),
        _ => None,
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
enum PuzzleCard {
    Normal(char),
    Wild(char),
}

/** (cards, bid) */
#[derive(Clone, Debug)]
struct Hand(Vec<PuzzleCard>, u32);

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Hand {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let zipped: Vec<(&PuzzleCard, &PuzzleCard)> = self.0
            .iter()
            .zip(other.0.iter())
            .collect();
        for (my_c, their_c) in zipped {
            if my_c != their_c {
                let ord_res = card_value(my_c).cmp(&card_value(their_c));

                println!("compared {:?} to {:?} with result {:?}",
                     my_c,
                     their_c,
                     ord_res
                );


                return ord_res;
            }
        }
        std::cmp::Ordering::Equal
    }
}

impl Eq for Hand {
}

impl PartialEq for Hand {
    fn eq(&self, other: &Self) -> bool {
        let zipped: Vec<(&PuzzleCard, &PuzzleCard)> = self.0
            .iter()
            .zip(other.0.iter())
            .collect();
        for (my_c, their_c) in zipped {
            if my_c != their_c { return false }
        }
        true
    }
}

fn parse(i: &str, is_part_2: bool) -> IResult<&str, Vec<Hand>> {
    let (rem, hands) = nom::multi::separated_list1(
        newline,
        |i| parse_hand(i, is_part_2)
    )(i)?;

    let (rem, _) = nom::sequence::tuple((newline, nom::combinator::eof))(rem)?;
    Ok((rem, hands))
}

fn parse_hand(i: &str, is_part_2: bool) -> IResult<&str, Hand> {
    let (rem, hand) = nom::multi::count(
        |i| parse_card(i, is_part_2),
        5
    )(i)?;

    let (rem, _) = nom::character::complete::multispace1(rem)?;

    let (rem, wager) = nom::character::complete::u32(rem)?;

    Ok((rem, Hand(hand, wager)))
}

fn parse_card(i: &str, is_part_2: bool) -> IResult<&str, PuzzleCard> {
    let (rem, c) = nom::branch::alt((
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
    ))(i)?;

    let card = match c {
        'J' => {
            if is_part_2 {
                PuzzleCard::Wild(c)
            } else {
                PuzzleCard::Normal(c)
            }
        },
        _ => PuzzleCard::Normal(c),
    };

    Ok((rem, card))
}
