use nom::IResult;
use nom::bytes::complete::tag;
use nom::character::complete::newline;
use nom::character::complete::u32;
use nom::character::complete::multispace1;
use nom::multi::separated_list1;
use nom::sequence::tuple;
use std::collections::HashMap;

//part1 - 227653707 first try every time
pub fn run(file_paths: [&str; 2]) {
    file_paths
        .into_iter()
        .for_each(do_file)
        ;
    //do_file(file_paths[0]);
}

fn do_file(file_path: &str) -> () {
    println!("file path:{:?}", file_path);
    let _contents = std::fs::read_to_string(file_path);
    assert!(Result::is_ok(&_contents), "was able to read file at {}", file_path);
    let contents = _contents.unwrap();
    let (_, (seeds, pre_map)) = nom::combinator::all_consuming(parse)(contents.as_str())
        .expect("parsed");
    //println!("seeds {:?}", seeds);
    let map: Map = build_map(pre_map)
        .expect("built ranges struct");
    //println!("map {:?}", map);
    let solved_seeds: Vec<SeedInfo> = seeds
        .iter()
        .map(|s| solve_seed(*s, &map))
        .collect();
    //println!("\nsolved seeds:{:?}", solved_seeds);
    if solved_seeds.len() <= 0 {
        panic!("no solved seeds to get locations from");
    }
    let lowest_location_seed = solved_seeds
        .iter()
        .map(|SeedInfo(_, SeedMapResult { loc, .. })| *loc)
        .fold(solved_seeds[0].1.loc, |p, c| std::cmp::min(p, c));
    println!("lowest location seed {}", lowest_location_seed);
    let seed_ranges: Vec<SeedRange> = seeds
        .chunks(2)
        .map(|seed_data| {
            if seed_data.len() != 2 {
                panic!("seed range didn't have exactly 2 elements");
            }
            let [start, len] = seed_data else { panic!("couldn't match 2 elements") };
            SeedRange(*start, start + len)
        })
        .collect();
    //println!("seed ranges {:?}", seed_ranges);
    let mut sorted_by_start = seed_ranges.clone();
    sorted_by_start.sort_unstable_by(|a, b| {
            a.0.partial_cmp(&b.0).unwrap()
        });
    //println!("sorted by start {:?}", sorted_by_start);
    let joined_ranges = sorted_by_start
        .iter()
        .fold(Vec::new(), |mut acc, elem| {
            //let try_combine = combine(acc, elem);
            //try_combine.unwrap()
            let last = acc.last();
            match last {
                None => {
                    acc.push(*elem);
                    acc
                },
                Some(l) => match combine(l, elem) {
                    Some(combine_success) => {
                        acc.pop();
                        acc.push(combine_success);
                        acc
                    },
                    None => {
                        acc.push(*elem);
                        acc
                    }
                }
            }
        });
    //joining them hasn't worked out, it doesn't look like they overlap
    //println!("not joined: {} joined: {}", sorted_by_start.len(), joined_ranges.len());
    let num_seeds_to_compute: u32 = seed_ranges
        .iter()
        .map(|SeedRange(s, e)| e - s)
        .sum()
        ;
    //println!("num seeds to compute: {}", num_seeds_to_compute);
    let humidity_locations_to_compute: u32 = map.humid_to_loc.0
        .iter()
        .map(|Range { length, ..}| {
            length
        })
        .sum()
        ;
    //println!("if I worked backwards, I would have to compute at least {} locations", humidity_locations_to_compute);
    let location_ranges: Vec<(u64, u64)> = map.humid_to_loc.0
        .iter()
        .map(|Range {dest_start, length, ..}| {
            println!("dest_start:{} length:{}", dest_start, length);
            let dest64 = u64::from(*dest_start);
            let len64 = u64::from(*length);
            (dest64, dest64 + len64)
        })
        .collect();
    let mut sorted_location_ranges = location_ranges.clone();
    sorted_location_ranges
        .sort_by(|a, b| {
            a.0.partial_cmp(&b.0).unwrap()
        });
    //println!("sorted location ranges:{:?}", sorted_location_ranges);
    let lowest_loc = 0;
    //this was slow as shit but it worked within a 2 minutes or so
    //78775051 - hurray for brute force
    let highest_loc = sorted_location_ranges.last().unwrap().1;
    //for loc_range@(start, end) in sorted_location_ranges {
        //println!("start:{} end:{}", start, end);
        //for loc_num in start..end {
        println!("searching from {} to {}", lowest_loc, highest_loc);
        for loc_num in lowest_loc..highest_loc {
            match solve_location(loc_num, &map) {
                None => continue,
                Some(projected_value) => {
                    for SeedRange(start, end) in seed_ranges.clone() {
                        //println!("proj:{}, sr start:{} sr end:{}", projected_value, start, end);
                        if projected_value >= start.into() && projected_value <= end.into() {
                            println!(
                                "lowest loc number: {} projected_value {}",
                                loc_num,
                                projected_value
                            );
                            return
                        }
                    }
                }
            }
        }
    //}
}

#[derive(Debug)]
struct SeedMapResult {
    soil: u32,
    fert: u32,
    water: u32,
    light: u32,
    temp: u32,
    humid: u32,
    loc: u32,
}

#[derive(Debug)]
struct SeedInfo(u32, SeedMapResult);

fn solve_location(l: u64, m: &Map) -> Option<u64> {
    let humid_num = map_loc_ranges(l, &m.humid_to_loc.0)
        .unwrap_or(l);
    let temp_num = map_loc_ranges(humid_num, &m.temp_to_humid.0)
        .unwrap_or(humid_num);
    let light_num = map_loc_ranges(temp_num, &m.light_to_temp.0)
        .unwrap_or(temp_num);
    let water_num = map_loc_ranges(light_num, &m.water_to_light.0)
        .unwrap_or(light_num);
    let fert_num = map_loc_ranges(water_num, &m.fert_to_water.0)
        .unwrap_or(water_num);
    let soil_num = map_loc_ranges(fert_num, &m.soil_to_fert.0)
        .unwrap_or(fert_num);
    let seed_num = map_loc_ranges(soil_num, &m.seed_to_soil.0);
    seed_num
}

fn solve_seed(s: u32, m: &Map) -> SeedInfo {
    //there's a pattern here where each step modifies the number OR leaves it as-is
    //so it feels like I should be able to chain some methods like I can with the
    //do notation in Haskell
    //but the Map function here will return None instead of the original value if
    //this would be a lot less dangerous than trying to remember to use the
    //correct local variable each time when calling functions and defaulting with
    //unwrap_or. With the way that I'm doing it now, each step is another local variable
    //which is very error prone
    //I thought using Option seemed natural here, with Some being the 'map successful' case
    //and None being the 'mapping failed, use the default case', but maybe I should have just
    //had the map_seed_range(s) function just return the original value if a mapping wasn't found
    //a range lookup fails
    //seed to soil
    let soil_num = map_seed_ranges(s, &m.seed_to_soil.0)
        .unwrap_or(s);
    //soil to fert
    let fert_num = map_seed_ranges(soil_num, &m.soil_to_fert.0)
        .unwrap_or(soil_num);
    //fert to water
    let water_num = map_seed_ranges(fert_num, &m.fert_to_water.0)
        .unwrap_or(fert_num);
    //water to light
    let light_num = map_seed_ranges(water_num, &m.water_to_light.0)
        .unwrap_or(water_num);
    //light to temperature
    let temp_num = map_seed_ranges(light_num, &m.light_to_temp.0)
        .unwrap_or(light_num);
    //temperature-to-humidity
    let humid_num = map_seed_ranges(temp_num, &m.temp_to_humid.0)
        .unwrap_or(temp_num);
    //humidity to location
    let loc_num = map_seed_ranges(humid_num, &m.humid_to_loc.0)
        .unwrap_or(humid_num);
    SeedInfo(s, SeedMapResult {
        soil: soil_num,
        fert: fert_num,
        water: water_num,
        light: light_num,
        temp: temp_num,
        humid: humid_num,
        loc: loc_num,
    })
}

fn build_map(pre_map: Vec<(&str, Vec<Range>)>) -> Option<Map> {
    //I assumed that I would need to use HashMap::from() to get a map
    //from a vec, but apparently you're supposed to use into_iter() and
    //collect
    let ranges_lookup: HashMap<&str, Vec<Range>> =
        pre_map
        .into_iter()
        .collect();
    let seed_to_soil = ranges_lookup.get(SEED_TO_SOIL)?;
    let fert_to_water = ranges_lookup.get(FERTILIZER_TO_WATER)?;
    let soil_to_fert = ranges_lookup.get(SOIL_TO_FERTILIZER)?;
    let water_to_light = ranges_lookup.get(WATER_TO_LIGHT)?;
    let light_to_temp = ranges_lookup.get(LIGHT_TO_TEMPERATURE)?;
    let temp_to_humid = ranges_lookup.get(TEMPERATURE_TO_HUMIDITY)?;
    let humid_to_loc = ranges_lookup.get(HUMIDITY_TO_LOCATION)?;
    Some(Map {
        seed_to_soil: SeedToSoil(seed_to_soil.to_vec()),
        fert_to_water: FertToWater(fert_to_water.to_vec()),
        soil_to_fert: SoilToFert(soil_to_fert.to_vec()),
        water_to_light: WaterToLight(water_to_light.to_vec()),
        light_to_temp: LightToTemp(light_to_temp.to_vec()),
        temp_to_humid: TempToHumid(temp_to_humid.to_vec()),
        humid_to_loc: HumidToLoc(humid_to_loc.to_vec()),
    })
}

#[derive(Debug)]
struct Map {
    seed_to_soil: SeedToSoil,
    fert_to_water: FertToWater,
    soil_to_fert: SoilToFert,
    water_to_light: WaterToLight,
    light_to_temp: LightToTemp,
    temp_to_humid: TempToHumid,
    humid_to_loc: HumidToLoc,
}

#[derive(Debug)]
struct SeedToSoil(Vec<Range>);
#[derive(Debug)]
struct FertToWater(Vec<Range>);
#[derive(Debug)]
struct SoilToFert(Vec<Range>);
#[derive(Debug)]
struct WaterToLight(Vec<Range>);
#[derive(Debug)]
struct LightToTemp(Vec<Range>);
#[derive(Debug)]
struct TempToHumid(Vec<Range>);
#[derive(Debug)]
struct HumidToLoc(Vec<Range>);

#[derive(Debug, Clone, Copy)]
struct SeedRange(u32, u32);

/** the idea is to join ranges that overlap with eachother
 * to prevent repeating work, and to get an idea of just doing this
 * loop will be possible with the huge seed ranges
 **/
fn combine(SeedRange(x_s, x_e): &SeedRange, SeedRange(y_s, y_e): &SeedRange) -> Option<SeedRange> {
    let start = if x_s >= y_s && x_s <= y_e {
        Some(y_s)
    } else if y_s >= x_s && y_s <= x_e {
        Some(x_s)
    } else { None };

    let end = if x_e >= y_s && x_e <= y_e {
        Some(y_e)
    } else if y_e >= x_s && y_e <= x_e {
        Some(x_e)
    } else { None };

    match (start, end) {
        (Some(s), Some(e)) => Some(SeedRange(*s, *e)),
        _ => None
    }
}

#[derive(Debug, Clone)]
struct Range {
    length: u32,
    source_start: u32,
    dest_start: u32,
}

/** Ok(mapping_result) None(no mapping, use value as-is) */
fn map_seed_ranges(num: u32, ranges: &Vec<Range>) -> Option<u32> {
    for r in ranges {
        match map_seed_range(num, r) {
            None => continue,
            Some(new_val) => return Some(new_val)
        }
    }
    None
}

fn map_loc_ranges(num: u64, ranges: &Vec<Range>) -> Option<u64> {
    for r in ranges {
        match map_loc_range(num, r) {
            None => continue,
            Some(new_val) => return Some(new_val)
        }
    }
    None
}

fn map_loc_range(num: u64, Range {length, source_start, dest_start}: &Range) -> Option<u64> {
    let dest64 = u64::from(*dest_start);
    let len64 = u64::from(*length);
    let source64 = u64::from(*source_start);
    if num >= dest64 && num <= dest64 + len64 {
        let from_dest = num - dest64;
        let source = source64 + from_dest;
        return Some(source)
    }
    None
}

/** Ok(mapping_result) None(no mapping, use value as-is) */
fn map_seed_range(num: u32, Range {length, source_start, dest_start}: &Range) -> Option<u32> {
    if num >= *source_start && num <= *source_start + *length {
        let from_start = num - source_start;
        let dest = dest_start + from_start;
        return Some(dest);
    }
    None
}

const SEEDS: &str = "seeds";
const SEED_TO_SOIL: &str = "seed-to-soil";
const FERTILIZER_TO_WATER: &str = "fertilizer-to-water";
const SOIL_TO_FERTILIZER: &str = "soil-to-fertilizer";
const WATER_TO_LIGHT: &str = "water-to-light";
const LIGHT_TO_TEMPERATURE: &str = "light-to-temperature";
const TEMPERATURE_TO_HUMIDITY: &str = "temperature-to-humidity";
const HUMIDITY_TO_LOCATION: &str = "humidity-to-location";

fn parse(input: &str) -> IResult<&str, (Vec<u32>, Vec<(&str, Vec<Range>)>)> {
    let (rem, seeds) = parse_seeds(input)?;
    let (rem, _) = newline(rem)?;
    let (rem, parsed_sections) = nom::multi::separated_list1(
        tuple((newline, newline)),
        parse_section
        )(rem)?;
    let (rem, _) = tuple((newline, nom::combinator::eof))(rem)?;
    Ok((
        rem,
        (seeds, parsed_sections)
    ))
}

fn parse_seeds(i: &str) -> IResult<&str, Vec<u32>> {
    let (rem, _) = tag(SEEDS)(i)?;
    let (rem, _) = tag(": ")(rem)?;
    let (rem, nums) = nom::multi::separated_list1(
        nom::character::complete::multispace1,
        nom::character::complete::u32
    )(rem)?;
    let (rem, _) = newline(rem)?;
    Ok((rem, nums))
}

fn parse_section(input: &str) -> IResult<&str, (&str, Vec<Range>)> {
    let (rem, map_name) = nom::branch::alt((
        tag(SEED_TO_SOIL),
        tag(FERTILIZER_TO_WATER),
        tag(SOIL_TO_FERTILIZER),
        tag(WATER_TO_LIGHT),
        tag(LIGHT_TO_TEMPERATURE),
        tag(TEMPERATURE_TO_HUMIDITY),
        tag(HUMIDITY_TO_LOCATION),
    ))(input)?;
    let (rem, _) = tag(" map:")(rem)?;
    let (rem, _) = newline(rem)?;
    let (rem, ranges) = separated_list1(
        newline,
        parse_map_range
    )(rem)?;
    Ok((rem, (map_name, ranges)))
}

fn parse_map_range(i: &str) -> IResult<&str, Range> {
    let (rem, dest_start) = u32(i)?;
    //Yay I think I found the '>>' operator ^.^
    let (rem, source_start) = nom::sequence::preceded(multispace1, u32)(rem)?;
    let (rem, range_length) = nom::sequence::preceded(multispace1, u32)(rem)?;
    Ok((rem, Range {
        length: range_length,
        dest_start,
        source_start
    }))
}
