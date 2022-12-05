use std::fs::File;
use std::fmt;
use std::env;
use std::io::{BufRead, BufReader, Lines};

struct Elf {
    calories: Vec<usize>,
}

impl fmt::Display for Elf {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Elf calories:{:?}", self.calories)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        print!("ERROR: need to provide the path to the input file");
        return;
    }

    let path = match args.get(1).as_deref() {
        None => panic!(),
        Some(p) => p.clone()
    };

    //let path: Option<String> = args.get(0);

    let file_lines = read_file_as_strings(&path);
    //it looks like the 'reduce' function is in a crate...
    let mut elves = Vec::new();
    let mut elf = Elf { calories: Vec::new() };
    for ln in file_lines.unwrap() {
        let unwrapped = ln.unwrap();
        if unwrapped.is_empty() {
            elves.push(elf);
            elf = Elf { calories: Vec::new() };
        } else {
            elf.calories.push(unwrapped.parse::<usize>().unwrap());
        }
    }

    let mut elfCalories = elves.iter().map(|e| e.calories.iter().sum::<usize>()).collect::<Vec<usize>>();
    elfCalories.sort_by(|a, b| b.cmp(a));

    println!("highest calorie count:{}", &elfCalories[0]);
    let top_three_calorie_counts = &elfCalories[0..3];
    println!(
        "highest three calorie counts:{:?}, sum:{}",
        top_three_calorie_counts,
        top_three_calorie_counts.iter().sum::<usize>()
    );
}

fn read_file_as_strings(path: &String) -> Result<Lines<BufReader<File>>, std::io::Error> {
    let file = File::open(path)?;

    Ok(BufReader::new(file).lines())
}
