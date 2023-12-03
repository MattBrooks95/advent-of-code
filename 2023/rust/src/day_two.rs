pub fn run(file_paths: [&str; 2]) {
    println!("day 2");
    file_paths
        .into_iter()
        .for_each(do_file)
        ;
}

enum Count {
    RedCount(usize),
    BlueCount(usize),
    GreenCount(usize),
}

const RED_LIMIT: usize = 12;
const GREEN_LIMIT: usize = 13;
const BLUE_LIMIT: usize = 14;

/**
 * TRUE -> Possible
 * FALSE -> Impossible
 **/
fn check_limit(count: &Count) -> bool {
    let (limit, val) = match count {
        Count::RedCount(val) => (RED_LIMIT, val),
        Count::GreenCount(val) => (GREEN_LIMIT, val),
        Count::BlueCount(val) => (BLUE_LIMIT, val),
    };

    *val <= limit
}

struct Hint {
    red: Count,
    blue: Count,
    green: Count,
}

struct Game {
    id: usize,
    hints: Vec<Hint>,
}

fn do_file(file_path: &str) {
    println!("file:{}", file_path);
}
