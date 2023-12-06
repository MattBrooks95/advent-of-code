//X values increase from 0 to W, left to right
#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug)]
struct X(usize);
//Y values increase from 0 to H, top to bottom
#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug)]
struct Y(usize);

//the top left corner of the input is 0,0
#[derive(Eq, PartialEq, Hash, Debug)]
struct Location(X, Y);

//a special character is the character and a location
#[derive(Debug)]
struct SpecialSymbol(char);

struct Value(i32);
struct NumDigits(i32);
//digits are a numeric value and the number of digits in the value
//the # of digits can be used to calculate the location span of the number's digits
struct Digits(Value, NumDigits);

enum BoardEntity {
    Symbol(SpecialSymbol),
    Number(Digits),
}

type Board = Vec<BoardEntity>;

pub fn run(file_paths: [&str; 2]) {
    println!("day three, files:{:?}", file_paths);
    //file_paths.into_iter().for_each(do_file);
    do_file(file_paths[0]);
}

//intermediate parsing state
//getting these x/y indices with Nom looked difficult
//so I'm just going to do nested loops and assign every character a location
#[derive(Debug)]
struct Input(char, X, Y);

type SpecialCharsMap = std::collections::HashMap<Location, SpecialSymbol>;
type DigitHitSet = std::collections::HashSet<Location>;

fn do_file(file_path: &str) -> () {
    println!("file:{}", file_path);
    let contents = std::fs::read_to_string(file_path)
        .expect("read file");
    //will this work on windows? XD
    let lines = contents.split('\n');
    let character_matrix: Vec<Vec<Input>> = lines
        .into_iter()
        .enumerate()
        .map(|(line_index, line_contents)| {
            line_contents
                .chars()
                .enumerate()
                .map(|(char_index, char)| {
                    Input(char, X(char_index), Y(line_index))
                })
                .collect()
        })
        .collect()
        ;
    let special_char_map: SpecialCharsMap = get_special_character_map(&character_matrix);
    //println!("{:?}", character_matrix);
    println!("{:?}", special_char_map);
    let touch_symbol_digits = get_digits_from_symbols(&character_matrix, &special_char_map);
}

fn get_digits_from_symbols(
    char_matrix: &Vec<Vec<Input>>,
    special_char_map: &SpecialCharsMap
) -> DigitHitSet {
    let mut set = std::collections::HashSet::new();
    let touching_digits_start_location = special_char_map
        .into_iter()
        .for_each(|(loc@Location(x, y), _)| {
            let search_locations = get_check_locations(loc);
            let digits: Vec<_> = search_locations
                .into_iter()
                //TODO be careful about the access order here
                //I think when I made the character matrix, y (rows) is the outer array
                //index, and x (cols) is the inner array index
                .map(|symbol_loc@Location(X(x), Y(y))| {
                    let Input(c, _, _) = char_matrix.get(y)?.get(x)?;
                    match char_matrix.get(y)?.get(x) {
                        None => None,
                        Some(Input(c, _, _)) => if c.is_ascii_digit() {
                            Some(find_first_digit(&symbol_loc, char_matrix))
                        } else {
                            None
                        },
                    }
                })
                .filter(Option::is_some)
                .map(Option::unwrap)
                .collect();
        });
    set
}

fn find_first_digit(Location(X(x), Y(y)): &Location, char_matrix: &Vec<Vec<Input>>) -> Location {
    let mut seek_x = x - 1;
    match char_matrix.get(y).get(x) {
    }
}

fn get_check_locations(Location(this_x@X(x), this_y@Y(y)): &Location) -> Vec<Location> {
    let x_prev = X(x - 1);
    let x_next = X(x + 1);
    let this_x_clone = this_x.clone();
    let this_y_clone = this_y.clone();
    //TODO does Rust have list comprehensions?
    vec!(
        Location(x_prev, Y(y - 1)),
        Location(x_prev, this_y_clone),
        Location(x_prev, Y(y + 1)),

        Location(this_x_clone, Y(y - 1)),
        Location(this_x_clone, this_y_clone),
        Location(this_x_clone, Y(y + 1)),

        Location(x_next, Y(y - 1)),
        Location(x_next, this_y_clone),
        Location(x_next, Y(y+1))
    )
}

fn get_special_character_map(char_matrix: &Vec<Vec<Input>>) -> SpecialCharsMap {
    let mut map = std::collections::HashMap::new();
    char_matrix
        .into_iter()
        .flatten()
        //.filter(|Input(c, x, y)| {
        //    !c.is_numeric() && *c != '.'
        //})
        .for_each(|Input(c, x, y)| {
            if !c.is_numeric() && *c != '.' {
                map.insert(Location(x.clone(), y.clone()), SpecialSymbol(*c));
            }
        });
    map
}

//fn parse_char(input: char) {
//    match input {
//         => 0,
//    }
//}
