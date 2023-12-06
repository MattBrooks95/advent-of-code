//X values increase from 0 to W, left to right
#[derive(Eq, PartialEq, Hash)]
struct X(usize);
//Y values increase from 0 to H, top to bottom
#[derive(Eq, PartialEq, Hash)]
struct Y(usize);

//the top left corner of the input is 0,0
#[derive(Eq, PartialEq)]
struct Location(X, Y);

//a special character is the character and a location
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
    file_paths.into_iter().for_each(do_file);
}

//intermediate parsing state
//getting these x/y indices with Nom looked difficult
//so I'm just going to do nested loops and assign every character a location
struct Input(char, X, Y);

type SpecialCharsMap = std::collections::HashMap<Location, SpecialSymbol>;

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
}

fn get_special_character_map(char_matrix: &Vec<Vec<Input>>) -> SpecialCharsMap {
    let map = std::collections::HashMap::new();
    char_matrix
        .into_iter()
        .flatten()
        //.filter(|Input(c, x, y)| {
        //    !c.is_numeric() && *c != '.'
        //})
        .for_each(|Input(c, x, y)| {
            if !c.is_numeric() && *c != '.' {
                map.insert((*x, *y), SpecialSymbol(*c));
            }
        });
}

//fn parse_char(input: char) {
//    match input {
//         => 0,
//    }
//}
