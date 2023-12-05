//X values increase from 0 to W, left to right
struct X(i32);
//Y values increase from 0 to H, top to bottom
struct Y(i32);

//the top left corner of the input is 0,0
type Location = (X, Y);

//a special character is the character and a location
struct SpecialSymbol(char, Location);

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
    file_paths.iter().for_each(do_file);
}

//intermediate parsing state
//getting these x/y indices with Nom looked difficult
//so I'm just going to do nested loops and assign every character a location
struct Input(char, X, Y);

fn do_file(file_path: &str) -> () {
    println!("file:{}", file_path);
    let contents = std::fs::read_to_string(file_path)
        .expect("read file");
    //will this work on windows? XD
    let lines = contents.split('\n');
    let character_matrix: Vec<Vec<_>> = lines
        .into_iter()
        .enumerate()
        .map(|(line_index, line_contents)| {
            line_contents
                .chars()
                .enumerate()
                .map(|(char_index, char)| {
                    (char, char_index, line_index)
                })
                .collect()
        })
        .collect()
        ;
}

//fn parse_char(input: char) {
//    match input {
//         => 0,
//    }
//}
