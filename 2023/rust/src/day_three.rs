use nom::IResult;

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

fn do_file(file_path: &str) -> () {
    println!("file:{}", file_path);
    let contents = std::fs::read_to_string(file_path)
        .expect("read file");
    let parse_result = nom::multi::many1(parse_line)(&contents);
}

fn parse_line(input: &str) -> IResult<&str, Board> {
}
