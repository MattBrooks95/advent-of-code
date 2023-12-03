mod day_one;
mod day_two;

fn main() {
	println!("Hello, world!");
	//day_one::run([
	//	"../inputs/day1-short.txt".to_string(),
	//	"../inputs/day1.txt".to_string(),
	//]);
    day_two::run([
        "../inputs/day2-short.txt",
        "../inputs/day2.txt"
    ]);
}
