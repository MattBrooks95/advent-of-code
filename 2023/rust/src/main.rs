mod day_one;
mod day_two;
mod day_three;
mod day_four;

fn main() {
	println!("Hello, world!");
    //day_one();
    //day_two();
    //day_three();
    day_four();
}

fn day_one() {
	day_one::run([
		"../inputs/day1-short.txt".to_string(),
		"../inputs/day1.txt".to_string(),
	]);
}

fn day_two() {
    day_two::run([
        "../inputs/day2-short.txt",
        "../inputs/day2.txt"
    ]);
}

fn day_three() {
    day_three::run([
        "../inputs/day3-short.txt",
        "../inputs/day3.txt"
    ]);
}

fn day_four() {
    day_four::run([
        "../inputs/day4-short.txt",
        "../inputs/day4.txt"
    ]);
}
