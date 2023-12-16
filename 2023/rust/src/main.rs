mod day_one;
mod day_two;
mod day_three;
mod day_four;
mod day_five;
mod day_six;
mod day_seven;
mod day_eight;
mod day_nine;

fn main() {
	println!("Hello, world!");
    //day_one();
    //day_two();
    //day_three();
    //day_four();
    //day_five();
    //day_six();
    //day_seven();
    //day_eight();
    day_nine();
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

fn day_five() {
    day_five::run([
        "../inputs/day5-short.txt",
        "../inputs/day5.txt"
    ]);
}

fn day_six() {
    day_six::run([
        "../inputs/day6-short.txt",
        "../inputs/day6.txt"
    ]);
}

fn day_seven() {
    day_seven::run([
        "../inputs/day7-short.txt",
        "../inputs/day7.txt",
    ]);
}

fn day_eight() {
    day_eight::run([
        "../inputs/day8-short.txt",
        "../inputs/day8.txt",
    ]);
}

fn day_nine() {
    day_nine::run([
        "../inputs/day9-short.txt",
        "../inputs/day9.txt",
    ]);
}
