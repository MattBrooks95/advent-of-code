use std::fs::File;
use std::env;
use std::io::{BufRead, BufReader};

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

    let fileLines = read_file_as_strings(&path);
    match fileLines {
        Err(e) => panic!("couldn't open file:{}, error:{}", path, e),
        Ok(lines) => {
            for line in lines {
                println!("{}", line);
            }
        }
    }
}

fn read_file_as_strings(path: &String) -> Result<Vec<String>, std::io::Error> {
    let file = match File::open(path) {
        Err(e) => return Err(e),
        Ok(res) => res
    };

    Ok(BufReader::new(file).lines().collect::<Result<_, _>>().unwrap())
}
