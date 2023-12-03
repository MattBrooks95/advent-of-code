pub fn run(file_paths: [&str; 2]) {
    println!("day 2");
    file_paths
        .into_iter()
        .for_each(do_file)
        ;
}

fn do_file(file_path: &str) {
    println!("file:{}", file_path);
}
