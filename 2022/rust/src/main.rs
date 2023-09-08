mod day22;

#[derive(PartialEq)]
struct Foo;

fn main() {
    let a = Foo;
    let b = Foo {};
    println!("{}", a == b);

    // println!(
    //     "Part 1 result (test data): {}",
    //     day22::part1("../data/day_22_test.txt")
    // );
    // println!(
    //     "Part 1 result (real data): {}",
    //     day22::part1("../data/day_22.txt")
    // );
    // println!(
    //     "Part 2 result (test data): {}",
    //     day22::part2("../data/day_22_test.txt")
    // );
    // println!(
    //     "Part 2 result (real data): {}",
    //     day22::part2("../data/day_22.txt")
    // );
}
