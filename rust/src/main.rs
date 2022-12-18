mod day15;

fn main() {
    println!("Part 1 result (test data): {}", day15::part1("../data/day_15_test.txt", 10));
    println!("Part 1 result (real data): {}", day15::part1("../data/day_15.txt", 2000000));
    println!("Part 2 result (test data): {}", day15::part2("../data/day_15_test.txt", 0, 20));
    println!("Part 2 result (real data): {}", day15::part2("../data/day_15.txt", 0, 4000000));
}
