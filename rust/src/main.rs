mod day17;

fn main() {
    println!("Part 1 result (test data): {}", day17::part1("../data/day_17_test.txt", 2022, false));
    println!("Part 1 result (real data): {}", day17::part1("../data/day_17.txt", 2022, false));
    println!("Part 2 result (test data): {}", day17::part2("../data/day_17_test.txt", 1e12 as u64, false));
    println!("Part 2 result (real data): {}", day17::part2("../data/day_17.txt", 1e12 as u64, false));
}
