use std::{collections::HashMap, fs};

fn parse_monkeys(file_path: &str) -> HashMap<String, String> {
    let mut result: HashMap<String, String> = HashMap::new();

    let file_contents = fs::read_to_string(file_path).unwrap();
    file_contents
        .split('\n')
        .map(|line| line.split(": ").collect::<Vec<_>>())
        .for_each(|split_str| {
            result.insert(split_str[0].to_string(), split_str[1].to_string());
        });
    result
}

fn calculate_number(monkey_name: String, monkeys: &HashMap<String, String>) -> i64 {
    let monkey_string = monkeys.get(&monkey_name).unwrap();
    if monkey_string.len() <= 4 {
        return monkey_string.parse::<i64>().unwrap();
    }
    let monkey_1_num = calculate_number(monkey_string[..4].to_string(), monkeys);
    let monkey_2_num = calculate_number(monkey_string[7..].to_string(), monkeys);
    let operator = monkey_string.chars().collect::<Vec<_>>()[5];
    match operator {
        '+' => monkey_1_num + monkey_2_num,
        '-' => monkey_1_num - monkey_2_num,
        '*' => monkey_1_num * monkey_2_num,
        '/' => monkey_1_num / monkey_2_num,
        _ => panic!("Found unexpected operator {}", operator),
    }
}

fn build_equation(monkey_name: String, monkeys: &HashMap<String, String>) -> String {
    if monkey_name == "humn" {
        return "x".to_string();
    }
    let monkey_string = monkeys.get(&monkey_name).unwrap();
    if monkey_string.len() <= 4 {
        return monkey_string.clone();
    }

    let monkey_1_equation = build_equation(monkey_string[..4].to_string(), monkeys);
    let monkey_2_equation = build_equation(monkey_string[7..].to_string(), monkeys);
    let operator = monkey_string.chars().collect::<Vec<_>>()[5];

    if !(monkey_1_equation.contains("x") || monkey_2_equation.contains("x")) {
        let monkey_1_num = monkey_1_equation.parse::<i64>().unwrap();
        let monkey_2_num = monkey_2_equation.parse::<i64>().unwrap();
        let result = match operator {
            '+' => monkey_1_num + monkey_2_num,
            '-' => monkey_1_num - monkey_2_num,
            '*' => monkey_1_num * monkey_2_num,
            '/' => monkey_1_num / monkey_2_num,
            _ => panic!("Found unexpected operator {}", operator),
        };
        result.to_string()
    } else {
        format!("({}{}{})", monkey_1_equation, operator, monkey_2_equation)
    }
}

pub fn part1(file_path: &str) -> i64 {
    let monkeys = parse_monkeys(file_path);
    let start_monkey = "root".to_string();
    calculate_number(start_monkey, &monkeys)
}

pub fn part2(file_path: &str) -> String {
    let monkeys = parse_monkeys(file_path);
    let root_equation = monkeys.get("root").unwrap();
    let lhs = build_equation(root_equation[..4].to_string(), &monkeys);
    let rhs = build_equation(root_equation[7..].to_string(), &monkeys);
    format!("{lhs} = {rhs}")
}
