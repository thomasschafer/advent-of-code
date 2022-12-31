use regex::{Captures, Regex};
use std::{fmt, fs, str::FromStr};

#[derive(Debug)]
struct Cost {
    ore: u32,
    clay: u32,
    obsidian: u32,
}

#[derive(Debug)]
struct Blueprint {
    id: u64,
    ore_robot_cost: Cost,
    clay_robot_cost: Cost,
    obsidian_robot_cost: Cost,
    geode_robot_cost: Cost,
}

fn capture_get<T>(capture: &Captures, key: &str) -> T
where
    T: FromStr,
    <T as FromStr>::Err: fmt::Debug,
{
    capture.name(key).unwrap().as_str().parse::<T>().unwrap()
}

fn parse_blueprint_from_line(line: &str) -> Blueprint {
    let input_re = Regex::new(
        r"Blueprint (?P<blueprint_id>\d*): Each ore robot costs (?P<ore_robot_cost_ore>\d*) ore. Each clay robot costs (?P<clay_robot_cost_ore>\d*) ore. Each obsidian robot costs (?P<obsidian_robot_cost_ore>\d*) ore and (?P<obsidian_robot_cost_clay>\d*) clay. Each geode robot costs (?P<geode_robot_cost_ore>\d*) ore and (?P<geode_robot_cost_obsidian>\d*) obsidian.",
    )
    .unwrap();
    let capture = input_re.captures(line).unwrap();

    Blueprint {
        id: capture_get::<u64>(&capture, "blueprint_id"),
        ore_robot_cost: Cost {
            ore: capture_get::<u32>(&capture, "ore_robot_cost_ore"),
            clay: 0,
            obsidian: 0,
        },
        clay_robot_cost: Cost {
            ore: capture_get::<u32>(&capture, "clay_robot_cost_ore"),
            clay: 0,
            obsidian: 0,
        },
        obsidian_robot_cost: Cost {
            ore: capture_get::<u32>(&capture, "obsidian_robot_cost_ore"),
            clay: capture_get::<u32>(&capture, "obsidian_robot_cost_clay"),
            obsidian: 0,
        },
        geode_robot_cost: Cost {
            ore: capture_get::<u32>(&capture, "geode_robot_cost_ore"),
            clay: 0,
            obsidian: capture_get::<u32>(&capture, "geode_robot_cost_obsidian"),
        },
    }
}

fn parse_blueprints(file_path: &str) -> Vec<Blueprint> {
    let file_contents = fs::read_to_string(file_path)
        .unwrap()
        .replace("\n", "")
        .replace("Blueprint", "\nBlueprint")
        .replace("  ", " ");
    let lines = file_contents[1..].split("\n").collect::<Vec<&str>>();
    lines
        .iter()
        .map(|line| parse_blueprint_from_line(line))
        .collect::<Vec<Blueprint>>()
}

fn largest_num_geodes_opened(blueprint: &Blueprint, mins: u32) -> u64 {
    1
}

fn sum_of_quality_levels_of_blueprints(blueprints: Vec<Blueprint>, mins: u32) -> u64 {
    blueprints
        .iter()
        .map(|blueprint| blueprint.id * largest_num_geodes_opened(blueprint, mins))
        .sum()
}

pub fn part1(file_path: &str, mins: u32) -> u64 {
    let blueprints = parse_blueprints(file_path);
    sum_of_quality_levels_of_blueprints(blueprints, mins)
}
