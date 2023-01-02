use regex::{Captures, Regex};
use std::{cmp, collections::HashMap, fmt, fs, str::FromStr, time::SystemTime};

#[derive(Debug)]
struct Cost {
    ore: u64,
    clay: u64,
    obsidian: u64,
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
            ore: capture_get::<u64>(&capture, "ore_robot_cost_ore"),
            clay: 0,
            obsidian: 0,
        },
        clay_robot_cost: Cost {
            ore: capture_get::<u64>(&capture, "clay_robot_cost_ore"),
            clay: 0,
            obsidian: 0,
        },
        obsidian_robot_cost: Cost {
            ore: capture_get::<u64>(&capture, "obsidian_robot_cost_ore"),
            clay: capture_get::<u64>(&capture, "obsidian_robot_cost_clay"),
            obsidian: 0,
        },
        geode_robot_cost: Cost {
            ore: capture_get::<u64>(&capture, "geode_robot_cost_ore"),
            clay: 0,
            obsidian: capture_get::<u64>(&capture, "geode_robot_cost_obsidian"),
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

#[derive(Debug, Eq, Hash, PartialEq)]
struct MiningAssets {
    mins: u64,
    ore: u64,
    ore_robots: u64,
    clay: u64,
    clay_robots: u64,
    obsidian: u64,
    obsidian_robots: u64,
}

fn largest_num_geodes_opened_recursive(
    blueprint: &Blueprint,
    mins: u64,
    ore: u64,
    ore_robots: u64,
    clay: u64,
    clay_robots: u64,
    obsidian: u64,
    obsidian_robots: u64,
    result_cache: &mut HashMap<MiningAssets, u64>,
) -> u64 {
    if mins == 0 {
        return 0;
    }
    let cache_key = MiningAssets {
        mins,
        ore,
        ore_robots,
        clay,
        clay_robots,
        obsidian,
        obsidian_robots,
    };

    if let Some(result) = result_cache.get(&cache_key) {
        return *result;
    }

    let mut result = largest_num_geodes_opened_recursive(
        blueprint,
        mins - 1,
        ore + ore_robots,
        ore_robots,
        clay + clay_robots,
        clay_robots,
        obsidian + obsidian_robots,
        obsidian_robots,
        result_cache,
    );

    if ore >= blueprint.ore_robot_cost.ore
        && clay >= blueprint.ore_robot_cost.clay
        && obsidian >= blueprint.ore_robot_cost.obsidian
        && ore_robots
            < *[
                blueprint.ore_robot_cost.ore,
                blueprint.clay_robot_cost.ore,
                blueprint.obsidian_robot_cost.ore,
                blueprint.geode_robot_cost.ore,
            ]
            .iter()
            .max()
            .unwrap()
    {
        result = cmp::max(
            result,
            largest_num_geodes_opened_recursive(
                blueprint,
                mins - 1,
                ore - blueprint.ore_robot_cost.ore + ore_robots,
                ore_robots + 1,
                clay - blueprint.ore_robot_cost.clay + clay_robots,
                clay_robots,
                obsidian - blueprint.ore_robot_cost.obsidian + obsidian_robots,
                obsidian_robots,
                result_cache,
            ),
        );
    }
    if ore >= blueprint.clay_robot_cost.ore
        && clay >= blueprint.clay_robot_cost.clay
        && obsidian >= blueprint.clay_robot_cost.obsidian
        && clay_robots
            < *[
                blueprint.ore_robot_cost.clay,
                blueprint.clay_robot_cost.clay,
                blueprint.obsidian_robot_cost.clay,
                blueprint.geode_robot_cost.clay,
            ]
            .iter()
            .max()
            .unwrap()
    {
        result = cmp::max(
            result,
            largest_num_geodes_opened_recursive(
                blueprint,
                mins - 1,
                ore - blueprint.clay_robot_cost.ore + ore_robots,
                ore_robots,
                clay - blueprint.clay_robot_cost.clay + clay_robots,
                clay_robots + 1,
                obsidian - blueprint.clay_robot_cost.obsidian + obsidian_robots,
                obsidian_robots,
                result_cache,
            ),
        );
    }
    if ore >= blueprint.obsidian_robot_cost.ore
        && clay >= blueprint.obsidian_robot_cost.clay
        && obsidian >= blueprint.obsidian_robot_cost.obsidian
        && obsidian_robots
            < *[
                blueprint.ore_robot_cost.obsidian,
                blueprint.clay_robot_cost.obsidian,
                blueprint.obsidian_robot_cost.obsidian,
                blueprint.geode_robot_cost.obsidian,
            ]
            .iter()
            .max()
            .unwrap()
    {
        result = cmp::max(
            result,
            largest_num_geodes_opened_recursive(
                blueprint,
                mins - 1,
                ore - blueprint.obsidian_robot_cost.ore + ore_robots,
                ore_robots,
                clay - blueprint.obsidian_robot_cost.clay + clay_robots,
                clay_robots,
                obsidian - blueprint.obsidian_robot_cost.obsidian + obsidian_robots,
                obsidian_robots + 1,
                result_cache,
            ),
        );
    }
    if ore >= blueprint.geode_robot_cost.ore
        && clay >= blueprint.geode_robot_cost.clay
        && obsidian >= blueprint.geode_robot_cost.obsidian
    {
        result = cmp::max(
            result,
            (mins - 1)
                + largest_num_geodes_opened_recursive(
                    blueprint,
                    mins - 1,
                    ore - blueprint.geode_robot_cost.ore + ore_robots,
                    ore_robots,
                    clay - blueprint.geode_robot_cost.clay + clay_robots,
                    clay_robots,
                    obsidian - blueprint.geode_robot_cost.obsidian + obsidian_robots,
                    obsidian_robots,
                    result_cache,
                ),
        );
    }

    result_cache.insert(cache_key, result);
    result
}

fn largest_num_geodes_opened(blueprint: &Blueprint, mins: u64) -> u64 {
    let mut result_cache: HashMap<MiningAssets, u64> = HashMap::new();
    largest_num_geodes_opened_recursive(blueprint, mins, 0, 1, 0, 0, 0, 0, &mut result_cache)
}

fn sum_of_quality_levels_of_blueprints(blueprints: Vec<Blueprint>, mins: u64) -> u64 {
    blueprints
        .iter()
        .map(|blueprint| blueprint.id * largest_num_geodes_opened(blueprint, mins))
        .sum()
}

pub fn part1(file_path: &str, mins: u64) -> u64 {
    let start_time = SystemTime::now();

    let blueprints = parse_blueprints(file_path);
    let result = sum_of_quality_levels_of_blueprints(blueprints, mins);

    println!(
        "Time taken: {}s",
        (start_time.elapsed().unwrap().as_millis() as f32) / 1000.0
    );
    result
}
