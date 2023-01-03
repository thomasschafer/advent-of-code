use regex::{Captures, Regex};
use std::{cmp, collections::HashMap, fmt, fs, str::FromStr, time::SystemTime};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct Materials {
    ore: u64,
    clay: u64,
    obsidian: u64,
}

#[derive(Clone, Debug)]
struct Blueprint {
    id: u64,
    ore_robot_cost: Materials,
    clay_robot_cost: Materials,
    obsidian_robot_cost: Materials,
    geode_robot_cost: Materials,
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
        ore_robot_cost: Materials {
            ore: capture_get::<u64>(&capture, "ore_robot_cost_ore"),
            clay: 0,
            obsidian: 0,
        },
        clay_robot_cost: Materials {
            ore: capture_get::<u64>(&capture, "clay_robot_cost_ore"),
            clay: 0,
            obsidian: 0,
        },
        obsidian_robot_cost: Materials {
            ore: capture_get::<u64>(&capture, "obsidian_robot_cost_ore"),
            clay: capture_get::<u64>(&capture, "obsidian_robot_cost_clay"),
            obsidian: 0,
        },
        geode_robot_cost: Materials {
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

fn can_afford(resources: &Materials, robot_cost: &Materials) -> bool {
    resources.ore >= robot_cost.ore
        && resources.clay >= robot_cost.clay
        && resources.obsidian >= robot_cost.obsidian
}

#[derive(Debug, Eq, Hash, PartialEq)]
struct MiningAssets {
    mins: u64,
    resources: Materials,
    robots: Materials,
}

fn largest_num_geodes_opened_recursive(
    blueprint: &Blueprint,
    mins: u64,
    resources: &Materials,
    robots: &Materials,
    max_robots: &Materials,
    result_cache: &mut HashMap<MiningAssets, u64>,
) -> u64 {
    if mins == 0 {
        return 0;
    }
    let cache_key = MiningAssets {
        mins,
        resources: resources.clone(),
        robots: robots.clone(),
    };

    if let Some(result) = result_cache.get(&cache_key) {
        return *result;
    }

    let mut result = largest_num_geodes_opened_recursive(
        blueprint,
        mins - 1,
        &Materials {
            ore: resources.ore + robots.ore,
            clay: resources.clay + robots.clay,
            obsidian: resources.obsidian + robots.obsidian,
        },
        robots,
        max_robots,
        result_cache,
    );

    if can_afford(&resources, &blueprint.ore_robot_cost) && robots.ore < max_robots.ore {
        result = cmp::max(
            result,
            largest_num_geodes_opened_recursive(
                blueprint,
                mins - 1,
                &Materials {
                    ore: resources.ore - blueprint.ore_robot_cost.ore + robots.ore,
                    clay: resources.clay - blueprint.ore_robot_cost.clay + robots.clay,
                    obsidian: resources.obsidian - blueprint.ore_robot_cost.obsidian
                        + robots.obsidian,
                },
                &Materials {
                    ore: robots.ore + 1,
                    ..*robots
                },
                max_robots,
                result_cache,
            ),
        );
    }
    if can_afford(&resources, &blueprint.clay_robot_cost) && robots.clay < max_robots.clay {
        result = cmp::max(
            result,
            largest_num_geodes_opened_recursive(
                blueprint,
                mins - 1,
                &Materials {
                    ore: resources.ore - blueprint.clay_robot_cost.ore + robots.ore,
                    clay: resources.clay - blueprint.clay_robot_cost.clay + robots.clay,
                    obsidian: resources.obsidian - blueprint.clay_robot_cost.obsidian
                        + robots.obsidian,
                },
                &Materials {
                    clay: robots.clay + 1,
                    ..*robots
                },
                max_robots,
                result_cache,
            ),
        );
    }
    if can_afford(&resources, &blueprint.obsidian_robot_cost)
        && robots.obsidian < max_robots.obsidian
    {
        result = cmp::max(
            result,
            largest_num_geodes_opened_recursive(
                blueprint,
                mins - 1,
                &Materials {
                    ore: resources.ore - blueprint.obsidian_robot_cost.ore + robots.ore,
                    clay: resources.clay - blueprint.obsidian_robot_cost.clay + robots.clay,
                    obsidian: resources.obsidian - blueprint.obsidian_robot_cost.obsidian
                        + robots.obsidian,
                },
                &Materials {
                    obsidian: robots.obsidian + 1,
                    ..*robots
                },
                max_robots,
                result_cache,
            ),
        );
    }
    if can_afford(&resources, &blueprint.geode_robot_cost) {
        result = cmp::max(
            result,
            (mins - 1)
                + largest_num_geodes_opened_recursive(
                    blueprint,
                    mins - 1,
                    &Materials {
                        ore: resources.ore - blueprint.geode_robot_cost.ore + robots.ore,
                        clay: resources.clay - blueprint.geode_robot_cost.clay + robots.clay,
                        obsidian: resources.obsidian - blueprint.geode_robot_cost.obsidian
                            + robots.obsidian,
                    },
                    robots,
                    max_robots,
                    result_cache,
                ),
        );
    }

    result_cache.insert(cache_key, result);
    result
}

fn largest_num_geodes_opened(blueprint: &Blueprint, mins: u64) -> u64 {
    let mut result_cache: HashMap<MiningAssets, u64> = HashMap::new();
    let resources = Materials {
        ore: 0,
        clay: 0,
        obsidian: 0,
    };
    let robots = Materials {
        ore: 1,
        clay: 0,
        obsidian: 0,
    };
    let max_robots = Materials {
        ore: *[
            blueprint.ore_robot_cost.ore,
            blueprint.clay_robot_cost.ore,
            blueprint.obsidian_robot_cost.ore,
            blueprint.geode_robot_cost.ore,
        ]
        .iter()
        .max()
        .unwrap(),
        clay: *[
            blueprint.ore_robot_cost.clay,
            blueprint.clay_robot_cost.clay,
            blueprint.obsidian_robot_cost.clay,
            blueprint.geode_robot_cost.clay,
        ]
        .iter()
        .max()
        .unwrap(),
        obsidian: *[
            blueprint.ore_robot_cost.obsidian,
            blueprint.clay_robot_cost.obsidian,
            blueprint.obsidian_robot_cost.obsidian,
            blueprint.geode_robot_cost.obsidian,
        ]
        .iter()
        .max()
        .unwrap(),
    };
    largest_num_geodes_opened_recursive(
        blueprint,
        mins,
        &resources,
        &robots,
        &max_robots,
        &mut result_cache,
    )
}

fn sum_of_quality_levels_of_blueprints(blueprints: Vec<Blueprint>, mins: u64) -> u64 {
    blueprints
        .iter()
        .map(|blueprint| blueprint.id * largest_num_geodes_opened(blueprint, mins))
        .sum()
}

fn product_of_geodes_opened_by_blueprints(blueprints: Vec<Blueprint>, mins: u64) -> u64 {
    blueprints
        .iter()
        .map(|blueprint| largest_num_geodes_opened(blueprint, mins))
        .product()
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

pub fn part2(file_path: &str, mins: u64, num_blueprints: usize) -> u64 {
    let start_time = SystemTime::now();

    let blueprints = &parse_blueprints(file_path)[..num_blueprints];
    let result = product_of_geodes_opened_by_blueprints(blueprints.to_vec(), mins);

    println!(
        "Time taken: {}s",
        (start_time.elapsed().unwrap().as_millis() as f32) / 1000.0
    );
    result
}
