use std::collections::{HashMap, HashSet};
use std::time::SystemTime;
use std::{cmp, fs};

#[derive(Debug)]
struct Valve {
    flow_rate: u32,
    leads_to: Vec<String>,
}

fn parse_valve(row: &str) -> (&str, Valve) {
    let valve_name = row.split(" ").collect::<Vec<&str>>()[1];
    let flow_rate: u32 = row.split("flow rate=").collect::<Vec<&str>>()[1]
        .split(";")
        .collect::<Vec<&str>>()[0]
        .parse()
        .unwrap();
    let leads_to = row.split("to v").collect::<Vec<&str>>()[1]
        .split(" ")
        .collect::<Vec<&str>>()[1..]
        .join("")
        .split(",")
        .map(|c| c.to_string())
        .collect::<Vec<String>>();
    let valve = Valve {
        flow_rate,
        leads_to,
    };
    (valve_name, valve)
}

fn parse_valves(rows: Vec<&str>) -> HashMap<&str, Valve> {
    let mut valves: HashMap<&str, Valve> = HashMap::new();
    for row in rows {
        let (valve_name, valve) = parse_valve(row);
        valves.insert(valve_name, valve);
    }
    valves
}

fn cache_key(
    mins_remaining: u32,
    current_valve: &str,
    open_valves: &HashSet<&str>,
    use_elephant: bool,
) -> String {
    let mut valves_vec = Vec::from_iter(open_valves.clone());
    valves_vec.sort();
    let res = [
        mins_remaining.to_string(),
        current_valve.to_string(),
        valves_vec.join("-").to_string(),
        use_elephant.to_string(),
    ]
    .join(",");
    res
}

fn most_pressure_released_recursive(
    valves: &HashMap<&str, Valve>,
    mins_remaining: u32,
    current_valve: &str,
    open_valves: &HashSet<&str>,
    result_cache: &mut HashMap<String, u32>,
    use_elephant: bool,
    original_mins_remaining: u32,
) -> u32 {
    let current_cache_key = cache_key(mins_remaining, current_valve, open_valves, use_elephant);
    if let Some(result) = result_cache.get(&current_cache_key) {
        return *result;
    }

    let mut max_result = if !use_elephant {
        0
    } else {
        most_pressure_released_recursive(
            valves,
            original_mins_remaining,
            "AA",
            open_valves,
            result_cache,
            false,
            original_mins_remaining,
        )
    };

    if mins_remaining == 0 {
        return max_result;
    }

    if !open_valves.contains(current_valve) && valves.get(current_valve).unwrap().flow_rate > 0 {
        let mut new_open_valves = open_valves.clone();
        new_open_valves.insert(current_valve);
        let open_valve_result = valves.get(current_valve).unwrap().flow_rate * (mins_remaining - 1)
            + most_pressure_released_recursive(
                &valves,
                mins_remaining - 1,
                current_valve,
                &new_open_valves,
                result_cache,
                use_elephant,
                original_mins_remaining,
            );
        max_result = cmp::max(max_result, open_valve_result)
    }

    for next_valve in &valves.get(current_valve).unwrap().leads_to {
        let next_valve_result = most_pressure_released_recursive(
            &valves,
            mins_remaining - 1,
            next_valve,
            &open_valves,
            result_cache,
            use_elephant,
            original_mins_remaining,
        );
        max_result = cmp::max(max_result, next_valve_result);
    }
    result_cache.insert(current_cache_key, max_result);
    max_result
}

fn most_pressure_released(
    file_path: &str,
    mins: u32,
    current_valve: &str,
    use_elephant: bool,
) -> u32 {
    let file_contents = fs::read_to_string(file_path).unwrap();
    let rows = file_contents.split("\n").collect::<Vec<&str>>();
    let valves = parse_valves(rows);

    most_pressure_released_recursive(
        &valves,
        mins,
        current_valve,
        &HashSet::new(),
        &mut HashMap::new(),
        use_elephant,
        mins,
    )
}

pub fn part1(file_path: &str, mins_remaining: u32) -> u32 {
    let start_time = SystemTime::now();
    let result = most_pressure_released(file_path, mins_remaining, "AA", false);
    println!(
        "Time taken: {}",
        (start_time.elapsed().unwrap().as_millis() as f32) / 1000.0
    );
    result
}

pub fn part2(file_path: &str, mins_remaining: u32) -> u32 {
    let start_time = SystemTime::now();
    let result = most_pressure_released(file_path, mins_remaining, "AA", true);
    println!(
        "Time taken: {}",
        (start_time.elapsed().unwrap().as_millis() as f32) / 1000.0
    );
    result
}
