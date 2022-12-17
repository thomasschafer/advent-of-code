use std::collections::HashSet;
use std::fs;

fn split_to_vec<'a>(s: &'a str, split_by: &str) -> Vec<&'a str> {
    return s.split(split_by).collect::<Vec<&str>>();
}

struct Position {
    x: i32,
    y: i32,
}

fn parse_sensor_position(row_split: &Vec<&str>) -> Position {
    let sensor_x = row_split[0].split(",").collect::<Vec<&str>>()[0]
        .split("=")
        .collect::<Vec<&str>>()[1]
        .parse::<i32>()
        .unwrap();
    let sensor_y = row_split[0].split(",").collect::<Vec<&str>>()[1]
        .split("=")
        .collect::<Vec<&str>>()[1]
        .parse::<i32>()
        .unwrap();
    return Position {
        x: sensor_x,
        y: sensor_y,
    };
}

fn parse_beacon_position(row_split: &Vec<&str>) -> Position {
    let beacon_x = row_split[1].split(",").collect::<Vec<&str>>()[0]
        .split("=")
        .collect::<Vec<&str>>()[1]
        .parse::<i32>()
        .unwrap();
    let beacon_y = row_split[1].split(",").collect::<Vec<&str>>()[1]
        .split("=")
        .collect::<Vec<&str>>()[1]
        .parse::<i32>()
        .unwrap();
    return Position {
        x: beacon_x,
        y: beacon_y,
    };
}

fn num_positions_that_cannot_contain_beacon(rows: Vec<&str>, row_num: i32) -> i32 {
    let mut x_values_beacon_cannot_be: HashSet<i32> = HashSet::new();
    let mut beacon_locations_in_row: HashSet<i32> = HashSet::new();
    for row in rows {
        let row_split = split_to_vec(row, ": closest beacon ");
        let sensor = parse_sensor_position(&row_split);
        let beacon = parse_beacon_position(&row_split);
        if beacon.y == row_num {
            beacon_locations_in_row.insert(beacon.x);
        }
        let distance = (beacon.x - sensor.x).abs() + (beacon.y - sensor.y).abs();

        if (sensor.y - row_num).abs() <= distance {
            let no_beacons_boundary_offset_x = distance - (row_num - sensor.y).abs();
            assert!(no_beacons_boundary_offset_x >= 0);
            for i in (sensor.x - no_beacons_boundary_offset_x)
                ..=(sensor.x + no_beacons_boundary_offset_x)
            {
                x_values_beacon_cannot_be.insert(i);
            }
        }
    }
    for beacon_x in beacon_locations_in_row {
        x_values_beacon_cannot_be.remove(&beacon_x);
    }
    return x_values_beacon_cannot_be.len() as i32;
}

pub fn part1(file_path: &str, row_num: i32) -> i32 {
    let file_contents = fs::read_to_string(file_path).unwrap();
    let rows = file_contents.split("\n").collect::<Vec<&str>>();
    return num_positions_that_cannot_contain_beacon(rows, row_num);
}
