use std::collections::HashSet;
use std::fs;

fn split_to_vec<'a>(s: &'a str, split_by: &str) -> Vec<&'a str> {
    return s.split(split_by).collect::<Vec<&str>>()
}

struct Position {
    x: i32,
    y: i32,
}

fn num_positions_that_cannot_contain_beacon(rows: Vec<&str>, row_num: i32) -> i32 {
    let positions_beacon_cannot_be: HashSet<Position> = HashSet::new();
    for row in rows {
        let row_split = split_to_vec(row, ": closest beacon ");
        let sensor_x = split_to_vec(split_to_vec(row_split[0], ",")[0], "=")[1];
        let sensor_y = split_to_vec(split_to_vec(row_split[0], ",")[1], "=")[1];
        let beacon_x = split_to_vec(split_to_vec(row_split[1], ",")[0], "=")[1];
        let beacon_y = split_to_vec(split_to_vec(row_split[1], ",")[1], "=")[1];
        // Add positions to set based on row num
    }
    return positions_beacon_cannot_be.len() as i32;
}

pub fn part1(file_path: &str, row_num: i32) -> i32 {
    let file_contents = fs::read_to_string(file_path).unwrap();
    let rows = file_contents.split("\n").collect::<Vec<&str>>();
    return num_positions_that_cannot_contain_beacon(rows, row_num);
}
