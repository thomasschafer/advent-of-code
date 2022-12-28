use std::{collections::HashMap, fs};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Position {
    x: i32,
    y: i32,
    z: i32,
}

fn next_positions(position: &Position) -> [Position; 6] {
    [
        Position {
            x: position.x + 1,
            y: position.y,
            z: position.z,
        },
        Position {
            x: position.x - 1,
            y: position.y,
            z: position.z,
        },
        Position {
            x: position.x,
            y: position.y + 1,
            z: position.z,
        },
        Position {
            x: position.x,
            y: position.y - 1,
            z: position.z,
        },
        Position {
            x: position.x,
            y: position.y,
            z: position.z + 1,
        },
        Position {
            x: position.x,
            y: position.y,
            z: position.z - 1,
        },
    ]
}

fn surface_area_recursive(
    current_position: &Position,
    positions_map: &mut HashMap<Position, bool>,
) -> u64 {
    positions_map.insert(*current_position, true);
    let mut surface_area_result = 0;
    for next_pos in next_positions(current_position) {
        // println!("--- next_pos = {:?}", next_pos);
        if let Some(visited) = positions_map.get(&next_pos) {
            if !*visited {
                surface_area_result += surface_area_recursive(&next_pos, positions_map);
            }
        } else {
            surface_area_result += 1;
        }
    }
    surface_area_result
}

fn surface_area(positions: Vec<Position>) -> u64 {
    let mut positions_map: HashMap<Position, bool> = HashMap::new();
    let mut total_surface_area = 0;
    for position in &positions {
        positions_map.insert(*position, false);
    }
    for position in &positions {
        if !*positions_map.get(&position).unwrap_or(&true) {
            total_surface_area += surface_area_recursive(position, &mut positions_map);
        }
    }
    total_surface_area
}

pub fn part1(file_path: &str) -> u64 {
    let file_contents = fs::read_to_string(file_path).unwrap();
    let positions = file_contents
        .split("\n")
        .map(|row| {
            row.split(",")
                .map(|val| val.parse::<i32>().unwrap())
                .collect::<Vec<i32>>()
        })
        .map(|position| Position {
            x: position[0],
            y: position[1],
            z: position[2],
        })
        .collect::<Vec<Position>>();

    surface_area(positions)
}

pub fn part2(file_path: &str) -> u64 {
    1
}
