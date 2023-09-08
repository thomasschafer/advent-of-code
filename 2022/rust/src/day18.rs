use f64;
use std::{
    collections::{HashMap, HashSet},
    fs,
};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Position {
    x: i32,
    y: i32,
    z: i32,
}

fn parse_positions(file_path: &str) -> Vec<Position> {
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
    positions
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

#[derive(Debug)]
struct CubeBoundary {
    min_x: f64,
    max_x: f64,
    min_y: f64,
    max_y: f64,
    min_z: f64,
    max_z: f64,
}

impl CubeBoundary {
    fn new() -> CubeBoundary {
        CubeBoundary {
            min_x: f64::INFINITY,
            max_x: f64::NEG_INFINITY,
            min_y: f64::INFINITY,
            max_y: f64::NEG_INFINITY,
            min_z: f64::INFINITY,
            max_z: f64::NEG_INFINITY,
        }
    }

    fn update(&mut self, position: Position) {
        if ((position.x - 1) as f64) < self.min_x {
            self.min_x = (position.x - 1) as f64;
        }
        if (position.x + 1) as f64 > self.max_x {
            self.max_x = (position.x + 1) as f64;
        }
        if ((position.y - 1) as f64) < self.min_y {
            self.min_y = (position.y - 1) as f64;
        }
        if (position.y + 1) as f64 > self.max_y {
            self.max_y = (position.y + 1) as f64;
        }
        if ((position.z - 1) as f64) < self.min_z {
            self.min_z = (position.z - 1) as f64;
        }
        if (position.z + 1) as f64 > self.max_z {
            self.max_z = (position.z + 1) as f64;
        }
    }

    fn contains(&self, position: Position) -> bool {
        self.min_x <= position.x as f64
            && self.max_x >= position.x as f64
            && self.min_y <= position.y as f64
            && self.max_y >= position.y as f64
            && self.min_z <= position.z as f64
            && self.max_z >= position.z as f64
    }
}

fn external_surface_area_recursive(
    current_position: Position,
    droplet_positions: &HashSet<Position>,
    visited: &mut HashSet<Position>,
    boundary: &CubeBoundary,
) -> u64 {
    visited.insert(current_position);
    let mut surface_area_result = 0;
    for next_pos in next_positions(&current_position) {
        if visited.contains(&next_pos) || !boundary.contains(next_pos) {
            continue;
        }
        if droplet_positions.contains(&next_pos) {
            surface_area_result += 1;
        } else {
            surface_area_result +=
                external_surface_area_recursive(next_pos, droplet_positions, visited, boundary);
        }
    }
    surface_area_result
}

fn external_surface_area(positions: Vec<Position>) -> u64 {
    let mut droplet_positions: HashSet<Position> = HashSet::new();
    let mut boundary = CubeBoundary::new();
    for position in &positions {
        droplet_positions.insert(*position);
        boundary.update(*position);
    }
    let mut visited: HashSet<Position> = HashSet::new();
    external_surface_area_recursive(
        Position {
            x: boundary.min_x as i32,
            y: boundary.min_y as i32,
            z: boundary.min_z as i32,
        },
        &droplet_positions,
        &mut visited,
        &boundary,
    )
}

pub fn part1(file_path: &str) -> u64 {
    let positions = parse_positions(file_path);
    surface_area(positions)
}

pub fn part2(file_path: &str) -> u64 {
    let positions = parse_positions(file_path);
    external_surface_area(positions)
}
