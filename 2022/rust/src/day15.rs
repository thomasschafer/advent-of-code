use std::cmp::max;
use std::collections::HashSet;
use std::fs;

fn split_to_vec<'a>(s: &'a str, split_by: &str) -> Vec<&'a str> {
    return s.split(split_by).collect::<Vec<&str>>();
}

#[derive(PartialEq, Clone)]
struct Position {
    x: i64,
    y: i64,
}

struct SensorBoundary {
    sensor: Position,
    distance: i64,
}

impl SensorBoundary {
    fn new(sensor: Position, beacon: Position) -> Self {
        let distance = (sensor.x - beacon.x).abs() + (sensor.y - beacon.y).abs();
        Self { sensor, distance }
    }

    fn within_sensor_range(&self, position: &Position) -> bool {
        return (position.x - self.sensor.x).abs() + (position.y - self.sensor.y).abs()
            <= self.distance;
    }

    fn contains_unseen_points_in_quadrant(&self, quadrant: &Quadrant) -> bool {
        let max_x_dist = max(
            (self.sensor.x - quadrant.top_left.x).abs(),
            (self.sensor.x - quadrant.bottom_right.x).abs(),
        );
        let max_y_dist = max(
            (self.sensor.y - quadrant.top_left.y).abs(),
            (self.sensor.y - quadrant.bottom_right.y).abs(),
        );
        return max_x_dist + max_y_dist > self.distance;
    }
}

fn parse_sensor_position(row_split: &Vec<&str>) -> Position {
    let sensor_x = row_split[0].split(",").collect::<Vec<&str>>()[0]
        .split("=")
        .collect::<Vec<&str>>()[1]
        .parse::<i64>()
        .unwrap();
    let sensor_y = row_split[0].split(",").collect::<Vec<&str>>()[1]
        .split("=")
        .collect::<Vec<&str>>()[1]
        .parse::<i64>()
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
        .parse::<i64>()
        .unwrap();
    let beacon_y = row_split[1].split(",").collect::<Vec<&str>>()[1]
        .split("=")
        .collect::<Vec<&str>>()[1]
        .parse::<i64>()
        .unwrap();
    return Position {
        x: beacon_x,
        y: beacon_y,
    };
}

fn num_positions_that_cannot_contain_beacon(rows: Vec<&str>, row_num: i64) -> i64 {
    let mut x_values_beacon_cannot_be: HashSet<i64> = HashSet::new();
    let mut beacon_locations_in_row: HashSet<i64> = HashSet::new();
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
    return x_values_beacon_cannot_be.len() as i64;
}

#[derive(Debug, Clone)]
struct SolutionNotFoundError;

fn row_to_sensor_boundary(row: &&str) -> SensorBoundary {
    let row_split = split_to_vec(row, ": closest beacon ");
    let sensor = parse_sensor_position(&row_split);
    let beacon = parse_beacon_position(&row_split);
    return SensorBoundary::new(sensor, beacon);
}

#[derive(Clone)]
struct Quadrant {
    top_left: Position,
    bottom_right: Position,
}

fn divide_quadrant(quadrant: &Quadrant) -> [Quadrant; 4] {
    let mid = Position {
        x: (quadrant.top_left.x + quadrant.bottom_right.x) / 2,
        y: (quadrant.top_left.y + quadrant.bottom_right.y) / 2,
    };
    let quadrants = [
        Quadrant {
            top_left: Position {
                x: quadrant.top_left.x,
                y: quadrant.top_left.y,
            },
            bottom_right: Position { x: mid.x, y: mid.y },
        },
        Quadrant {
            top_left: Position {
                x: mid.x + 1,
                y: quadrant.top_left.y,
            },
            bottom_right: Position {
                x: quadrant.bottom_right.x,
                y: mid.y,
            },
        },
        Quadrant {
            top_left: Position {
                x: quadrant.top_left.x,
                y: mid.y + 1,
            },
            bottom_right: Position {
                x: mid.x,
                y: quadrant.bottom_right.y,
            },
        },
        Quadrant {
            top_left: Position {
                x: mid.x + 1,
                y: mid.y + 1,
            },
            bottom_right: Position {
                x: quadrant.bottom_right.x,
                y: quadrant.bottom_right.y,
            },
        },
    ];
    return quadrants;
}

fn position_of_beacon(
    rows: Vec<&str>,
    min_x_and_y: i64,
    max_x_and_y: i64,
) -> Result<Position, SolutionNotFoundError> {
    let sensor_boundaries = rows
        .iter()
        .map(row_to_sensor_boundary)
        .collect::<Vec<SensorBoundary>>();
    let full_quadrant = Quadrant {
        top_left: Position {
            x: min_x_and_y,
            y: min_x_and_y,
        },
        bottom_right: Position {
            x: max_x_and_y,
            y: max_x_and_y,
        },
    };
    let mut quadrant_stack = vec![full_quadrant];

    while let Some(quadrant) = quadrant_stack.pop() {
        if quadrant.top_left == quadrant.bottom_right {
            let possible_solution = Position {
                x: quadrant.top_left.x,
                y: quadrant.top_left.y,
            };
            if sensor_boundaries
                .iter()
                .all(|sensor_boundary| !sensor_boundary.within_sensor_range(&possible_solution))
            {
                return Ok(possible_solution);
            }
        } else {
            let sub_quadrants = divide_quadrant(&quadrant);
            for sub_quadrant in sub_quadrants.iter() {
                if sub_quadrant.top_left.x > sub_quadrant.bottom_right.x
                    || sub_quadrant.top_left.y > sub_quadrant.bottom_right.y
                {
                    continue;
                }
                if sensor_boundaries
                    .iter()
                    .all(|boundary| boundary.contains_unseen_points_in_quadrant(sub_quadrant))
                {
                    quadrant_stack.push(sub_quadrant.clone());
                }
            }
        }
    }
    return Err(SolutionNotFoundError);
}

fn tuning_frequency(position: Position) -> i64 {
    return position.x * 4000000 + position.y;
}

pub fn part1(file_path: &str, row_num: i64) -> i64 {
    let file_contents = fs::read_to_string(file_path).unwrap();
    let rows = file_contents.split("\n").collect::<Vec<&str>>();
    return num_positions_that_cannot_contain_beacon(rows, row_num);
}

pub fn part2(file_path: &str, min_x_and_y: i64, max_x_and_y: i64) -> i64 {
    // Credit for solution idea: https://github.com/dclamage/AOC2022/blob/main/day15/src/main.rs
    let file_contents = fs::read_to_string(file_path).unwrap();
    let rows = file_contents.split("\n").collect::<Vec<&str>>();
    let beacon_position = position_of_beacon(rows, min_x_and_y, max_x_and_y).unwrap();
    return tuning_frequency(beacon_position);
}
