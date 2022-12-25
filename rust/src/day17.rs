use std::{cmp, collections::HashMap, fs, time::SystemTime};

const CHAMBER_WIDTH: u32 = 7;

type Rock = Vec<Vec<bool>>;

type RockTower = HashMap<(u32, u32), bool>;

fn display_rock_tower(rock_tower: &RockTower) {
    let tower_height = cmp::max(6, tower_height(rock_tower));

    for y in (0..=tower_height + 1).rev() {
        print!("|");
        for x in 0..CHAMBER_WIDTH {
            print!(
                "{}",
                if *rock_tower.get(&(x, y)).unwrap_or(&false) {
                    "#"
                } else {
                    "."
                }
            );
        }
        println!("|");
    }
    println!("+{}+\n", "-".repeat(CHAMBER_WIDTH as usize));
}

fn display_rock_tower_with_rock(
    rock_tower: &mut RockTower,
    rock: &Rock,
    bottom_left_corner_position: (u32, u32),
) {
    add_rock_to_tower(rock_tower, rock, bottom_left_corner_position);
    display_rock_tower(rock_tower);
}

fn parse_rocks(rock_file_path: &str) -> Vec<Rock> {
    let file_contents = fs::read_to_string(rock_file_path).unwrap();
    let rock_strings = file_contents.split("\n\n").collect::<Vec<&str>>();
    let mut result: Vec<Rock> = Vec::with_capacity(rock_strings.len());
    for rock_string in rock_strings {
        let rock: Rock = rock_string
            .split("\n")
            .map(|line| line.chars().map(|s| s == '#').collect())
            .collect();
        result.push(rock);
    }
    result
}

fn rock_intersects_tower(
    rock_tower: &RockTower,
    rock: &Rock,
    bottom_left_corner_position: (u32, u32),
) -> bool {
    for (row_idx, row) in rock.iter().enumerate() {
        for (col_idx, rock_block) in row.iter().enumerate() {
            let rock_block_x = bottom_left_corner_position.0 + col_idx as u32;
            let rock_block_y = bottom_left_corner_position.1 + (rock.len() - 1 - row_idx) as u32;
            if *rock_block
                && *rock_tower
                    .get(&(rock_block_x, rock_block_y))
                    .unwrap_or(&false)
            {
                return false;
            }
        }
    }
    true
}

fn add_rock_to_tower(
    rock_tower: &mut RockTower,
    rock: &Rock,
    bottom_left_corner_position: (u32, u32),
) {
    for (row_idx, row) in rock.iter().enumerate() {
        for (col_idx, rock_block) in row.iter().enumerate() {
            let rock_block_x = bottom_left_corner_position.0 + col_idx as u32;
            let rock_block_y = bottom_left_corner_position.1 + (rock.len() - 1 - row_idx) as u32;
            if *rock_block {
                rock_tower.insert((rock_block_x, rock_block_y), true);
            }
        }
    }
}

fn can_move_left(
    rock_tower: &RockTower,
    rock: &Rock,
    bottom_left_corner_position: (u32, u32),
) -> bool {
    bottom_left_corner_position.0 > 0
        && rock_intersects_tower(
            rock_tower,
            rock,
            (
                bottom_left_corner_position.0 - 1,
                bottom_left_corner_position.1,
            ),
        )
}

fn can_move_right(
    rock_tower: &RockTower,
    rock: &Rock,
    bottom_left_corner_position: (u32, u32),
) -> bool {
    bottom_left_corner_position.0 + (rock[0].len() as u32 - 1) < CHAMBER_WIDTH - 1
        && rock_intersects_tower(
            rock_tower,
            rock,
            (
                bottom_left_corner_position.0 + 1,
                bottom_left_corner_position.1,
            ),
        )
}

fn can_move_down(
    rock_tower: &RockTower,
    rock: &Rock,
    bottom_left_corner_position: (u32, u32),
) -> bool {
    bottom_left_corner_position.1 > 0
        && rock_intersects_tower(
            rock_tower,
            rock,
            (
                bottom_left_corner_position.0,
                bottom_left_corner_position.1 - 1,
            ),
        )
}

fn tower_height(rock_tower: &RockTower) -> u32 {
    let mut max_y = 0;
    for ((_, y), _) in rock_tower {
        if *y > max_y {
            max_y = *y;
        }
    }
    max_y + 1
}

fn rock_tower_height(
    jet_patterns: Vec<char>,
    rocks: Vec<Rock>,
    num_rocks: u32,
    display: bool,
) -> u32 {
    let mut rock_tower_height = 0;
    let mut rock_tower: RockTower = HashMap::new();
    if display {
        display_rock_tower(&rock_tower)
    };

    let mut jet_idx = 0;
    for i in 0..num_rocks {
        let rock_idx = usize::try_from(i % (rocks.len() as u32)).unwrap();
        let rock = &rocks[rock_idx];
        let mut bottom_left_corner_position: (u32, u32) = (2, rock_tower_height + 3);

        if display {
            display_rock_tower_with_rock(
                &mut (rock_tower.clone()),
                rock,
                bottom_left_corner_position,
            )
        };

        loop {
            let jet_pattern = jet_patterns[jet_idx % jet_patterns.len()];
            match jet_pattern {
                '<' => {
                    if can_move_left(&rock_tower, rock, bottom_left_corner_position) {
                        bottom_left_corner_position.0 = bottom_left_corner_position.0 - 1
                    }
                }
                '>' => {
                    if can_move_right(&rock_tower, rock, bottom_left_corner_position) {
                        bottom_left_corner_position.0 = bottom_left_corner_position.0 + 1
                    }
                }
                _ => panic!("Unrecognised jet pattern {}", jet_pattern),
            }
            jet_idx += 1;

            if display {
                display_rock_tower_with_rock(
                    &mut (rock_tower.clone()),
                    rock,
                    bottom_left_corner_position,
                );
            }

            if can_move_down(&rock_tower, rock, bottom_left_corner_position) {
                bottom_left_corner_position.1 = bottom_left_corner_position.1 - 1
            } else {
                break;
            }

            if display {
                display_rock_tower_with_rock(
                    &mut (rock_tower.clone()),
                    rock,
                    bottom_left_corner_position,
                );
            }
        }
        add_rock_to_tower(&mut rock_tower, rock, bottom_left_corner_position);
        rock_tower_height = tower_height(&rock_tower);

        if display {
            display_rock_tower(&rock_tower);
        };
    }

    rock_tower_height
}

pub fn part1(file_path: &str, num_rocks: u32, display: bool) -> u32 {
    let start_time = SystemTime::now();

    let jet_patterns: Vec<char> = fs::read_to_string(file_path).unwrap().chars().collect();
    let rocks = parse_rocks("../data/day_17_rocks.txt");
    let result = rock_tower_height(jet_patterns, rocks, num_rocks, display);

    println!(
        "Time taken: {}s",
        (start_time.elapsed().unwrap().as_millis() as f32) / 1000.0
    );
    result
}
