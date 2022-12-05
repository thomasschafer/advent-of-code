mod day_2;

use std::collections::HashMap;
use lazy_static::lazy_static;

lazy_static! {
    static opponent_values: HashMap<char, i8> = HashMap::from([
        ('A', 1),
        ('B', 2),
        ('C', 3),
    ]);

    static user_values: HashMap<char, i8> = HashMap::from([
        ('X', 1),
        ('Y', 2),
        ('Z', 3),
    ]);

    static result_scores: HashMap<i8, i8> = HashMap::from([
        (1, 6),
        (0, 3),
        (-1, 0)
    ]);
}

fn calculate_score(user_shape: char, opponent_shape: char) -> i8 {
    let shape_score = user_values.get(&user_shape).unwrap();

    let user_shape_value = user_values.get(&user_shape).unwrap();
    let opponent_shape_value = opponent_values.get(&opponent_shape).unwrap();
    let result_score = result_scores.get(&((user_shape_value - opponent_shape_value) % 3)).unwrap();

    return shape_score + result_score;
}

fn main() {
    println!("Hi there!!");


    println!("{}", calculate_score('A', 'Y'));
}

// Column 1: A=Rock, B=Paper, C=Scissors
// Column 2: X=Rock, Y=Paper, Z=Scissors
// Score = shape_scores[shape] + result_scores[shape] where
//  shape_scores = {'Rock': 1, 'Paper': 2, 'Scissors': 3}
//  result_scores = {'Lose': 0, 'Draw': 3, 'Win': 6}