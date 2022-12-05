package main

import (
	"fmt"
	"strings"

	"github.com/thomasschafer/advent_of_code_2022/utils"
)

var opponent_values = map[string]int{
	"A": 1,
	"B": 2,
	"C": 3,
}

var user_values = map[string]int{
	"X": 1,
	"Y": 2,
	"Z": 3,
}

var result_scores = map[int]int{
	1: 6,
	0: 3,
	2: 0,
}

func calculate_score(opponent_shape, user_shape string) int {
	shape_score := user_values[user_shape]

	user_shape_value := user_values[user_shape]
	opponent_shape_value := opponent_values[opponent_shape]
	result_score := result_scores[((user_shape_value - opponent_shape_value + 3) % 3)]
	return shape_score + result_score
}

func part_1() {
	rows := utils.RowsFromFile("../../data/day_2.txt")
	user_score := 0
	for _, row := range rows {
		if row == "" {
			continue
		}
		shapes := strings.Split(row, " ")
		opponent_shape := shapes[0]
		user_shape := shapes[1]
		user_score += calculate_score(opponent_shape, user_shape)
	}
	fmt.Println(user_score)
}
