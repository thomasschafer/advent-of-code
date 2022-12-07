package day1

import (
	"cuelang.org/go/pkg/strconv"
	"github.com/thomasschafer/advent_of_code_2022/utils"
)

func Part1(filePath string) int {
	rows := utils.RowsFromFile(filePath)

	var maxTotalCalories int = 0
	var currentTotalCalories int = 0

	for _, calories := range rows {
		caloriesInt, _ := strconv.Atoi(calories)
		if calories == "" {
			maxTotalCalories = utils.Max(maxTotalCalories, currentTotalCalories)
			currentTotalCalories = 0
		} else {
			currentTotalCalories += caloriesInt
		}
	}

	return maxTotalCalories
}
