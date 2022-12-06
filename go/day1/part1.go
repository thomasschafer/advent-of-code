package day1

import (
	"fmt"

	"cuelang.org/go/pkg/strconv"
	"github.com/thomasschafer/advent_of_code_2022/utils"
	"golang.org/x/exp/constraints"
)

func max[T constraints.Ordered](a, b T) T {
	if a > b {
		return a
	}
	return b
}

func Part1(filePath string) {
	rows := utils.RowsFromFile(filePath)

	var maxTotalCalories int = 0
	var currentTotalCalories int = 0

	for _, calories := range rows {
		caloriesInt, _ := strconv.Atoi(calories)
		if calories == "" {
			maxTotalCalories = max(maxTotalCalories, currentTotalCalories)
			currentTotalCalories = 0
		} else {
			currentTotalCalories += caloriesInt
		}
	}

	fmt.Println(maxTotalCalories)
}
