package day01

import (
	"cuelang.org/go/pkg/strconv"
	"github.com/thomasschafer/advent_of_code_2022/utils"
)

func caloriesFromTopElves(rows []string, numElves int) int {
	maxLengthArray := utils.NewMaxLengthArray(numElves)
	currentTotalCalories := 0
	for _, calories := range rows {
		if calories == "" {
			maxLengthArray.AppendIfLargeEnough(currentTotalCalories)
			currentTotalCalories = 0
		} else {
			currentTotalCalories += utils.Expect(strconv.Atoi(calories))
		}
	}
	return utils.Sum(maxLengthArray.GetData())

}

func Part2(filePath string) int {
	rows := utils.RowsFromFile(filePath)
	return caloriesFromTopElves(rows, 3)
}
