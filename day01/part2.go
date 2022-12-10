package day01

import (
	"fmt"

	"cuelang.org/go/pkg/strconv"
	"github.com/thomasschafer/advent_of_code_2022/utils"
)

type MaxLengthArray struct {
	data      []int
	maxLength int
}

func (arr *MaxLengthArray) appendIfLargeEnough(value int) {
	if len(arr.data) < arr.maxLength {
		arr.data = append(arr.data, value)
	} else {
		minVal := utils.Min(arr.data...)
		if value > minVal {
			newData := []int{}
			removedMin := false
			for _, x := range arr.data {
				if x == minVal && !removedMin {
					removedMin = true
				} else {
					newData = append(newData, x)
				}
			}
			newData = append(newData, value)
			arr.data = newData
		}
	}
}

func caloriesFromTopElves(rows []string, numElves int) int {
	maxLengthArray := MaxLengthArray{data: make([]int, 0, numElves), maxLength: numElves}
	currentTotalCalories := 0
	for _, calories := range rows {
		if calories == "" {
			maxLengthArray.appendIfLargeEnough(currentTotalCalories)
			currentTotalCalories = 0
		} else {
			currentTotalCalories += utils.Expect(strconv.Atoi(calories))
		}
	}
	fmt.Println("maxLengthArray.data", maxLengthArray.data)
	return utils.Sum(maxLengthArray.data)

}

func Part2(filePath string) int {
	rows := utils.RowsFromFile(filePath)
	return caloriesFromTopElves(rows, 3)
}
