package day06

import (
	"github.com/thomasschafer/advent_of_code_2022/utils"
)

func Part2(filePath string) []int {
	rows := utils.RowsFromFile(filePath)
	results := []int{}
	for _, row := range rows {
		results = append(results, findBufferEnd(row, 14))
	}
	return results
}
