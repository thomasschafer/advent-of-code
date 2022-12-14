package day14

import "github.com/thomasschafer/advent_of_code_2022/utils"

func Part2(filePath string) int {
	rows := utils.RowsFromFile(filePath)
	return len(rows)
}
