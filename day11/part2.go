package day11

import (
	"github.com/thomasschafer/advent_of_code_2022/utils"
)

func Part2(filePath string) int {
	rows := utils.RowsFromFile(filePath)
	monkeys := parseRows(rows)
	return calculateMonkeyBusiness(monkeys, 10000, 1)
}
