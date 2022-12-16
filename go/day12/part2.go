package day12

import (
	"github.com/thomasschafer/advent_of_code_2022/utils"
)

func Part2(filePath string) int {
	rows := utils.RowsFromFile(filePath)
	heightMap := parseHeightMap(rows)
	isValidNextElevation := func(current int, next int) bool {
		return next >= current-1
	}
	return fewestStepsToReachEnd(heightMap, "E", "a", isValidNextElevation)
}
