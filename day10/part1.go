package day10

import (
	"fmt"
	"strconv"

	"github.com/thomasschafer/advent_of_code_2022/utils"
)

func sumOfSignalStrengths(rows []string, cyclesToMeasure []int) int {
	maxCycle := utils.Max(cyclesToMeasure...)
	cyclesToMeasureSet := utils.ArrayToSet(cyclesToMeasure)
	result := 0
	registerValue := 1
	cycle := 1
	for _, row := range rows {
		if row == "noop" {
			if cyclesToMeasureSet[cycle] {
				result += registerValue * cycle
			}
			cycle++
		} else if row[:4] == "addx" {
			for i := 0; i < 2; i++ {
				if cyclesToMeasureSet[cycle] {
					result += registerValue * cycle
				}
				cycle++
			}
			registerValue += utils.Expect(strconv.Atoi(row[5:]))
		} else {
			panic(fmt.Sprintf("Unexpected command %v", row))
		}
		if cycle > maxCycle {
			break
		}
	}
	return result
}

func Part1(filePath string) int {
	rows := utils.RowsFromFile(filePath)
	cyclesToMeasure := []int{20, 60, 100, 140, 180, 220}
	return sumOfSignalStrengths(rows, cyclesToMeasure)
}
