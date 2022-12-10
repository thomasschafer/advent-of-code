package day06

import (
	"github.com/thomasschafer/advent_of_code_2022/utils"
)

func findBufferEnd(data string, windowLength int) int {
	// Track number of unique characters seen so that we don't need to iterate over the window
	// repeatedly - we only process each character once
	uniqueCharsSeen := 0
	charsInWindow := map[string]int{}

	for i := 0; i < len(data); i++ {
		charToAdd := string(data[i])
		if charsInWindow[charToAdd] == 0 {
			uniqueCharsSeen += 1
		}
		charsInWindow[charToAdd] += 1

		if i >= windowLength {
			charToRemove := string(data[i-windowLength])
			charsInWindow[charToRemove] -= 1
			if charsInWindow[charToRemove] == 0 {
				uniqueCharsSeen -= 1
			}
		}

		if uniqueCharsSeen == windowLength {
			return i + 1
		}
	}

	panic("Could not find position")
}

func Part1(filePath string) []int {
	rows := utils.RowsFromFile(filePath)
	results := []int{}
	for _, row := range rows {
		results = append(results, findBufferEnd(row, 4))
	}
	return results
}
