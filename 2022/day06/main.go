package main

import (
	"fmt"
	"io"
	"os"

	"solutions/aoc"
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

func part1(input string) []int {
	rows := aoc.Rows(input)
	results := []int{}
	for _, row := range rows {
		results = append(results, findBufferEnd(row, 4))
	}
	return results
}

func part2(input string) []int {
	rows := aoc.Rows(input)
	results := []int{}
	for _, row := range rows {
		results = append(results, findBufferEnd(row, 14))
	}
	return results
}

func main() {
	data, err := io.ReadAll(os.Stdin)
	if err != nil {
		panic(err)
	}
	input := string(data)
	// The puzzle input is one datastream, so there's one result for each part.
	fmt.Println(part1(input)[0])
	fmt.Println(part2(input)[0])
}
