package main

import (
	"fmt"
	"io"
	"os"
	"strconv"

	"solutions/aoc"
)

func part1(input string) int {
	rows := aoc.Rows(input)

	var maxTotalCalories int = 0
	var currentTotalCalories int = 0

	for _, calories := range rows {
		caloriesInt, _ := strconv.Atoi(calories)
		if calories == "" {
			maxTotalCalories = aoc.Max(maxTotalCalories, currentTotalCalories)
			currentTotalCalories = 0
		} else {
			currentTotalCalories += caloriesInt
		}
	}

	return maxTotalCalories
}

func caloriesFromTopElves(rows []string, numElves int) int {
	maxLengthArray := aoc.NewMaxLengthArray(numElves)
	currentTotalCalories := 0
	for _, calories := range rows {
		if calories == "" {
			maxLengthArray.AppendIfLargeEnough(currentTotalCalories)
			currentTotalCalories = 0
		} else {
			currentTotalCalories += aoc.Expect(strconv.Atoi(calories))
		}
	}
	return aoc.Sum(maxLengthArray.GetData())

}

func part2(input string) int {
	rows := aoc.Rows(input)
	return caloriesFromTopElves(rows, 3)
}

func main() {
	data, err := io.ReadAll(os.Stdin)
	if err != nil {
		panic(err)
	}
	input := string(data)
	fmt.Println(part1(input))
	fmt.Println(part2(input))
}
