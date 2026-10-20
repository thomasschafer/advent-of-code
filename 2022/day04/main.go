package main

import (
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"

	"solutions/aoc"
)

func assignmentContainsAnother(assignment1, assignment2 string) bool {
	assignment1Split := strings.Split(assignment1, "-")
	min1 := aoc.Expect(strconv.Atoi(assignment1Split[0]))
	max1 := aoc.Expect(strconv.Atoi(assignment1Split[1]))
	assignment2Split := strings.Split(assignment2, "-")
	min2 := aoc.Expect(strconv.Atoi(assignment2Split[0]))
	max2 := aoc.Expect(strconv.Atoi(assignment2Split[1]))
	return (min1 <= min2 && max1 >= max2) || (min2 <= min1 && max2 >= max1)
}

func part1(input string) int {
	rows := aoc.Rows(input)
	result := 0
	for _, row := range rows {
		rowSplit := strings.Split(row, ",")
		assignment1 := rowSplit[0]
		assignment2 := rowSplit[1]
		if assignmentContainsAnother(assignment1, assignment2) {
			result += 1
		}
	}
	return result
}

func assignmentsIntersect(assignment1, assignment2 string) bool {
	assignment1Split := strings.Split(assignment1, "-")
	min1 := aoc.Expect(strconv.Atoi(assignment1Split[0]))
	max1 := aoc.Expect(strconv.Atoi(assignment1Split[1]))
	assignment2Split := strings.Split(assignment2, "-")
	min2 := aoc.Expect(strconv.Atoi(assignment2Split[0]))
	max2 := aoc.Expect(strconv.Atoi(assignment2Split[1]))
	return (min1 <= min2 && min2 <= max1) || (min1 <= max2 && max2 <= max1) || (min1 <= min2 && max1 >= max2) || (min2 <= min1 && max2 >= max1)
}

func part2(input string) int {
	rows := aoc.Rows(input)
	result := 0
	for _, row := range rows {
		rowSplit := strings.Split(row, ",")
		assignment1 := rowSplit[0]
		assignment2 := rowSplit[1]
		if assignmentsIntersect(assignment1, assignment2) {
			result += 1
		}
	}
	return result
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
