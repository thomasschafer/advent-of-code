package day13

import (
	"fmt"
	"strconv"

	"github.com/thomasschafer/advent_of_code_2022/utils"
)

func findFirstChar(s string, charToFind rune) (int, error) {
	for idx, char := range s {
		if char == charToFind {
			return idx, nil
		}
	}
	return 0, fmt.Errorf("could not find separator %v in string %v", charToFind, s)
}

func findClosingBracketIndex(row string) int {
	if row[0] != '[' {
		panic(fmt.Sprintf("Expected first character of row to be [, instead found row %v", row))
	}
	openBrackets := 0
	for idx, char := range row {
		if char == '[' {
			openBrackets += 1
		} else if char == ']' {
			openBrackets -= 1
			if openBrackets == 0 {
				return idx
			}
		}
	}
	panic(fmt.Sprintf("Could not find closing bracket in string %v", row))
}

type DeepList = []any

func parseList(row string) DeepList {
	var result DeepList
	if len(row) == 0 {
		result = DeepList{}
	} else if row[0] == '[' {
		closingBracketIdx := findClosingBracketIndex(row)
		result = DeepList{parseList(row[1:closingBracketIdx])}
		if closingBracketIdx+2 < len(row) {
			result = append(result, parseList(row[closingBracketIdx+2:])...)
		}
	} else {
		nextCommaIdx, err := findFirstChar(row, ',')
		if err != nil {
			nextCommaIdx = len(row)
		}
		firstElement := utils.Expect(strconv.Atoi(row[:nextCommaIdx]))
		result = DeepList{firstElement}
		if err == nil {
			result = append(result, parseList(row[nextCommaIdx+1:])...)
		}
	}
	return result
}

func parseListWithCheck(row string) DeepList {
	if row[0] != '[' || row[len(row)-1] != ']' {
		panic(fmt.Sprintf("Expected row to start with [ and end with ], instead found %v", row))
	}
	return parseList(row[1 : len(row)-1])
}

func compare(left any, right any) int {
	switch left.(type) {
	case int:
		switch right.(type) {
		case int:
			if left.(int) < right.(int) {
				return -1
			} else if left.(int) > right.(int) {
				return 1
			} else {
				return 0
			}
		default:
			left = DeepList{left}
		}
	case DeepList:
		switch right.(type) {
		case DeepList:
			if len(left.(DeepList)) == 0 && len(right.(DeepList)) == 0 {
				return 0
			} else if len(left.(DeepList)) == 0 {
				return -1
			} else if len(right.(DeepList)) == 0 {
				return 1
			}
			firstElementsCompared := compare(left.(DeepList)[0], right.(DeepList)[0])
			if firstElementsCompared != 0 {
				return firstElementsCompared
			}
			left = left.(DeepList)[1:]
			right = right.(DeepList)[1:]
		case int:
			right = DeepList{right}
		}
	}
	return compare(left, right)
}

func sumOfIndicesInRightOrder(rows []string) int {
	result := 0
	i := 0
	pairIndex := 1
	for i < len(rows) {
		left := rows[i]
		right := rows[i+1]
		leftParsed := parseListWithCheck(left)
		rightParsed := parseListWithCheck(right)
		compareResult := compare(leftParsed, rightParsed)
		if compareResult == -1 {
			result += pairIndex
		}
		i += 3
		pairIndex++
	}
	return result
}

func Part1(filePath string) int {
	rows := utils.RowsFromFile(filePath)
	return sumOfIndicesInRightOrder(rows)
}
