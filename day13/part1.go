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
	fmt.Println("Parsing row", row)
	if row[0] != '[' || row[len(row)-1] != ']' {
		panic(fmt.Sprintf("Expected row to start with [ and end with ], instead found %v", row))
	}
	return parseList(row[1 : len(row)-1])
}

// func compare(left DeepList, right DeepList) int {
// 	switch left[0].(type) {
// 	case int:
// 		switch right[0].(type) {
// 		case int:
// 			if left[0].(int) < right[0].(int) {
// 				return 1
// 			} else if left[0].(int) > right[0].(int) {
// 				return -1
// 			}
// 			left = left[1:]
// 			right = right[1:]
// 		default:
// 			left = DeepList{left}
// 		}
// 	case DeepList:
// 		switch right[0].(type) {
// 		case DeepList:
// 			firstElementsCompared := compare(left[0].(DeepList), right[0].(DeepList))
// 			if firstElementsCompared != 0 {
// 				return firstElementsCompared
// 			}
// 			left = left[1:]
// 			right = right[1:]
// 		case int:
// 			right = DeepList{right}
// 		}
// 	}
// 	return compare(left, right)
// }

func sumOfIndicesInRightOrder(rows []string) int {
	result := 0
	i := 0
	for i < len(rows) {
		left := rows[i]
		right := rows[i+1]
		leftParsed := parseListWithCheck(left)
		fmt.Println("leftParsed", leftParsed)
		rightParsed := parseListWithCheck(right)
		fmt.Println("rightParsed", rightParsed)
		fmt.Println("--")
		// if compare(leftParsed, rightParsed) == 1 {
		// 	result += 1
		// }
		i += 3
	}
	return result
}

func Part1(filePath string) int {
	rows := utils.RowsFromFile(filePath)
	return sumOfIndicesInRightOrder(rows)
}
