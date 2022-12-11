package day11

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/thomasschafer/advent_of_code_2022/utils"
)

type Monkey struct {
	items     []int
	operation func(int) int
	test      func(int) int
}

func createOperation(operand1 string, operation string, operand2 string) func(int) int {
	return func(worryLevel int) int {
		var operand1Parsed int
		if operand1 == "old" {
			operand1Parsed = worryLevel
		} else {
			operand1Parsed = utils.Expect(strconv.Atoi(operand1))
		}

		var operand2Parsed int
		if operand2 == "old" {
			operand2Parsed = worryLevel
		} else {
			operand2Parsed = utils.Expect(strconv.Atoi(operand2))
		}

		switch operation {
		case "+":
			return operand1Parsed + operand2Parsed
		case "-":
			return operand1Parsed - operand2Parsed
		case "*":
			return operand1Parsed * operand2Parsed
		case "/":
			return operand1Parsed / operand2Parsed
		default:
			panic(fmt.Sprintf("Unkown operation %v", operation))
		}
	}
}

func parseRows(rows []string) map[int]*Monkey {
	monkeys := make(map[int]*Monkey)
	for i := 0; i < len(rows); i++ {
		row := rows[i]
		if row == "" {
			continue
		} else if row[:6] == "Monkey" {
			monkey := Monkey{}
			monkeyId := utils.Expect(strconv.Atoi(row[7 : len(row)-1]))
			if _, present := monkeys[monkeyId]; present {
				panic(fmt.Sprintf("Found duplicate monkey with id %v", monkeyId))
			}

			i++
			monkeyItemsStrings := strings.Split(strings.Split(rows[i], "Starting items: ")[1], ", ")
			for _, item := range monkeyItemsStrings {
				monkey.items = append(monkey.items, utils.Expect(strconv.Atoi(item)))
			}

			i++
			operationStringSplit := strings.Split(strings.Split(rows[i], "new = ")[1], " ")
			operand1 := string(operationStringSplit[0])
			operation := string(operationStringSplit[1])
			operand2 := string(operationStringSplit[2])
			monkey.operation = createOperation(operand1, operation, operand2)

			i++
			testDivisibleBy := utils.Expect(strconv.Atoi(strings.Split(rows[i], "divisible by ")[1]))
			i++
			testThrowToIfTrue := utils.Expect(strconv.Atoi(strings.Split(rows[i], "If true: throw to monkey ")[1]))
			i++
			testThrowToIfFalse := utils.Expect(strconv.Atoi(strings.Split(rows[i], "If false: throw to monkey ")[1]))
			monkey.test = func(worryLevel int) int {
				if worryLevel%testDivisibleBy == 0 {
					return testThrowToIfTrue
				} else {
					return testThrowToIfFalse
				}
			}

			monkeys[monkeyId] = &monkey
		} else {
			panic(fmt.Sprintf("Could not handle row %v", row))
		}
	}
	return monkeys
}

func productOfTopNValues[T comparable](m map[T]int, n int) int {
	topValues := utils.NewMaxLengthArray(n)
	for _, v := range m {
		topValues.AppendIfLargeEnough(v)
	}
	result := 1
	for _, x := range topValues.GetData() {
		result *= x
	}
	return result
}

func calculateMonkeyBusiness(monkeys map[int]*Monkey, numRounds int) int {
	itemsInspectedByMonkey := make(map[int]int)
	maxMonkeyId := utils.MaxKeyFromMap(monkeys)
	for round := 1; round <= numRounds; round++ {
		for monkeyId := 0; monkeyId <= maxMonkeyId; monkeyId++ {
			monkey, present := monkeys[monkeyId]
			if !present {
				panic(fmt.Sprintf("Could not find monkey with id %v \n", monkeyId))
			}
			for _, itemWorryLevel := range monkey.items {
				itemWorryLevel = monkey.operation(itemWorryLevel)
				itemWorryLevel /= 3
				monkeyToThrowTo := monkey.test(itemWorryLevel)
				monkeys[monkeyToThrowTo].items = append(monkeys[monkeyToThrowTo].items, itemWorryLevel)
				itemsInspectedByMonkey[monkeyId]++
			}
			monkey.items = []int{}
		}
	}
	return productOfTopNValues(itemsInspectedByMonkey, 2)
}

func Part1(filePath string) int {
	rows := utils.RowsFromFile(filePath)
	monkeys := parseRows(rows)
	return calculateMonkeyBusiness(monkeys, 20)
}
