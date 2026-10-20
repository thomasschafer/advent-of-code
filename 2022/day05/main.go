package main

import (
	"fmt"
	"io"
	"os"
	"sort"
	"strconv"
	"strings"

	"solutions/aoc"
)

type Movement struct {
	numToMove int
	from      int
	to        int
}

type StackInstructions struct {
	stacks       [][]string
	instructions []Movement
}

func reverse[T comparable](s []T) {
	sort.SliceStable(s, func(i, j int) bool {
		return i > j
	})
}

func parseStackFile(input string) StackInstructions {
	rows := aoc.Rows(input)
	numRows := len(rows)

	stackInstructions := StackInstructions{}

	// Parse initial crate configuration
	i := 0
	stackMap := map[int][]string{}
	for i < numRows && rows[i] != "" {
		row := rows[i]
		for i, rowIdx := 1, 0; i < len(row); i, rowIdx = i+4, rowIdx+1 {
			crate := string(row[i])
			if crate != " " {
				stackMap[rowIdx] = append(stackMap[rowIdx], crate)
			}
		}
		i++
	}
	maxKey := aoc.MaxKeyFromMap(stackMap)
	for i := 0; i <= maxKey; i++ {
		curStack := stackMap[i]
		curStackWithoutNumber := curStack[:len(curStack)-1]
		reverse(curStackWithoutNumber)
		stackInstructions.stacks = append(stackInstructions.stacks, curStackWithoutNumber)
	}
	i++

	// Parse instructions
	for i < numRows {
		rowSplit := strings.Split(rows[i], " ")
		movement := Movement{
			numToMove: aoc.Expect(strconv.Atoi(rowSplit[1])),
			from:      aoc.Expect(strconv.Atoi(rowSplit[3])),
			to:        aoc.Expect(strconv.Atoi(rowSplit[5])),
		}
		stackInstructions.instructions = append(stackInstructions.instructions, movement)
		i++
	}
	return stackInstructions
}

func (stackInstructions StackInstructions) applyInstructionsPart1() {
	for _, instruction := range stackInstructions.instructions {
		for i := 0; i < instruction.numToMove; i++ {
			fromStack := stackInstructions.stacks[instruction.from-1]
			elementToMove := fromStack[len(fromStack)-1]
			stackInstructions.stacks[instruction.to-1] = append(stackInstructions.stacks[instruction.to-1], elementToMove)
			stackInstructions.stacks[instruction.from-1] = fromStack[:len(fromStack)-1]
		}
	}
}

func part1(input string) string {
	stackInstructions := parseStackFile(input)
	stackInstructions.applyInstructionsPart1()

	topCrates := []string{}
	for _, stack := range stackInstructions.stacks {
		topCrates = append(topCrates, stack[len(stack)-1])
	}
	return strings.Join(topCrates, "")
}

func (stackInstructions StackInstructions) applyInstructionsPart2() {
	for _, instruction := range stackInstructions.instructions {
		fromStack := stackInstructions.stacks[instruction.from-1]
		elementsToMove := fromStack[len(fromStack)-instruction.numToMove:]
		stackInstructions.stacks[instruction.to-1] = append(stackInstructions.stacks[instruction.to-1], elementsToMove...)
		stackInstructions.stacks[instruction.from-1] = fromStack[:len(fromStack)-instruction.numToMove]
	}
}

func part2(input string) string {
	stackInstructions := parseStackFile(input)
	stackInstructions.applyInstructionsPart2()

	topCrates := []string{}
	for _, stack := range stackInstructions.stacks {
		topCrates = append(topCrates, stack[len(stack)-1])
	}
	return strings.Join(topCrates, "")
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
