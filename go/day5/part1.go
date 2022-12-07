package day5

import (
	"os"
	"strconv"
	"strings"

	"github.com/thomasschafer/advent_of_code_2022/utils"
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

func parseStackFile(filePath string) StackInstructions {
	b, err := os.ReadFile(filePath)
	if err != nil {
		panic(err)
	}
	data := string(b)
	rows := strings.Split(data, "\n")
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
	maxKey := utils.MaxKeyFromMap(stackMap)
	for i := 0; i <= maxKey; i++ {
		curStack := stackMap[i]
		curStackWithoutNumber := curStack[:len(curStack)-1]
		utils.Reverse(curStackWithoutNumber)
		stackInstructions.stacks = append(stackInstructions.stacks, curStackWithoutNumber)
	}
	i++

	// Parse instructions
	for i < numRows {
		rowSplit := strings.Split(rows[i], " ")
		movement := Movement{
			numToMove: utils.PanicIfErr(strconv.Atoi(rowSplit[1])),
			from:      utils.PanicIfErr(strconv.Atoi(rowSplit[3])),
			to:        utils.PanicIfErr(strconv.Atoi(rowSplit[5])),
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

func Part1(filePath string) string {
	stackInstructions := parseStackFile(filePath)
	stackInstructions.applyInstructionsPart1()

	topCrates := []string{}
	for _, stack := range stackInstructions.stacks {
		topCrates = append(topCrates, stack[len(stack)-1])
	}
	return strings.Join(topCrates, "")
}
