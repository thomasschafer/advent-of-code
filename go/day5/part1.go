package day5

import (
	"fmt"
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

func Part1(filePath string) []string {
	stackInstructions := parseStackFile(filePath)
	fmt.Println("----- Stacks: -----")
	for _, stack := range stackInstructions.stacks {
		fmt.Println(stack)
	}
	fmt.Println("----- Instructions: -----")
	for _, instruction := range stackInstructions.instructions {
		fmt.Printf("%#v\n", instruction)
	}
	return []string{} // TODO
}
