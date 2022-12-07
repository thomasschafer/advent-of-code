package day5

import (
	"strings"
)

func (stackInstructions StackInstructions) applyInstructionsPart2() {
	for _, instruction := range stackInstructions.instructions {
		fromStack := stackInstructions.stacks[instruction.from-1]
		elementsToMove := fromStack[len(fromStack)-instruction.numToMove:]
		stackInstructions.stacks[instruction.to-1] = append(stackInstructions.stacks[instruction.to-1], elementsToMove...)
		stackInstructions.stacks[instruction.from-1] = fromStack[:len(fromStack)-instruction.numToMove]
	}
}

func Part2(filePath string) string {
	stackInstructions := parseStackFile(filePath)
	stackInstructions.applyInstructionsPart2()

	topCrates := []string{}
	for _, stack := range stackInstructions.stacks {
		topCrates = append(topCrates, stack[len(stack)-1])
	}
	return strings.Join(topCrates, "")
}
