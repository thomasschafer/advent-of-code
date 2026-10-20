package main

import (
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"

	"solutions/aoc"
)

func arrayToSet[T comparable](array []T) map[T]bool {
	result := make(map[T]bool)
	for _, x := range array {
		result[x] = true
	}
	return result
}

func sumOfSignalStrengths(rows []string, cyclesToMeasure []int) int {
	maxCycle := aoc.Max(cyclesToMeasure...)
	cyclesToMeasureSet := arrayToSet(cyclesToMeasure)
	result := 0
	registerValue := 1
	cycle := 1
	for _, row := range rows {
		if row == "noop" {
			if cyclesToMeasureSet[cycle] {
				result += registerValue * cycle
			}
			cycle++
		} else if row[:4] == "addx" {
			for i := 0; i < 2; i++ {
				if cyclesToMeasureSet[cycle] {
					result += registerValue * cycle
				}
				cycle++
			}
			registerValue += aoc.Expect(strconv.Atoi(row[5:]))
		} else {
			panic(fmt.Sprintf("Unexpected command %v", row))
		}
		if cycle > maxCycle {
			break
		}
	}
	return result
}

func part1(input string) int {
	rows := aoc.Rows(input)
	cyclesToMeasure := []int{20, 60, 100, 140, 180, 220}
	return sumOfSignalStrengths(rows, cyclesToMeasure)
}

const screenWidth = 40
const screenHeight = 6

type CrtPosition struct {
	x int
	y int
}

func (position *CrtPosition) move() {
	if position.x+1 == screenWidth {
		position.x = 0
		position.y = (position.y + 1) % screenHeight
	} else {
		position.x++
	}
}

type Screen [][]string

func createScreen(height int, width int) Screen {
	screen := make([][]string, height)
	for i := 0; i < height; i++ {
		screen[i] = make([]string, width)
	}
	return screen
}

// print draws the screen on stderr, as its letters are read by eye rather than printed as the
// answer.
func (screen Screen) print() {
	for _, row := range screen {
		fmt.Fprintln(os.Stderr, strings.Join(row, ""))
	}
}

func (screen *Screen) update(crtPosition *CrtPosition, registerValue int, cycle *int) {
	if aoc.Abs(crtPosition.x-registerValue) <= 1 {
		(*screen)[crtPosition.y][crtPosition.x] = "#"
	} else {
		(*screen)[crtPosition.y][crtPosition.x] = "."
	}
	crtPosition.move()
	(*cycle)++
}

func drawCrtOutput(rows []string) {
	registerValue := 1
	cycle := 0
	crtPosition := CrtPosition{}
	screen := createScreen(screenHeight, screenWidth)
	for _, row := range rows {
		if row == "noop" {
			screen.update(&crtPosition, registerValue, &cycle)
		} else if row[:4] == "addx" {
			for i := 0; i < 2; i++ {
				screen.update(&crtPosition, registerValue, &cycle)
			}
			registerValue += aoc.Expect(strconv.Atoi(row[5:]))
		} else {
			panic(fmt.Sprintf("Unexpected command %v", row))
		}
	}
	screen.print()
}

func part2(input string) {
	rows := aoc.Rows(input)
	drawCrtOutput(rows)
}

func main() {
	data, err := io.ReadAll(os.Stdin)
	if err != nil {
		panic(err)
	}
	input := string(data)
	fmt.Println(part1(input))
	part2(input)
}
