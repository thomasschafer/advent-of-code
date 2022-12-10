package day10

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/thomasschafer/advent_of_code_2022/utils"
)

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

func (screen Screen) print() {
	for _, row := range screen {
		fmt.Println(strings.Join(row, ""))
	}
}

func (screen *Screen) update(crtPosition *CrtPosition, registerValue int, cycle *int) {
	if utils.Abs(crtPosition.x-registerValue) <= 1 {
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
			registerValue += utils.Expect(strconv.Atoi(row[5:]))
		} else {
			panic(fmt.Sprintf("Unexpected command %v", row))
		}
	}
	screen.print()
}

func Part2(filePath string) {
	rows := utils.RowsFromFile(filePath)
	drawCrtOutput(rows)
}
