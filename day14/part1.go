package day14

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/thomasschafer/advent_of_code_2022/utils"
)

type CaveElement string

const (
	ROCK       CaveElement = "#"
	SAND       CaveElement = "o"
	SAND_START CaveElement = "+"
)

type CaveGrid struct {
	grid [][]CaveElement
	minX int
	maxX int
	minY int
	maxY int
}

func (caveGrid *CaveGrid) set(x int, y int, setTo CaveElement) {
	caveGrid.grid[y-caveGrid.minY][x-caveGrid.minX] = setTo
}

func newCaveGrid(minX int, maxX int, minY int, maxY int) CaveGrid {
	grid := [][]CaveElement{}
	width := maxX + 1 - minX
	height := maxY + 1 - minY
	for idx := 0; idx < height; idx++ {
		grid = append(grid, make([]CaveElement, width))
	}
	caveGrid := CaveGrid{grid: grid, minX: minX, maxX: maxX, minY: minY, maxY: maxY}
	return caveGrid
}

func (caveGrid CaveGrid) display() {
	for i := 0; i < len(caveGrid.grid[0])+2; i++ {
		fmt.Print("-")
	}
	fmt.Println()
	for _, row := range caveGrid.grid {
		fmt.Print("|")
		for _, c := range row {
			if c == "" {
				fmt.Print(" ")
			} else {
				fmt.Print(c)
			}
		}
		fmt.Print("|\n")
	}
	for i := 0; i < len(caveGrid.grid[0])+2; i++ {
		fmt.Print("-")
	}
	fmt.Println()
}

const SAND_START_X = 500
const SAND_START_Y = 0

func parseCaveGrid(rows []string) CaveGrid {
	minX := SAND_START_X
	maxX := SAND_START_X
	minY := SAND_START_Y
	maxY := SAND_START_Y
	parsedRows := [][][]int{}
	for _, row := range rows {
		parsedRow := [][]int{}
		rowSplit := strings.Split(row, " -> ")
		for _, pair := range rowSplit {
			pairSplit := strings.Split(pair, ",")
			x := utils.Expect(strconv.Atoi(string(pairSplit[0])))
			y := utils.Expect(strconv.Atoi(string(pairSplit[1])))
			maxX = utils.Max(maxX, x)
			minX = utils.Min(minX, x)
			maxY = utils.Max(maxY, y)
			minY = utils.Min(minY, y)
			parsedRow = append(parsedRow, []int{x, y})
		}
		parsedRows = append(parsedRows, parsedRow)
	}

	caveGrid := newCaveGrid(minX, maxX, minY, maxY)

	caveGrid.set(SAND_START_X, SAND_START_Y, SAND_START)

	for _, row := range parsedRows {
		for i := 0; i <= len(row)-2; i++ {
			pair1 := row[i]
			x1 := pair1[0]
			y1 := pair1[1]
			pair2 := row[i+1]
			x2 := pair2[0]
			y2 := pair2[1]
			if y1 == y2 {
				xStart := utils.Min(x1, x2)
				xEnd := utils.Max(x1, x2)
				for x := xStart; x <= xEnd; x++ {
					caveGrid.set(x, y1, ROCK)
				}
			} else if x1 == x2 {
				yStart := utils.Min(y1, y2)
				yEnd := utils.Max(y1, y2)
				for y := yStart; y <= yEnd; y++ {
					caveGrid.set(x1, y, ROCK)
				}
			} else {
				panic(fmt.Sprintf("Expected either x1 == x2 or y1 == y2, instead found x1=%v, y1=%v, x2=%v and y2=%v", x1, y1, x2, y2))
			}
		}
	}
	return caveGrid
}

func Part1(filePath string) int {
	rows := utils.RowsFromFile(filePath)
	caveGrid := parseCaveGrid(rows)
	fmt.Println("caveGrid.display()")
	caveGrid.display()
	return len(rows)
}
