package main

import (
	"fmt"
	"io"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"time"

	"solutions/aoc"
)

type CaveElement string

const (
	ROCK       CaveElement = "#"
	SAND       CaveElement = "o"
	SAND_START CaveElement = "+"
	EMPTY      CaveElement = ""
)

type SandPosition struct {
	x int
	y int
}

type CaveGrid struct {
	grid      [][]CaveElement
	sandStart SandPosition
	minX      int
	maxX      int
	minY      int
	maxY      int
}

func (caveGrid *CaveGrid) set(x int, y int, value CaveElement) {
	caveGrid.grid[y-caveGrid.minY][x-caveGrid.minX] = value
}
func (caveGrid *CaveGrid) get(x int, y int) CaveElement {
	return caveGrid.grid[y-caveGrid.minY][x-caveGrid.minX]
}

func clearScreen() {
	cmd := exec.Command("clear")
	cmd.Stdout = os.Stderr
	cmd.Run()
}

func (caveGrid CaveGrid) display() {
	result := ""
	for i := 0; i < len(caveGrid.grid[0])+2; i++ {
		result += "-"
	}
	result += "\n"
	for _, row := range caveGrid.grid {
		result += "|"
		for _, c := range row {
			if c == "" {
				result += " "
			} else {
				result += string(c)
			}
		}
		result += "|\n"
	}
	for i := 0; i < len(caveGrid.grid[0])+2; i++ {
		result += "-"
	}
	clearScreen()
	fmt.Fprintln(os.Stderr, result+"\n")
}

func newCaveGrid(minX int, maxX int, minY int, maxY int, sandStart SandPosition) CaveGrid {
	grid := [][]CaveElement{}
	width := maxX + 1 - minX
	height := maxY + 1 - minY
	for idx := 0; idx < height; idx++ {
		grid = append(grid, make([]CaveElement, width))
	}
	caveGrid := CaveGrid{grid: grid, minX: minX, maxX: maxX, minY: minY, maxY: maxY, sandStart: sandStart}
	return caveGrid
}

var sandStartPosition = SandPosition{x: 500, y: 0}

func parseCaveGrid(rows []string) CaveGrid {
	minX := sandStartPosition.x
	maxX := sandStartPosition.x
	minY := sandStartPosition.y
	maxY := sandStartPosition.y
	parsedRows := [][][]int{}
	for _, row := range rows {
		parsedRow := [][]int{}
		rowSplit := strings.Split(row, " -> ")
		for _, pair := range rowSplit {
			pairSplit := strings.Split(pair, ",")
			x := aoc.Expect(strconv.Atoi(string(pairSplit[0])))
			y := aoc.Expect(strconv.Atoi(string(pairSplit[1])))
			maxX = aoc.Max(maxX, x)
			minX = aoc.Min(minX, x)
			maxY = aoc.Max(maxY, y)
			minY = aoc.Min(minY, y)
			parsedRow = append(parsedRow, []int{x, y})
		}
		parsedRows = append(parsedRows, parsedRow)
	}

	caveGrid := newCaveGrid(minX, maxX, minY, maxY, sandStartPosition)

	caveGrid.set(sandStartPosition.x, sandStartPosition.y, SAND_START)

	for _, row := range parsedRows {
		for i := 0; i <= len(row)-2; i++ {
			pair1 := row[i]
			x1 := pair1[0]
			y1 := pair1[1]
			pair2 := row[i+1]
			x2 := pair2[0]
			y2 := pair2[1]
			if y1 == y2 {
				xStart := aoc.Min(x1, x2)
				xEnd := aoc.Max(x1, x2)
				for x := xStart; x <= xEnd; x++ {
					caveGrid.set(x, y1, ROCK)
				}
			} else if x1 == x2 {
				yStart := aoc.Min(y1, y2)
				yEnd := aoc.Max(y1, y2)
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

func unitsOfSandThatComeToRest(caveGrid *CaveGrid, visualise bool) int {
	result := 0
	for {
		currentSandPosition := SandPosition{x: sandStartPosition.x, y: sandStartPosition.y}
		sandHasSettled := false
		for !sandHasSettled {
			allPossibleMoves := []SandPosition{
				{x: currentSandPosition.x, y: currentSandPosition.y + 1},
				{x: currentSandPosition.x - 1, y: currentSandPosition.y + 1},
				{x: currentSandPosition.x + 1, y: currentSandPosition.y + 1},
			}
			couldMoveSand := false
			for _, move := range allPossibleMoves {
				if move.y > caveGrid.maxY || move.x < caveGrid.minX || move.x > caveGrid.maxX {
					return result
				}
				if caveGrid.get(move.x, move.y) == EMPTY {
					caveGrid.set(currentSandPosition.x, currentSandPosition.y, EMPTY)
					currentSandPosition = move
					caveGrid.set(currentSandPosition.x, currentSandPosition.y, SAND)
					couldMoveSand = true
					break
				}
			}
			if !couldMoveSand {
				result += 1
				sandHasSettled = true
			}
			if visualise {
				caveGrid.display()
				time.Sleep(10 * time.Millisecond)
			}
		}
	}
}

func part1(input string, visualise bool) int {
	rows := aoc.Rows(input)
	caveGrid := parseCaveGrid(rows)
	caveGrid.display()
	result := unitsOfSandThatComeToRest(&caveGrid, visualise)
	caveGrid.display()
	return result
}

func parseCaveGrid2(rows []string) CaveGrid {
	minX := sandStartPosition.x
	maxX := sandStartPosition.x
	minY := sandStartPosition.y
	maxY := sandStartPosition.y
	parsedRows := [][][]int{}
	for _, row := range rows {
		parsedRow := [][]int{}
		rowSplit := strings.Split(row, " -> ")
		for _, pair := range rowSplit {
			pairSplit := strings.Split(pair, ",")
			x := aoc.Expect(strconv.Atoi(string(pairSplit[0])))
			y := aoc.Expect(strconv.Atoi(string(pairSplit[1])))
			maxX = aoc.Max(maxX, x)
			minX = aoc.Min(minX, x)
			maxY = aoc.Max(maxY, y)
			minY = aoc.Min(minY, y)
			parsedRow = append(parsedRow, []int{x, y})
		}
		parsedRows = append(parsedRows, parsedRow)
	}
	offsetToAdd := maxY + 1 - minY
	caveGrid := newCaveGrid(minX-offsetToAdd, maxX+offsetToAdd, minY, maxY+2, sandStartPosition)

	caveGrid.set(sandStartPosition.x, sandStartPosition.y, SAND_START)

	for _, row := range parsedRows {
		for i := 0; i <= len(row)-2; i++ {
			pair1 := row[i]
			x1 := pair1[0]
			y1 := pair1[1]
			pair2 := row[i+1]
			x2 := pair2[0]
			y2 := pair2[1]
			if y1 == y2 {
				xStart := aoc.Min(x1, x2)
				xEnd := aoc.Max(x1, x2)
				for x := xStart; x <= xEnd; x++ {
					caveGrid.set(x, y1, ROCK)
				}
			} else if x1 == x2 {
				yStart := aoc.Min(y1, y2)
				yEnd := aoc.Max(y1, y2)
				for y := yStart; y <= yEnd; y++ {
					caveGrid.set(x1, y, ROCK)
				}
			} else {
				panic(fmt.Sprintf("Expected either x1 == x2 or y1 == y2, instead found x1=%v, y1=%v, x2=%v and y2=%v", x1, y1, x2, y2))
			}
		}
	}

	for i := 0; i < len(caveGrid.grid[0]); i++ {
		caveGrid.grid[len(caveGrid.grid)-1][i] = ROCK
	}

	return caveGrid
}

func unitsOfSandThatComeToRest2(caveGrid *CaveGrid, visualise bool) int {
	result := 0
	for {
		if caveGrid.get(sandStartPosition.x, sandStartPosition.y) == SAND {
			return result
		}
		currentSandPosition := SandPosition{x: sandStartPosition.x, y: sandStartPosition.y}
		sandHasSettled := false
		for !sandHasSettled {
			allPossibleMoves := []SandPosition{
				{x: currentSandPosition.x, y: currentSandPosition.y + 1},
				{x: currentSandPosition.x - 1, y: currentSandPosition.y + 1},
				{x: currentSandPosition.x + 1, y: currentSandPosition.y + 1},
			}
			couldMoveSand := false
			for _, move := range allPossibleMoves {
				if move.y >= caveGrid.maxY+2 || move.x < caveGrid.minX || move.x > caveGrid.maxX {
					panic(fmt.Sprintf("Out of bounds - attempted to move to x=%v,y=%v", move.x, move.y))
				}
				if caveGrid.get(move.x, move.y) == EMPTY {
					caveGrid.set(currentSandPosition.x, currentSandPosition.y, EMPTY)
					currentSandPosition = move
					caveGrid.set(currentSandPosition.x, currentSandPosition.y, SAND)
					couldMoveSand = true
					break
				}
			}
			if !couldMoveSand {
				caveGrid.set(currentSandPosition.x, currentSandPosition.y, SAND)
				result += 1
				sandHasSettled = true
			}
			if visualise {
				caveGrid.display()
				time.Sleep(1 * time.Millisecond)
			}
		}
	}
}

func part2(input string, visualise bool) int {
	rows := aoc.Rows(input)
	caveGrid := parseCaveGrid2(rows)
	caveGrid.display()
	result := unitsOfSandThatComeToRest2(&caveGrid, visualise)
	caveGrid.display()
	return result
}

func main() {
	data, err := io.ReadAll(os.Stdin)
	if err != nil {
		panic(err)
	}
	input := string(data)
	fmt.Println(part1(input, false))
	fmt.Println(part2(input, false))
}
