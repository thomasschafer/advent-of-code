package day14

import (
	"fmt"
	"strconv"
	"strings"
	"time"

	"github.com/thomasschafer/advent_of_code_2022/utils"
)

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

func Part2(filePath string, visualise bool) int {
	rows := utils.RowsFromFile(filePath)
	caveGrid := parseCaveGrid2(rows)
	caveGrid.display()
	result := unitsOfSandThatComeToRest2(&caveGrid, visualise)
	caveGrid.display()
	return result
}
