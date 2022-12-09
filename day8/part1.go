package day8

import (
	"strconv"
	"strings"

	"github.com/thomasschafer/advent_of_code_2022/utils"
)

func buildTreeGrid(rows []string) [][]int {
	grid := [][]int{}
	for _, row := range rows {
		gridRow := []int{}
		for _, char := range strings.Split(row, "") {
			gridRow = append(gridRow, utils.PanicIfErr(strconv.Atoi(char)))
		}
		grid = append(grid, gridRow)
	}
	return grid
}

func numTreesVisible(isTreeVisibleGrid [][]bool) int {
	result := 0
	for _, row := range isTreeVisibleGrid {
		for _, isTreeVisible := range row {
			if isTreeVisible {
				result++
			}
		}
	}
	return result
}

func treesVisibleFromOutsideGrid(treeGrid [][]int) int {
	height := len(treeGrid)
	width := len(treeGrid[0])
	isTreeVisibleGrid := make([][]bool, len(treeGrid))
	for i := 0; i < height; i++ {
		isTreeVisibleGrid[i] = make([]bool, width)
	}
	for i := 0; i <= height-1; i++ {
		maxHeightFromLeft := -1
		for j := 0; j <= width-1; j++ {
			if treeGrid[i][j] > maxHeightFromLeft {
				isTreeVisibleGrid[i][j] = true
				maxHeightFromLeft = treeGrid[i][j]
			}
		}
		maxHeightFromRight := -1
		for j := width - 1; j >= 0; j-- {
			if treeGrid[i][j] > maxHeightFromRight {
				isTreeVisibleGrid[i][j] = true
				maxHeightFromRight = treeGrid[i][j]
			}
		}
	}
	for j := 0; j <= width-1; j++ {
		maxHeightFromAbove := -1
		for i := 0; i <= height-1; i++ {
			if treeGrid[i][j] > maxHeightFromAbove {
				isTreeVisibleGrid[i][j] = true
				maxHeightFromAbove = treeGrid[i][j]
			}
		}
		maxHeightFromBelow := -1
		for i := height - 1; i >= 0; i-- {
			if treeGrid[i][j] > maxHeightFromBelow {
				isTreeVisibleGrid[i][j] = true
				maxHeightFromBelow = treeGrid[i][j]
			}
		}
	}
	return numTreesVisible(isTreeVisibleGrid)
}

func Part1(filePath string) int {
	rows := utils.RowsFromFile(filePath)
	grid := buildTreeGrid((rows))
	return treesVisibleFromOutsideGrid((grid))
}
