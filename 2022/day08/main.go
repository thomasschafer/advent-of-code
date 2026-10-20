package main

import (
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"

	"solutions/aoc"
)

func buildTreeGrid(rows []string) [][]int {
	grid := [][]int{}
	for _, row := range rows {
		gridRow := []int{}
		for _, char := range strings.Split(row, "") {
			gridRow = append(gridRow, aoc.Expect(strconv.Atoi(char)))
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

func part1(input string) int {
	rows := aoc.Rows(input)
	grid := buildTreeGrid((rows))
	return treesVisibleFromOutsideGrid((grid))
}

func scenicScore(grid [][]int, row int, col int) int {
	height := len(grid)
	width := len(grid[0])
	curHeight := grid[row][col]
	upScore := row
	// fmt.Println("upScore", upScore)
	for i := row - 1; i >= 0; i-- {
		// fmt.Printf("row=%v, col=%v, grid[i][col] >= %v, curHeight = %v\n", row, col, grid[i][col], curHeight)
		if grid[i][col] >= curHeight {
			upScore = row - i
			break
		}
	}
	rightScore := width - 1 - col
	for j := col + 1; j <= width-1; j++ {
		if grid[row][j] >= curHeight {
			rightScore = j - col
			break
		}
	}
	downScore := height - 1 - row
	for i := row + 1; i <= height-1; i++ {
		if grid[i][col] >= curHeight {
			downScore = i - row
			break
		}
	}
	leftScore := col
	for j := col - 1; j >= 0; j-- {
		if grid[row][j] >= curHeight {
			leftScore = col - j
			break
		}
	}
	// fmt.Printf("row=%v, col=%v, upScore=%v, rightScore=%v, downScore=%v, leftScore=%v\n", row, col, upScore, rightScore, downScore, leftScore)
	return upScore * rightScore * downScore * leftScore
}

func part2(input string) int {
	rows := aoc.Rows(input)
	grid := buildTreeGrid(rows)
	highestScenicScore := 0
	for _, row := range grid {
		fmt.Fprintln(os.Stderr, row)
	}
	for i := 0; i <= len(grid)-1; i++ {
		for j := 0; j <= len(grid[0])-1; j++ {
			score := scenicScore(grid, i, j)
			fmt.Fprintf(os.Stderr, "%v, ", score)
			if score > highestScenicScore {
				highestScenicScore = score
			}
		}
		fmt.Fprintln(os.Stderr)
	}
	return highestScenicScore
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
