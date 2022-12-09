package day8

import (
	"fmt"

	"github.com/thomasschafer/advent_of_code_2022/utils"
)

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

func Part2(filePath string) int {
	rows := utils.RowsFromFile(filePath)
	grid := buildTreeGrid(rows)
	highestScenicScore := 0
	for _, row := range grid {
		fmt.Println(row)
	}
	for i := 0; i <= len(grid)-1; i++ {
		for j := 0; j <= len(grid[0])-1; j++ {
			score := scenicScore(grid, i, j)
			fmt.Printf("%v, ", score)
			if score > highestScenicScore {
				highestScenicScore = score
			}
		}
		fmt.Println()
	}
	return highestScenicScore
}
