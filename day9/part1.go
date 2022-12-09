package day9

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/thomasschafer/advent_of_code_2022/utils"
)

type Position struct {
	x int
	y int
}

func (position Position) toString() string {
	return fmt.Sprintf("%x-%v", position.x, position.y)
}

func (position *Position) move(direction string) {
	switch direction {
	case "R":
		position.x++
	case "D":
		position.y--
	case "L":
		position.x--
	case "U":
		position.y++
	default:
		panic(fmt.Sprintf("Expected one of R, D, L, U, instead received %v", direction))
	}
}

func (position *Position) follow(head Position) {
	if head.y == position.y {
		// Move horizontally
		if head.x > position.x+1 {
			position.x++
		} else if head.x < position.x-1 {
			position.x--
		}
	} else if head.x == position.x {
		// Move vertically
		if head.y > position.y+1 {
			position.y++
		} else if head.y < position.y-1 {
			position.y--
		}
	} else if utils.Abs(head.y-position.y)+utils.Abs(head.x-position.x) > 2 {
		// Diagonal case
		if head.x > position.x {
			position.x++
		} else {
			position.x--
		}
		if head.y > position.y {
			position.y++
		} else {
			position.y--
		}
	}
}

func numPositionsVisited(rows []string) int {
	headPosition := Position{}
	tailPosition := Position{}
	// positionsVisited maps "<dx>-<dy>" to true if visited, where <dx> and <dy> are the distances
	// moved from the starting point horizontally and vertically respectively
	positionsVisited := make(map[string]bool)
	for _, row := range rows {
		rowSplit := strings.Split(row, " ")
		direction := rowSplit[0]
		numMoves := utils.PanicIfErr(strconv.Atoi(rowSplit[1]))
		for i := 1; i <= numMoves; i++ {
			headPosition.move(direction)
			tailPosition.follow(headPosition)
			positionsVisited[tailPosition.toString()] = true
		}
	}
	result := 0
	for _, val := range positionsVisited {
		if val {
			result++
		}
	}
	return result
}

func Part1(filePath string) int {
	rows := utils.RowsFromFile(filePath)
	return numPositionsVisited(rows)
}
