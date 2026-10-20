package main

import (
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"

	"solutions/aoc"
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
	} else if aoc.Abs(head.y-position.y)+aoc.Abs(head.x-position.x) > 2 {
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
		numMoves := aoc.Expect(strconv.Atoi(rowSplit[1]))
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

func part1(input string) int {
	rows := aoc.Rows(input)
	return numPositionsVisited(rows)
}

func numPositionsVisitedWithLongRope(rows []string, numKnots int) int {
	allKnotPositions := make([]*Position, numKnots)
	for i := 0; i <= numKnots-1; i++ {
		allKnotPositions[i] = &Position{}
	}
	positionsVisited := make(map[string]bool)
	for _, row := range rows {
		rowSplit := strings.Split(row, " ")
		direction := rowSplit[0]
		numMoves := aoc.Expect(strconv.Atoi(rowSplit[1]))
		for i := 1; i <= numMoves; i++ {
			allKnotPositions[0].move(direction)
			for knotIdx := 1; knotIdx <= numKnots-1; knotIdx++ {
				leader := allKnotPositions[knotIdx-1]
				follower := allKnotPositions[knotIdx]
				follower.follow(*leader)
				if knotIdx == numKnots-1 {
					positionsVisited[follower.toString()] = true
				}
			}
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

func part2(input string) int {
	rows := aoc.Rows(input)
	return numPositionsVisitedWithLongRope(rows, 10)
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
