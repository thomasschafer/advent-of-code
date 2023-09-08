package day09

import (
	"strconv"
	"strings"

	"github.com/thomasschafer/advent_of_code_2022/utils"
)

func numPositionsVisitedWithLongRope(rows []string, numKnots int) int {
	allKnotPositions := make([]*Position, numKnots)
	for i := 0; i <= numKnots-1; i++ {
		allKnotPositions[i] = &Position{}
	}
	positionsVisited := make(map[string]bool)
	for _, row := range rows {
		rowSplit := strings.Split(row, " ")
		direction := rowSplit[0]
		numMoves := utils.Expect(strconv.Atoi(rowSplit[1]))
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

func Part2(filePath string) int {
	rows := utils.RowsFromFile(filePath)
	return numPositionsVisitedWithLongRope(rows, 10)
}
