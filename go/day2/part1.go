package day2

import (
	"fmt"
	"strings"

	"github.com/thomasschafer/advent_of_code_2022/utils"
)

var opponentValues = map[string]int{
	"A": 1,
	"B": 2,
	"C": 3,
}

var userValues = map[string]int{
	"X": 1,
	"Y": 2,
	"Z": 3,
}

var resultScores = map[int]int{
	1: 6,
	0: 3,
	2: 0,
}

func calculateScore(opponentShape, userShape string) int {
	shapeScore := userValues[userShape]

	userShapeValue := userValues[userShape]
	opponentShapeValue := opponentValues[opponentShape]
	resultScore := resultScores[((userShapeValue - opponentShapeValue + 3) % 3)]
	return shapeScore + resultScore
}

func Part1(filePath string) {
	rows := utils.RowsFromFile(filePath)
	userScore := 0
	for _, row := range rows {
		shapes := strings.Split(row, " ")
		opponentShape := shapes[0]
		userShape := shapes[1]
		userScore += calculateScore(opponentShape, userShape)
	}
	fmt.Println(userScore)
}
