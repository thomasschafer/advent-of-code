package day2

import (
	"fmt"
	"strings"

	"github.com/thomasschafer/advent_of_code_2022/utils"
)

var resultScoresByShape = map[string]int{
	"X": 0,
	"Y": 3,
	"Z": 6,
}

var resultMapping = map[string]int{
	"X": 2,
	"Y": 0,
	"Z": 1,
}

func calculateScore2(opponentShape, requiredResult string) int {
	opponentShapeScore := utils.GetIfExists(opponentValues, opponentShape)
	requiredUserShapeScore := (opponentShapeScore + utils.GetIfExists(resultMapping, requiredResult)) % 3
	if requiredUserShapeScore == 0 {
		requiredUserShapeScore = 3
	}
	resultScore := utils.GetIfExists(resultScoresByShape, requiredResult)
	return requiredUserShapeScore + resultScore
}

func Part2(filePath string) {
	rows := utils.RowsFromFile(filePath)
	userScore := 0
	for _, row := range rows {
		shapes := strings.Split(row, " ")
		opponentShape := shapes[0]
		requiredResult := shapes[1]
		userScore += calculateScore2(opponentShape, requiredResult)
	}
	fmt.Println(userScore)
}
