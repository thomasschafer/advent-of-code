package main

import (
	"fmt"
	"io"
	"os"
	"strings"

	"solutions/aoc"
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

func part1(input string) int {
	rows := aoc.Rows(input)
	userScore := 0
	for _, row := range rows {
		shapes := strings.Split(row, " ")
		opponentShape := shapes[0]
		userShape := shapes[1]
		userScore += calculateScore(opponentShape, userShape)
	}
	return userScore
}

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

func getIfExists[K comparable, V any](dict map[K]V, key K) V {
	value, ok := dict[key]
	if !ok {
		panic(fmt.Sprintf("Could not find key %v in map dict %v", key, dict))
	}
	return value
}

func calculateScore2(opponentShape, requiredResult string) int {
	opponentShapeScore := getIfExists(opponentValues, opponentShape)
	requiredUserShapeScore := (opponentShapeScore + getIfExists(resultMapping, requiredResult)) % 3
	if requiredUserShapeScore == 0 {
		requiredUserShapeScore = 3
	}
	resultScore := getIfExists(resultScoresByShape, requiredResult)
	return requiredUserShapeScore + resultScore
}

func part2(input string) int {
	rows := aoc.Rows(input)
	userScore := 0
	for _, row := range rows {
		shapes := strings.Split(row, " ")
		opponentShape := shapes[0]
		requiredResult := shapes[1]
		userScore += calculateScore2(opponentShape, requiredResult)
	}
	return userScore
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
