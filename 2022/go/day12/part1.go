package day12

import (
	"fmt"
	"strings"

	"github.com/thomasschafer/advent_of_code_2022/utils"
)

type HeightMap [][]string

type MapPosition struct {
	x    int
	y    int
	path string // Used for debugging
}

func (mapPosition MapPosition) toString() string {
	return fmt.Sprintf("%v,%v", mapPosition.x, mapPosition.y)
}

func stringToElevation(s string) int {
	if s == "E" {
		s = "z"
	} else if s == "S" {
		s = "a"
	}
	return int([]rune(s)[0])
}

func isValidNextStep(current string, next string, startString string, endString string, isValidNextElevation isValidNextElevationFunc) bool {
	if !(len(current) == 1 && len(next) == 1) {
		panic("Arguments should be of length 1")
	}
	currentInt := stringToElevation(current)
	nextInt := stringToElevation(next)
	return isValidNextElevation(currentInt, nextInt)
}

func positionsToMoveTo(
	position MapPosition,
	heightMap HeightMap,
	visited map[string]bool,
	startString string,
	endString string,
	isValidNextElevation isValidNextElevationFunc,
) []MapPosition {
	maxX := len(heightMap[0]) - 1
	maxY := len(heightMap) - 1
	possibleNewPositions := []MapPosition{}
	if position.x > 0 {
		possibleNewPositions = append(possibleNewPositions, MapPosition{x: position.x - 1, y: position.y, path: position.path + fmt.Sprintf("%v-%v (%v), ", position.x, position.y, heightMap[position.y][position.x])})
	}
	if position.x < maxX {
		possibleNewPositions = append(possibleNewPositions, MapPosition{x: position.x + 1, y: position.y, path: position.path + fmt.Sprintf("%v-%v (%v), ", position.x, position.y, heightMap[position.y][position.x])})
	}
	if position.y > 0 {
		possibleNewPositions = append(possibleNewPositions, MapPosition{x: position.x, y: position.y - 1, path: position.path + fmt.Sprintf("%v-%v (%v), ", position.x, position.y, heightMap[position.y][position.x])})
	}
	if position.y < maxY {
		possibleNewPositions = append(possibleNewPositions, MapPosition{x: position.x, y: position.y + 1, path: position.path + fmt.Sprintf("%v-%v (%v), ", position.x, position.y, heightMap[position.y][position.x])})
	}
	newPositions := []MapPosition{}
	for _, possibleNewPosition := range possibleNewPositions {
		if !visited[possibleNewPosition.toString()] &&
			isValidNextStep(heightMap[position.y][position.x], heightMap[possibleNewPosition.y][possibleNewPosition.x], startString, endString, isValidNextElevation) {
			newPositions = append(newPositions, possibleNewPosition)
		}
	}
	return newPositions
}

func (heightMap HeightMap) findChar(c string) (int, int) {
	for y, row := range heightMap {
		for x, char := range row {
			s := string(char)
			if s == c {
				return x, y
			}
		}
	}
	panic(fmt.Sprintf("Could not find character %v", c))
}

func parseHeightMap(rows []string) HeightMap {
	heightMap := make(HeightMap, len(rows))
	for i, row := range rows {
		heightMap[i] = strings.Split(row, "")
	}
	return heightMap
}

type isValidNextElevationFunc = func(current int, next int) bool

func fewestStepsToReachEnd(
	heightMap HeightMap,
	startString string,
	endString string,
	isValidNextElevation isValidNextElevationFunc,
) int {
	x, y := heightMap.findChar(startString)
	initialPosition := MapPosition{x: x, y: y}
	positions := []MapPosition{initialPosition}
	visited := make(map[string]bool)
	steps := 1 // We won't bother checking whether the starting point is also the end point
	for {
		newPositions := []MapPosition{}
		for _, position := range positions {
			for _, newPosition := range positionsToMoveTo(position, heightMap, visited, startString, endString, isValidNextElevation) {
				if heightMap[newPosition.y][newPosition.x] == endString {
					return steps
				} else {
					newPositions = append(newPositions, newPosition)
					visited[newPosition.toString()] = true
				}
			}
		}
		positions = newPositions
		steps++
	}
}

func Part1(filePath string) int {
	rows := utils.RowsFromFile(filePath)
	heightMap := parseHeightMap(rows)
	isValidNextElevation := func(current int, next int) bool {
		return next <= current+1
	}
	return fewestStepsToReachEnd(heightMap, "S", "E", isValidNextElevation)
}
