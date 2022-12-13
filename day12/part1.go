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

const startString = "S"
const endString = "E"

func characterDifference(current string, next string) int {
	if !(len(current) == 1 && len(next) == 1) {
		panic("Arguments should be of length 1")
	}
	if next == endString {
		next = "z"
	}
	if current == startString {
		current = "a"
	}
	return int([]rune(next)[0]) - int([]rune(current)[0])
}

func positionsToMoveTo(position MapPosition, heightMap HeightMap, visited map[string]bool) []MapPosition {
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
			characterDifference(heightMap[position.y][position.x], heightMap[possibleNewPosition.y][possibleNewPosition.x]) <= 1 {
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

func fewestStepsToReachEnd(heightMap HeightMap) int {
	x, y := heightMap.findChar(startString)
	initialPosition := MapPosition{x: x, y: y}
	positions := []MapPosition{initialPosition}
	visited := make(map[string]bool)
	steps := 1 // We won't bother checking whether the starting point is also the end point
	for {
		newPositions := []MapPosition{}
		for _, position := range positions {
			for _, newPosition := range positionsToMoveTo(position, heightMap, visited) {
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
	return fewestStepsToReachEnd(heightMap)
}
