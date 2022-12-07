package day3

import (
	"fmt"

	"github.com/thomasschafer/advent_of_code_2022/utils"
)

var validCharacters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

func characterToPriority(character string) int {
	for idx, letter := range validCharacters {
		if string(letter) == character {
			return idx + 1
		}
	}
	panic(fmt.Sprintf("Character %v not found", character))
}

func sharedItemPriority(item1, item2 string) int {
	item1Items := make(map[string]bool)
	for _, item := range item1 {
		if _, present := item1Items[string(item)]; !present {
			item1Items[string(item)] = true
		}
	}
	for _, item := range item2 {
		itemString := string(item)
		if _, present := item1Items[itemString]; present {
			return characterToPriority(itemString)
		}
	}
	return 0
}

func Part1(filePath string) int {
	rows := utils.RowsFromFile(filePath)
	result := 0
	for _, row := range rows {
		items1 := row[:len(row)/2]
		items2 := row[len(row)/2:]
		result += sharedItemPriority(items1, items2)
	}
	return result
}
