package day3

import (
	"fmt"

	"github.com/thomasschafer/advent_of_code_2022/utils"
)

func priorityOfBadge(items1, items2, items3 string) int {
	item1Set := make(map[string]bool)
	item2Set := make(map[string]bool)
	for _, item := range items1 {
		item1Set[string(item)] = true
	}
	for _, item := range items2 {
		item2Set[string(item)] = true
	}
	for _, item := range items3 {
		itemString := string(item)
		if item1Set[itemString] && item2Set[itemString] {
			return characterToPriority(itemString)
		}
	}
	panic(fmt.Sprintf("No shared element found for items %v, %v and %v", items1, items2, items3))
}

func Part2(filePath string) {
	rows := utils.RowsFromFile(filePath)
	result := 0
	for i := 0; i+2 < len(rows); i += 3 {
		result += priorityOfBadge(rows[i], rows[i+1], rows[i+2])
	}
	fmt.Println(result)
}
