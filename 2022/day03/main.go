package main

import (
	"fmt"
	"io"
	"os"

	"solutions/aoc"
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

func part1(input string) int {
	rows := aoc.Rows(input)
	result := 0
	for _, row := range rows {
		items1 := row[:len(row)/2]
		items2 := row[len(row)/2:]
		result += sharedItemPriority(items1, items2)
	}
	return result
}

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

func part2(input string) int {
	rows := aoc.Rows(input)
	result := 0
	for i := 0; i+2 < len(rows); i += 3 {
		result += priorityOfBadge(rows[i], rows[i+1], rows[i+2])
	}
	return result
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
