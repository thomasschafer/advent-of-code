package utils

import (
	"fmt"
	"os"
	"strings"
)

func RowsFromFile(file_path string) []string {
	b, err := os.ReadFile(file_path)
	if err != nil {
		panic(err)
	}

	data := string(b)
	rows := strings.Split(data, "\n")
	return rows
}

func GetIfExists[K comparable, V any](dict map[K]V, key K) V {
	value, ok := dict[key]
	if !ok {
		panic(fmt.Sprintf("Could not find key %v in map dict %v", key, dict))
	}
	return value
}
