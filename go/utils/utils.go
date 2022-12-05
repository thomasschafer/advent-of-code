package utils

import (
	"os"
	"strings"
)

func RowsFromFile(filePath string) []string {
	b, err := os.ReadFile(filePath)
	if err != nil {
		panic(err)
	}

	data := string(b)
	rows := strings.Split(data, "\n")
	return rows
}
