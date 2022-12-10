package utils

import (
	"fmt"
	"os"
	"sort"
	"strings"

	"golang.org/x/exp/constraints"
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

func GetIfExists[K comparable, V any](dict map[K]V, key K) V {
	value, ok := dict[key]
	if !ok {
		panic(fmt.Sprintf("Could not find key %v in map dict %v", key, dict))
	}
	return value
}

func Expect[T any](val T, err error) T {
	if err != nil {
		panic(err)
	}
	return val
}

func Max[T constraints.Ordered](args ...T) T {
	if len(args) == 0 {
		panic("Not enough arguments")
	}
	max := args[0]
	for _, t := range args {
		if t > max {
			max = t
		}
	}
	return max
}

func Min[T constraints.Ordered](args ...T) T {
	if len(args) == 0 {
		panic("Not enough arguments")
	}
	min := args[0]
	for _, t := range args {
		if t < min {
			min = t
		}
	}
	return min
}

func Abs[T constraints.Integer | constraints.Float](t T) T {
	if t > 0 {
		return t
	}
	return -t
}

func Sum[T constraints.Integer | constraints.Float](arr []T) T {
	if len(arr) == 0 {
		panic("arr should contain at least 1 element")
	}
	var curSum T = 0
	for _, x := range arr {
		curSum += x
	}
	return curSum
}

func MaxKeyFromMap[K constraints.Ordered, V any](dict map[K]V) K {
	var maxKey K
	for key := range dict {
		maxKey = Max(maxKey, key)
	}
	return maxKey
}

func Reverse[T comparable](s []T) {
	sort.SliceStable(s, func(i, j int) bool {
		return i > j
	})
}

func ArrayToSet[T comparable](array []T) map[T]bool {
	result := make(map[T]bool)
	for _, x := range array {
		result[x] = true
	}
	return result
}
