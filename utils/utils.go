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

func PanicIfErr[T any](val T, err error) T {
	if err != nil {
		panic(err)
	}
	return val
}

func Max[T constraints.Ordered](initial T, rest ...T) T {
	max := initial
	for _, t := range rest {
		if t > max {
			max = t
		}
	}
	return max
}

func Abs[T constraints.Integer | constraints.Float](t T) T {
	if t > 0 {
		return t
	}
	return -t
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
