// Package aoc has helpers your solutions can share: import "solutions/aoc" to use them, as in
// aoc.Ints(input), and add your own.
package aoc

import (
	"regexp"
	"strconv"
	"strings"

	"golang.org/x/exp/constraints"
)

var integer = regexp.MustCompile(`-?\d+`)

// Ints is every integer in the text, including negative ones: Ints("x=-3, y=12") is [-3 12].
func Ints(text string) []int {
	matches := integer.FindAllString(text, -1)
	ints := make([]int, len(matches))
	for i, match := range matches {
		n, err := strconv.Atoi(match)
		if err != nil {
			panic(err)
		}
		ints[i] = n
	}
	return ints
}

// Point is a position in a grid, by row and column.
type Point struct{ R, C int }

// Grid is a grid of characters by position, from lines of text.
func Grid(text string) map[Point]rune {
	cells := map[Point]rune{}
	for r, line := range strings.Split(text, "\n") {
		for c, ch := range []rune(line) {
			cells[Point{r, c}] = ch
		}
	}
	return cells
}

// Neighbours4 is the positions above, below, left and right.
func Neighbours4(p Point) []Point {
	return []Point{{p.R - 1, p.C}, {p.R + 1, p.C}, {p.R, p.C - 1}, {p.R, p.C + 1}}
}

// Neighbours8 is the positions around, diagonals included.
func Neighbours8(p Point) []Point {
	var around []Point
	for dr := -1; dr <= 1; dr++ {
		for dc := -1; dc <= 1; dc++ {
			if dr != 0 || dc != 0 {
				around = append(around, Point{p.R + dr, p.C + dc})
			}
		}
	}
	return around
}

// Sum is the numbers added up. It panics if there are none.
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

// Rows is the lines of the text, without the newlines at its end.
func Rows(text string) []string {
	return strings.Split(strings.TrimRight(text, "\n"), "\n")
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

func MaxKeyFromMap[K constraints.Ordered, V any](dict map[K]V) K {
	var maxKey K
	for key := range dict {
		maxKey = Max(maxKey, key)
	}
	return maxKey
}

type MaxLengthArray struct {
	data      []int
	maxLength int
}

func NewMaxLengthArray(maxLength int) *MaxLengthArray {
	return &MaxLengthArray{maxLength: maxLength}
}

func (arr *MaxLengthArray) GetData() []int {
	return arr.data
}

func (arr *MaxLengthArray) AppendIfLargeEnough(value int) {
	if len(arr.data) < arr.maxLength {
		arr.data = append(arr.data, value)
	} else {
		minVal := Min(arr.data...)
		if value > minVal {
			newData := []int{}
			removedMin := false
			for _, x := range arr.data {
				if x == minVal && !removedMin {
					removedMin = true
				} else {
					newData = append(newData, x)
				}
			}
			newData = append(newData, value)
			arr.data = newData
		}
	}
}
