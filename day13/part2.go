package day13

import (
	"sort"

	"github.com/thomasschafer/advent_of_code_2022/utils"
)

type Packet struct {
	data      DeepList
	isDivider bool
}

func decoderKey(rows []string) int {
	var packets []Packet = []Packet{{data: DeepList{2}, isDivider: true}, {data: DeepList{6}, isDivider: true}}
	for _, row := range rows {
		if row != "" {
			packets = append(packets, Packet{data: parseListWithCheck(row), isDivider: false})
		}
	}
	sort.SliceStable(packets, func(i, j int) bool {
		return compare(packets[i].data, packets[j].data) < 0
	})
	result := 1
	for idx, packet := range packets {
		if packet.isDivider {
			result *= idx + 1
		}
	}
	return result
}

func Part2(filePath string) int {
	rows := utils.RowsFromFile(filePath)
	return decoderKey(rows)
}
