package utils

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
