# TODO: Rewrite in Go, and replace MaxLengthArray with max heap

class MaxLengthArray:
    def __init__(self, max_length=3):
        self.data = []
        self.max = max_length

    def add_if_large_enough(self, value):
        if len(self.data) < self.max:
            self.data.append(value)
        else:
            min_val = min(self.data)
            if value > min_val:
                new_data = []
                removed_min = False
                for x in self.data:
                    if x == min_val and not removed_min:
                        removed_min = True
                    else:
                        new_data.append(x)
                new_data.append(value)
                self.data = new_data

def calculate_max_calories(file_name: str) -> int:
    with open(file_name, "r") as file:
        data = file.read()
    rows = data.split("\n")

    maxTotalCalories = MaxLengthArray()
    currentTotalCalories = 0

    for calories in rows:
        if calories == "":
            maxTotalCalories.add_if_large_enough(currentTotalCalories)
            currentTotalCalories = 0
        else:
            currentTotalCalories += int(calories)
    return sum(maxTotalCalories.data)


if __name__ == "__main__":
    print(calculate_max_calories("../data/day_1.txt"))
