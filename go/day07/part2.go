package day07

import (
	"math"

	"github.com/thomasschafer/advent_of_code_2022/utils"
)

func spaceUsed(root *DirectoryOrFile) int {
	totalSize := 0
	for _, child := range root.children {
		if child.isDirectory {
			totalSize += spaceUsed(child)
		} else {
			totalSize += child.size
		}
	}
	return totalSize
}

func findSmallestDirToDelete(root *DirectoryOrFile, diskSpaceNeeded int) (int, int) {
	totalSize := 0
	sizeOfSmallestDirToDelete := math.MaxInt64
	for _, child := range root.children {
		if child.isDirectory {
			totalChildSize, childSizeOfSmallestDirToDelete := findSmallestDirToDelete(child, diskSpaceNeeded)
			for _, size := range []int{totalChildSize, childSizeOfSmallestDirToDelete} {
				if size < sizeOfSmallestDirToDelete && size >= diskSpaceNeeded {
					sizeOfSmallestDirToDelete = size
				}
			}
			totalSize += totalChildSize
		} else {
			totalSize += child.size
		}
	}
	return totalSize, sizeOfSmallestDirToDelete
}

func sizeOfSmallestDirToBeDeleted(root DirectoryOrFile, totalDiskSpace int, requiredUnusedSpace int) int {
	totalDiskSpaceUsed := spaceUsed(&root)
	diskSpaceNeeded := requiredUnusedSpace - (totalDiskSpace - totalDiskSpaceUsed)
	if diskSpaceNeeded <= 0 {
		return 0
	}
	rootSize, sizeOfSmallestDirToDelete := findSmallestDirToDelete(&root, diskSpaceNeeded)
	if rootSize < sizeOfSmallestDirToDelete && rootSize >= diskSpaceNeeded {
		sizeOfSmallestDirToDelete = rootSize
	}
	return sizeOfSmallestDirToDelete

}

func Part2(filePath string) int {
	rows := utils.RowsFromFile(filePath)
	directoryTree := buildDirectoryTree(rows)
	printDirectoryTree(*directoryTree)
	return sizeOfSmallestDirToBeDeleted(*directoryTree, 70000000, 30000000)
}
