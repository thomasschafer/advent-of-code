package day7

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/thomasschafer/advent_of_code_2022/utils"
)

type DirectoryOrFile struct {
	name        string
	isDirectory bool
	parent      *DirectoryOrFile
	children    []*DirectoryOrFile // Only applicable when isDirectory
	size        int                // Only applicable when not isDirectory
}

// Cases to handle:
// $ cd into directory
// $ cd back (..)
// $ ls, then files shown

func buildDirectoryTree(rows []string) *DirectoryOrFile {
	root := DirectoryOrFile{name: "/", isDirectory: true}
	curDir := &root
	rowIdx := 0
	for rowIdx < len(rows) {
		row := rows[rowIdx]
		fmt.Printf("1 row --------- %v ---------\n", row)
		if string(row[0]) == "$" {
			if string(row[2:4]) == "cd" {
				if string(row[5:]) == "/" {
					curDir = &root
				} else if string(row[5:]) == ".." {
					curDir = curDir.parent
					fmt.Printf("cd ..: Moved back to %v\n", curDir.name)
				} else {
					// Create child, move curDir to point to child and update parent.children
					nameOfDirToMoveTo := string(row[5:])
					foundDir := false
					for _, dir := range curDir.children {
						if dir.name == nameOfDirToMoveTo {
							curDir = dir
							fmt.Printf("cd <dir>: Moved to %v (%v)\n", curDir.name, nameOfDirToMoveTo)
							foundDir = true
							break
						}
					}
					if !foundDir {
						panic(fmt.Sprintf("Could not find directory with name %v.", nameOfDirToMoveTo))
					}
				}
			} else if string(row[2:4]) == "ls" {
				for rowIdx+1 < len(rows) && string(rows[rowIdx+1][0]) != "$" {
					rowIdx += 1
					fmt.Printf("2 row --------- %v ---------\n", string(rows[rowIdx]))
					rowSplit := strings.Split(string(rows[rowIdx]), " ")
					isDirectory := rowSplit[0] == "dir"
					child := DirectoryOrFile{name: rowSplit[1], isDirectory: isDirectory, parent: curDir}
					if !isDirectory {
						child.size = utils.Expect(strconv.Atoi(rowSplit[0]))
					}
					curDir.children = append(curDir.children, &child)
					fmt.Printf("ls: Added child %v to dir %v\n", child.name, curDir.name)
				}
			} else {
				panic(fmt.Sprintf("Unkown command %v", string(row)))
			}
		}
		rowIdx += 1
	}
	return &root
}

func printDirectoryTreeHelper(rootDir *DirectoryOrFile, prefix string) {
	if !rootDir.isDirectory {
		panic(fmt.Sprintf("Only directories should be passed in as rootDir, received %v", rootDir.name))
	}
	fmt.Printf("%v- %v (dir)\n", prefix, rootDir.name)
	for _, child := range rootDir.children {
		if child.isDirectory {
			printDirectoryTreeHelper(child, prefix+"  ")
		} else {
			fmt.Printf("%v- %v (file, size=%v)\n", prefix+"  ", child.name, child.size)
		}
	}
}

func printDirectoryTree(rootDir DirectoryOrFile) {
	fmt.Println("----- Directory tree: -----")
	printDirectoryTreeHelper(&rootDir, "")
}

func sumOfSizesOfDirsMaxSizeHelper(root *DirectoryOrFile, maxSize int) (int, int) {
	totalSize := 0
	sizeOfDirsMaxSize := 0
	for _, child := range root.children {
		if child.isDirectory {
			childTotalSize, childSizeOfDirsMaxSize := sumOfSizesOfDirsMaxSizeHelper(child, maxSize)
			totalSize += childTotalSize
			sizeOfDirsMaxSize += childSizeOfDirsMaxSize
			if childTotalSize <= maxSize {
				sizeOfDirsMaxSize += childTotalSize
			}
		} else {
			totalSize += child.size
		}
	}
	return totalSize, sizeOfDirsMaxSize
}

func sumOfSizesOfDirsMaxSize(root DirectoryOrFile, maxSize int) int {
	_, sizeOfDirsMaxSize := sumOfSizesOfDirsMaxSizeHelper(&root, maxSize)
	return sizeOfDirsMaxSize
}

func Part1(filePath string) int {
	rows := utils.RowsFromFile(filePath)
	directoryTree := buildDirectoryTree(rows)
	printDirectoryTree(*directoryTree)
	sizeOfDirsMaxSize := sumOfSizesOfDirsMaxSize(*directoryTree, 100000)
	return sizeOfDirsMaxSize
}
