package main

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/alixq/advent-of-code-2022/utils"
)

func main() {
	lines := utils.ReadFileLines("input.txt")
	result1 := getFullSubSetCount(lines)
	fmt.Printf("%v\n", result1)

	result2 := getOverlapCount(lines)
	fmt.Printf("%v\n", result2)
}

func getFullSubSetCount(lines []string) (count int) {
	for _, line := range lines {
		a, b := retrievePair(line)
		aContainsB := a[0] <= b[0] && a[1] >= b[1]
		bContainsA := b[0] <= a[0] && b[1] >= a[1]
		if (aContainsB || bContainsA) {
			count += 1
		}
	}

	return count
}

func retrievePair(line string) (a [2]int, b [2]int) {
	pair := strings.Split(line, ",")
	if (len(pair) != 2) {
		panic(fmt.Sprintf("Not 2 ranges: %v", line))
	}
	return retrieveBounds(pair[0]), retrieveBounds(pair[1])
}

func retrieveBounds(stringRange string) (bounds [2]int) {
	stringBounds := strings.Split(stringRange, "-")
	if (len(stringBounds) != 2) {
		panic(fmt.Sprintf("Not 2 bounds: %v", stringBounds))
	}

	for i, bound := range stringBounds {
		n, err := strconv.Atoi(bound)
		if err != nil {
			panic(fmt.Sprintf("Invalid int: %v", bound))
		}
		bounds[i] = n
	}

	return bounds
}

func getOverlapCount(lines []string) (count int) {
	for _, line := range lines {
		a, b := retrievePair(line)
		bOverlapsA := a[0] <= b[0] && b[0] <= a[1]
		aOverlapsB := b[0] <= a[0] && a[0] <= b[1]
		if (bOverlapsA || aOverlapsB) {
			count += 1
		}
	}

	return count
}
