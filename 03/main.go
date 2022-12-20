package main

import (
	"fmt"
	"strings"

	"github.com/alixq/advent-of-code-2022/utils"
)

const alphabet = "abcdefghijklmnopqrstuvwxyz"
var fullAlphabet = alphabet + strings.ToUpper(alphabet)

func main() {
	lines := utils.ReadFileLines("input.txt")
	result1 := computeTotalPriority(lines)
	fmt.Printf("%v\n", result1)

	result2 := computeBadgeTotalPriority(lines)
	fmt.Printf("%v\n", result2)
}

func computeTotalPriority(lines []string) (points int) {
	for _, line := range lines {
		if len(line) % 2 != 0 {
			panic("Should have even number of items")
		}
		size := len(line) / 2
		match := findMatch(line[:size], line[size:])
		points += mapToPriorityCode(match)
	}

	return points
}

func findMatch(a string, b string) string {
	for _, char1 := range a {
		for _, char2 := range b {
			if (char1 == char2) {
				return string(char1)
			}
		}
	}
	panic("No element in common!")
}

func mapToPriorityCode(char string) (prio int) {
	for ind, alphaByte := range fullAlphabet {
		if char == string(alphaByte) {
			return ind + 1
		}
	}
	panic("No priority found!")
}

func computeBadgeTotalPriority(lines []string) (prio int) {
	for _, group := range groupRucksacks(lines) {
		match := findMatchBis(group[0], group[1], group[2])
		prio += mapToPriorityCode(match)
	}
	return prio
}

func groupRucksacks(lines []string) (groups [][]string) {
	if len(lines) % 3 != 0 {
		panic("We should have only groups of 3")
	}

	for i := 3; i <= len(lines); i += 3 {
    groups = append(groups, lines[i - 3:i])
  }

	return groups
}

func findMatchBis(a string, b string, c string) string {
	for _, char1 := range a {
		for _, char2 := range b {
			for _, char3 := range c {
				if (char1 == char2 && char1 == char3) {
					return string(char1)
				}
			}
		}
	}
	panic("No element in common!")
}
