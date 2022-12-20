package main

import (
	"fmt"
	"log"
	"sort"
	"strconv"

	"github.com/alixq/advent-of-code-2022/utils"
)

func main() {
	lines := utils.ReadFileLines("input.txt")
	result1 := computeMostCalories(lines)
	fmt.Printf("%v\n", result1)

	result2 := computeTopThreeMostCalories(lines)
	fmt.Printf("%v\n", result2)
}

func computeMostCalories(lines []string) int {
	maxElfCalories, currentElfCalories := 0, 0

	for _, line := range lines {
		if line == "" {
			if currentElfCalories > maxElfCalories {
				maxElfCalories = currentElfCalories
			}
			currentElfCalories = 0
			continue
		}
		calories, err := strconv.Atoi(line)
		if err != nil {
			log.Fatalf("error parsing number: %v", err)
		}
		currentElfCalories += calories
	}

	return maxElfCalories
}

func computeTopThreeMostCalories(lines []string) int {
	currentElfCalories := 0
	caloriesByElves := []int{}

	for _, line := range lines {
		if line == "" {
			caloriesByElves = append(caloriesByElves, currentElfCalories)
			currentElfCalories = 0
			continue
		}
		calories, err := strconv.Atoi(line)
		if err != nil {
			log.Fatalf("error parsing number: %v", err)
		}
		currentElfCalories += calories
	}

	sort.Ints(caloriesByElves)
	lastThreeSelector, totalCal := len(caloriesByElves) - 3, 0
	for _, cal := range caloriesByElves[lastThreeSelector:] {
		totalCal += cal
	}
	return totalCal
}
