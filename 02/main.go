package main

import (
	"fmt"

	"github.com/alixq/advent-of-code-2022/utils"
)

func main() {
	lines := utils.ReadFileLines("input.txt")
	result1 := retrievePoints(lines)
	fmt.Printf("%v\n", result1)

	result2 := retrievePointsBis(lines)
	fmt.Printf("%v\n", result2)
}

func retrievePoints(lines []string) (points int) {
	for _, line := range lines {
		points += retrieveWinningPoints(line)
		points += mapToPoints(string(line[2]))
	}

	return points
}

func mapToPoints(char string) int {
	switch char {
	case "A", "X":
		return 1
	case "B", "Y":
		return 2
	case "C", "Z":
		return 3
	default:
		panic(fmt.Sprintf("Unexpected letter: %v", char))
	}
}

func retrieveWinningPoints(round string) (points int) {
	switch round {
	case "A X", "B Y", "C Z":
		return 3
	case "A Y", "B Z", "C X":
		return 6
	case "A Z", "B X", "C Y":
		return 0
	default:
		panic(fmt.Sprintf("Unexpected round: %v", round))
	}
}


func retrievePointsBis(lines []string) (points int) {
	for _, line := range lines {
		hand := computeHand(line)
		points += mapToPoints(hand)
		points += retrieveWinningPoints(line[0:2] + hand)
	}

	return points
}

func computeHand(round string) (hand string) {
	switch round {
	case "A X", "B Z", "C Y":
		return "Z"
	case "B X", "C Z", "A Y":
		return "X"
	case "C X", "A Z", "B Y":
		return "Y"
	default:
		panic(fmt.Sprintf("Unexpected round: %v", round))
	}
}
