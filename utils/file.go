package utils

import (
	"bufio"
	"log"
	"os"
)

func ReadFileLines(path string) []string {
	file := OpenFile(path)
	defer file.Close()
	fileScanner := bufio.NewScanner(file)
	fileScanner.Split(bufio.ScanLines)
	var lines []string
	for fileScanner.Scan() {
		lines = append(lines, fileScanner.Text())
	}
	return lines
}

func OpenFile(path string) (file *os.File) {
	file, err := os.Open(path)
	if err != nil {
		log.Fatalf("error opening file: %v", err)
	}
	return file
}
