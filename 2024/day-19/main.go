/*
Advent of code; Year YYYY, Day XX

# Some notes on the challenge or solution here

usage:

	go run main.go path/to/input.txt
*/
package main

import (
	"fmt"
	"os"
	"strings"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Println("usage: go run main.go path/to/input.txt")
		return
	}

	input, err := os.ReadFile(os.Args[1])
	if err != nil {
		fmt.Println(err)
		return
	}

	t, p, err := Parse(string(input))
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Println(Part1(t, p))
	fmt.Println(Part2(t, p))
}

// Structs and types

/* Any structs required for the challenge go here */

// Solution for Part 1 of the challenge
func Part1(patterns map[string]int, designs []string) int {
	rtn := 0
	for _, p := range designs {
		if Part1Inner(patterns, p) {
			rtn++
		}
	}
	return rtn
}

func Part1Inner(patterns map[string]int, design string) bool {
	if design == "" {
		return true
	}
	rtn := false
	for i := 1; i <= len(design); i++ {
		p := design[:i]
		if _, ok := patterns[p]; ok {
			rtn = rtn || Part1Inner(patterns, design[i:])
		}
	}
	return rtn
}

// Solution for Part 2 of the challenge
func Part2(patterns map[string]int, designs []string) int {
	rtn := 0
	for _, p := range designs {
		rtn += Part2Inner(patterns, p)
	}
	return rtn
}

var RESULTS = map[string]int{}

func Part2Inner(patterns map[string]int, design string) int {
	if v, ok := RESULTS[design]; ok {
		return v
	}
	if design == "" {
		return 1
	}
	rtn := 0
	for i := 1; i <= len(design); i++ {
		p := design[:i]
		if _, ok := patterns[p]; ok {
			x := Part2Inner(patterns, design[i:])
			RESULTS[design] += x
			rtn = rtn + x
		}
	}
	return rtn
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (map[string]int, []string, error) {
	sections := strings.Split(input, "\n\n")
	patterns := make(map[string]int)
	patterns_list := strings.Split(sections[0], ", ")
	for _, t := range patterns_list {
		patterns[t]++
	}
	designs := strings.Split(sections[1], "\n")
	return patterns, designs, nil
}
