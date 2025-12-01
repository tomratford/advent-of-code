/*
Advent of code; Year YYYY, Day XX

Some notes on the challenge or solution here
   
usage:
   go run main.go path/to/input.txt
*/
package main

import (
	"fmt"
	"os"
	"strings"
	"strconv"
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

	p, err := Parse(string(input))
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Println(Part1(p))
	fmt.Println(Part2(p))
}

// Structs and types

type Instruction struct {
	Direction byte
	Distance int
} 

// Solution for Part 1 of the challenge
func Part1(input []Instruction) int {
	start := 50
	score := 0
	for _, i := range input {
		//fmt.Print(i)
		if i.Direction == 'L' {
			start = start - i.Distance
		} else if i.Direction == 'R' {
			start = start + i.Distance 
		} else {
			panic("This shouldn't happen")
		}

		for (start < 0) {
			start = 100 + start
		}
		for (start >= 100) {
			start = start - 100
		}
		//fmt.Print(" ")
		//fmt.Println(start)
		if start == 0 {
			score += 1
		}
	}
	return score
}

// Solution for Part 2 of the challenge
func Part2(input []Instruction) int {
	start := 50
	score := 0
	for _, i := range input {
		if i.Direction == 'L' {
			for range i.Distance {
				start -= 1
				if start%100 == 0 {
					score += 1
				} 
			}
		} else if i.Direction == 'R' {
			for range i.Distance {
				start += 1
				if start%100 == 0 {
					score += 1
				} 
			} 
		} else {
			panic("This shouldn't happen")
		}
	}
	return score
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([]Instruction, error) {
	lines := strings.Split(input, "\n")
	instructions := make([]Instruction, 0, len(lines))
	for _, l := range lines {
		if l == "" {
			continue
		}
		number := l[1:]
		num, err := strconv.Atoi(number)
		if err != nil {
			return []Instruction{}, fmt.Errorf("number couldn't parse in line %q: %v", l, err)
		}
		instructions = append(instructions, Instruction{l[0], num})
	}
	return instructions, nil
}
