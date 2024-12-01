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
	"slices"
	"strconv"
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

	p, err := Parse(string(input))
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Println(Part1(p))
	fmt.Println(Part2(p))
}

// Structs and types

/* Any structs required for the challenge go here */

// Solution for Part 1 of the challenge
func Part1(input [2][]int) int {
	left := input[0]
	right := input[1]
	slices.Sort(left)
	slices.Sort(right)
	rtn := 0
	for i := range left {
		diff := left[i] - right[i]
		if diff < 0 {
			rtn -= diff
		} else {
			rtn += diff
		}
	}
	return rtn
}

// Solution for Part 2 of the challenge
func Part2(input [2][]int) int {
	left := input[0]
	right := input[1]
	right_map := make(map[int]int)
	for _, r := range right {
		right_map[r]++
	}
	rtn := 0
	for _, l := range left {
		if v, ok := right_map[l]; ok {
			rtn += l * v
		}
	}
	return rtn
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([2][]int, error) {
	lines := strings.Split(input, "\n")
	left := make([]int, 0, len(lines))
	right := make([]int, 0, len(lines))
	for _, l := range lines {
		if l == "" {
			continue
		}
		numbers := strings.Split(l, "   ")
		left_i, err := strconv.Atoi(numbers[0])
		if err != nil {
			return [2][]int{}, fmt.Errorf("left number couldn't parse in line %q: %v", l, err)
		}
		right_i, err := strconv.Atoi(numbers[1])
		if err != nil {
			return [2][]int{}, fmt.Errorf("right number couldn't parse: %v", err)
		}
		left = append(left, left_i)
		right = append(right, right_i)
	}
	return [2][]int{left,right}, nil
}
