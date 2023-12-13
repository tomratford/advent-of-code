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
func Part1(input [][]int) int {
	val := 0
	for _, line := range input {
		val += solve1Line(line)
	}
	return val
}

func solve1Line (s []int) int {
	if allZero(s) {
		return 0
	} else {
		d := getDiff(s)
		return s[len(s)-1] + solve1Line(d)
	}
}

func getDiff(s []int) []int {
	if len(s) == 1 {
		return s
	}
	
	diffs := make([]int, len(s)-1)
	for i:=0;i<len(s)-1;i++ {
		diffs[i] = s[i+1] - s[i]
	}
	return diffs
}

func allZero(s []int) bool {
	return !slices.ContainsFunc(s, func(i int) bool {
		return i != 0
	})
}

// Solution for Part 2 of the challenge
func Part2(input [][]int) int {
	val := 0
	for _, line := range input {
		val += solve2Line(line)
	}
	return val
}

func solve2Line (s []int) int {
	if allZero(s) {
		return 0
	} else {
		d := getDiff(s)
		return s[0] - solve2Line(d)
	}
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([][]int, error) {
	lines := strings.Split(input, "\n")
	nums := make([][]int, len(lines)-1)
	for i, line := range lines[:len(lines)-1] {
		ns, err := LineToList(line)
		if err != nil {
			return [][]int{}, err
		}
		nums[i] = ns
	}
	return nums, nil
}

func LineToList(input string) ([]int, error) {
	nums_str := strings.Split(input, " ")
	nums := make([]int, len(nums_str))
	for i, raw := range nums_str {
		n, err := strconv.Atoi(raw)
		if err != nil {
			return []int{}, err
		}
		nums[i] = n
	}
	return nums, nil
}
