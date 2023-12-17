/*
Advent of code; Year 2023, Day 12

Inital shit permutation solution

usage:

	go run main.go path/to/input.txt
*/
package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"slices"
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
}

// Structs and types

const (
	BROKEN = 1
	UNKNOWN = 0
	EMPTY = -1
)

type Row struct {
	line []int
	sizes []int
}

// Solution for Part 1 of the challenge
func Part1(input []Row) int {
	score := 0
	for _, r := range input {
		perms := getAllPerms(r.line)
		valid := 0
		for _, p := range perms {
			if isValid(p, r.sizes) {
				valid += 1
			}
		}
		score += valid
	}
	return score
}

func getAllPerms(input []int) [][]int {
	start := make([][]int,0,2^(len(input)))
	start = append(start, input)
	for {
		temps := make([][]int, 0, 2^(len(start) - 1))
		for _, xs := range start {
			for i, x := range xs {
				if x == UNKNOWN {
					copy := slices.Clone(xs)
					xs[i] = BROKEN
					copy[i] = EMPTY
					temps = append(temps, copy)
				}
			}
		}
		if len(temps) == 0 {
			return start
		} else {
			start = append(start, temps...)
		}
	}
}

func isValid(xs, ns []int) bool {
	temp := []int{}
	i := 0
	// Append a final empty to ensure we check all our loops
	for _, x := range xs {
		switch x {
		case BROKEN:
			temp = append(temp, x)
		case EMPTY:
			// Ensure we at least have 1 element in temp
			if len(temp) != 0 {
				// Guard going too far if that happens
				if i >= len(ns) {
					return false
				}
				if len(temp) == ns[i] {
					temp = []int{}
					i += 1
				} else {
					return false
				} 
			}
		default:
			fmt.Printf("Bad input: %d\n", x)
			return false
		}
	}
	if len(temp) != 0 {
		// Guard going too far if that happens
		if i != len(ns)-1 {
			return false
		}			
		if len(temp) == ns[i] {
			return true
		} else {
			return false
		} 
	}	
	if i == len(ns) {
		return true
	}
	return false
}

// Solution for Part 2 of the challenge
func Part2(input string) int {
	return 1
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([]Row, error) {
	lines := strings.Split(input, "\n")
	nums := make([]Row, len(lines)-1)
	for i, line := range lines[:len(lines)-1] {
		xs, ns, err := ParseLine(line)
		if err != nil {
			return []Row{}, err
		}
		nums[i] = Row{xs, ns}
	}
	return nums, nil
}

func ParseLine(input string) ([]int, []int, error) {
	part, req, found := strings.Cut(input, " ")
	if !found {
		return []int{}, []int{}, fmt.Errorf("no space found in line")
	}
	xs := make([]int, len(part))
	for i, c := range part {
		switch c {
		case '#':
			xs[i] = BROKEN
		case '?':
			xs[i] = UNKNOWN
		case '.':
			xs[i] = EMPTY
		}
	}

	nums_str := strings.Split(req, ",")
	ns := make([]int, len(nums_str))
	for i, a := range nums_str {
		x, err := strconv.Atoi(a)
		if err != nil {
			return []int{}, []int{}, err
		}
		ns[i] = x
	}

	return xs, ns, nil
}
