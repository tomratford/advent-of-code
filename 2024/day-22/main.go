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
func Part1(input []int) int {
	rtn := 0
	for _, i := range input {
		rtn += Calc2000thNumber(i)
	}
	return rtn
}

func f(x int) int {
	x = (x ^ x<<6) % (1 << 24)
	x = (x ^ x>>5) % (1 << 24)
	return (x ^ x<<11) % (1 << 24)
}

var SEQUENCES = map[int][]int{}

func Calc2000thNumber(x int) int {
	store := x
	seq := make([]int, 2000)
	for i := 0; i < 2000; i++ {
		x = f(x)
		seq[i] = x
	}
	SEQUENCES[store] = seq
	return x
}

// Solution for Part 2 of the challenge
var FOURS = make(map[[4]int]int)

func Part2(input []int) int {
	for _, n := range input {
		seen := make(map[[4]int]int)
		sequence := SEQUENCES[n]
		for i := 4; i < 2000; i++ {
			last4 := [4]int{sequence[i-4]%10 - sequence[i-3]%10,
				sequence[i-3]%10 - sequence[i-2]%10,
				sequence[i-2]%10 - sequence[i-1]%10,
				sequence[i-1]%10 - sequence[i]%10,
			}
			if _, ok := seen[last4]; !ok {
				seen[last4]++
				FOURS[last4] += sequence[i] % 10
			}
		}
	}
	max := 0
	for _, v := range FOURS {
		if v > max {
			max = v
		}
	}
	return max
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([]int, error) {
	lines := strings.Split(input, "\n")
	rtn := make([]int, 0, len(lines))
	for _, l := range lines {
		if l == "" {
			continue
		}
		if n, err := strconv.Atoi(l); err != nil {
			return []int{}, fmt.Errorf("couldn't parse %q: %v", l, err)
		} else {
			rtn = append(rtn, n)
		}
	}
	return rtn, nil
}
