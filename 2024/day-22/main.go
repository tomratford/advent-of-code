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

	x := 123
	// first step
	mix := x
	x *= 1 << 6
	mix = x ^ mix
	x = mix % (1 << 24)
	// snd step
	mix = x
	x /= (1 << 5)
	mix = x ^ mix
	x = mix % (1 << 24)
	// 3rd step
	mix = x
	x *= (1 << 11)
	mix = x ^ mix
	x = mix % (1 << 24)
	fmt.Println(x)
	fmt.Println(p)
}

// Structs and types

/* Any structs required for the challenge go here */

// Solution for Part 1 of the challenge
func Part1(input string) int {
	return 1
}

// Solution for Part 2 of the challenge
func Part2(input string) int {
	return 1
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (string, error) {
	return input, nil
}
