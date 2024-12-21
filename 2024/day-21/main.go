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

	fmt.Println(p)
}

// Structs and types

// I think I can be somewhat cheeky and hardcode the best possible paths
// between two points on the pads.
// This saves me having to solve that using programming (hard) when my eyes can
// clearly do this a lot better.
// I bet part 2 makes it so certain routes break or something though.
// Nightmare.

var DirpadMoves = map[string][]string{
	"<<": []string{},
	"<v": []string{">"},
	"<>": []string{">>"},
	"<^": []string{">^"},
	"<A": []string{">^>", ">>^"},
	"v<": []string{"<"},
	"vv": []string{},
	"v>": []string{">"},
	"v^": []string{"^"},
	"vA": []string{"^>", ">^"},
	"><": []string{"<<"},
	">v": []string{"<"},
	">>": []string{},
	">^": []string{"<^", "^<"},
	">A": []string{"^"},
	"^<": []string{"v<"},
	"^v": []string{"v"},
	"^>": []string{"v>", ">v"},
	"^^": []string{},
	"^A": []string{">"},
	"A<": []string{"v<<", "<v<"},
	"Av": []string{"<v", "v<"},
	"A>": []string{"v"},
	"A^": []string{"<"},
	"AA": []string{},
}

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
