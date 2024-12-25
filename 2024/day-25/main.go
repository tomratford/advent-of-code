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

	schs, keys, err := Parse(string(input))
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Println(Part1(schs, keys))
}

// Structs and types

/* Any structs required for the challenge go here */

// Solution for Part 1 of the challenge
func Part1(schs []map[complex128]int, keys []map[complex128]int) int {
	rtn := len(schs) * len(keys)
	for _, sch := range schs {
		for _, key := range keys {
			for k := range key {
				if _, ok := sch[k]; ok {
					rtn--
					break
				}
			}
		}
	}
	return rtn
}

// Solution for Part 2 of the challenge
func Part2(input string) int {
	return 1
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([]map[complex128]int, []map[complex128]int, error) {
	objects := strings.Split(input, "\n\n")
	schematics := make([]map[complex128]int, 0, len(objects))
	keys := make([]map[complex128]int, 0, len(objects))
	for _, o := range objects {
		x := 0.0
		y := 0.0
		obj := make(map[complex128]int)
		for _, r := range o {
			if r == '\n' {
				y++
				x = 0
				continue
			}
			if r == '#' {
				obj[complex(x, y)]++
			}
			x++
		}
		if strings.HasPrefix(o, "#") {
			schematics = append(schematics, obj)
		} else {
			keys = append(keys, obj)
		}
	}

	return schematics, keys, nil
}
