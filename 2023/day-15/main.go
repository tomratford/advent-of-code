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

type lens struct {
	label string
	focal int
}

// Solution for Part 1 of the challenge
func Part1(input []string) int {
	rtn := 0
	for _, w := range input {
		rtn += inner(w)
	}
	return rtn
}

func inner(input string) int {
	rtn := 0
	for _, r := range input {
		rtn += int(r)
		rtn *= 17
		rtn = rtn % 256
	}
	return rtn
}

// Solution for Part 2 of the challenge
func Part2(input []string) int {
	rtn := make(map[int][]lens)

	for _, s := range input {
		op_i := strings.IndexAny(s, "-=")

		op := s[op_i]

		key := s[0:op_i]
		box := inner(key)

		switch op {
		case '=':
			focal, err := strconv.Atoi(s[op_i+1:])
			if err != nil {
				panic(err)
			}
			if _, ok := rtn[box]; ok {
				lbl_i := slices.IndexFunc(rtn[box], func(a lens) bool {
					return a.label == key
				})
				if lbl_i != -1 {
					rtn[box][lbl_i] = lens{key, focal}
				} else {
					rtn[box] = append(rtn[box], lens{key, focal})
				}
			} else {
				rtn[box] = []lens{{key, focal}}
			}
		case '-':
			lbl_i := slices.IndexFunc(rtn[box], func(a lens) bool {
				return a.label == key
			})
			if lbl_i != -1 {
				rtn[box] = append(rtn[box][:lbl_i], rtn[box][lbl_i+1:]...)
			}
		}
	}

	rtn2 := 0
	for k, v := range rtn {
		for i, x := range v {
			rtn2 += (k + 1) * (i + 1) * x.focal
		}
	}

	return rtn2
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([]string, error) {
	return strings.Split(strings.ReplaceAll(input, "\n", ""), ","), nil
}
