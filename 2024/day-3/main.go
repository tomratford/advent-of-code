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
	"unicode"
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
func Part1(input string) int {
	rtn := 0
	for i := 0; i < len(input); i++ {
		if input[i] == 'm' {
			i++
			if input[i] == 'u' {
				i++
				if input[i] == 'l' {
					i++
					if input[i] == '(' {
						i++
						var fst_num_str strings.Builder
						for unicode.IsDigit(rune(input[i])) {
							fst_num_str.WriteByte(input[i])
							i++
						}
						if fst_num, err := strconv.Atoi(fst_num_str.String()); input[i] == ',' && err == nil {
							i++
							var snd_num_str strings.Builder
							for unicode.IsDigit(rune(input[i])) {
								snd_num_str.WriteByte(input[i])
								i++
							}
							if snd_num, err := strconv.Atoi(snd_num_str.String()); input[i] == ')' && err == nil {
								// fmt.Printf("mul(%d,%d)\n", fst_num, snd_num)
								rtn += fst_num * snd_num
							}
						}
					}
				}
			}
		}
	}
	return rtn
}

// Solution for Part 2 of the challenge
func Part2(input string) int {
	rtn := 0
	for i := 0; i < len(input); i++ {
		if dnt := strings.Index(input[i:], "don't()"); dnt == -1 { // No more don't
			rtn += Part1(input[i:])
			i = len(input)
		} else {
			rtn += Part1(input[i : i+dnt])
			if do := strings.Index(input[i+dnt:], "do()"); do == -1 {
				break
			} else {
				i += dnt + do
			}
		}
	}
	return rtn
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (string, error) {
	return input, nil
}
