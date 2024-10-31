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

	p, n, err := Parse(string(input))
	if err != nil {
		fmt.Println(err)
		return
	}

	//fmt.Println(part1_inner(p[2], n[2]))
	fmt.Println(Part1(p, n))
}

// Structs and types

/* Any structs required for the challenge go here */

// Solution for Part 1 of the challenge
func Part1(input []string, line_lens []int) int {
	rtn := 0
	for i := range input {
		fmt.Println(i, part1_inner(input[i], line_lens[i]))
		rtn += part1_inner(input[i], line_lens[i])
	}
	return rtn
}

func part1_inner(input string, line_len int) int {
	number_of_lines := len(input) / line_len
	if vertical := findVertical(input, line_len); vertical != -1 {
		// fmt.Println(vertical)
		return vertical * 100
	}
	if vertical2 := findVertical(rotate(rotate(input, line_len))); vertical2 != -1 {
		// fmt.Println(vertical2)
		return (number_of_lines - vertical2) * 100
	}
	if horizontal := findVertical(rotate(input, line_len)); horizontal != -1 {
		// fmt.Println(horizontal)
		return line_len - horizontal
	}
	if horizontal2 := findVertical(rotate(rotate(rotate(input, line_len)))); horizontal2 != -1 {
		// fmt.Println(horizontal2)
		return horizontal2
	}
	fmt.Println("No sol found")
	return 0
}

// Finds a vertical reflection across a horizontal line
func findVertical(input string, n int) int {
	i := 0
	j := len(input)
	m := n
	for {
		if i+m > len(input) || j-m < 0 {
			return -1
		}
		left := input[i : i+m]
		var rb strings.Builder
		for k := 1; k <= m/n; k++ {
			if j-m < 0 {
				return -1
			}
			rb.WriteString(input[j-k*n : j-((k-1)*n)])
		}
		right := rb.String()
		// fmt.Println(left, right)
		if left != right {
			m = n
			i += n
		}
		if left == right {
			m += n
			if i+m == len(input) {
				return ((i / n) + (j / n)) / 2 // midpoint
			}
		}
	}
}

// Returns 90 degrees anticlockwise rotated string and new line length
func rotate(input string, n int) (string, int) {
	n_rows := len(input) / n // Old number of columns
	n_cols := n              // Old number of rows
	new := make([]string, len(input))
	for i := 0; i < n_cols; i++ { // For each new column
		for j := 0; j < n_rows; j++ { // For each new row
			new[(i*n_rows)+j] = string(input[((j+1)*n_cols)-i-1])
		}
	}
	return strings.Join(new, ""), n_rows
}

// reverse a string
func reverse(s string) string {
	chars := []rune(s)
	for i, j := 0, len(chars)-1; i < j; i, j = i+1, j-1 {
		chars[i], chars[j] = chars[j], chars[i]
	}
	return string(chars)
}

// Solution for Part 2 of the challenge
func Part2(input string) int {
	return 1
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([]string, []int, error) {
	maps := strings.Split(input, "\n\n")
	linelen := make([]int, len(maps))
	for i := range maps {
		linelen[i] = strings.Index(maps[i], "\n")
		maps[i] = strings.ReplaceAll(maps[i], "\n", "")
	}
	return maps, linelen, nil
}
