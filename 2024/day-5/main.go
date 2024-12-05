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

	rules, pages, err := Parse(string(input))
	if err != nil {
		fmt.Println(err)
		return
	}

	p1, wrongs := Part1(rules, pages)
	fmt.Println(p1)
	fmt.Println(Part2(rules, wrongs))
}

// Structs and types

/* Any structs required for the challenge go here */

// Solution for Part 1 of the challenge
func Part1(rules map[int]map[int]int, pages [][]int) (int, [][]int) {
	rtn := 0
	wrongs := make([][]int, 0)
outer:
	for _, page := range pages {
		p := make([]int, len(page))
		copy(p, page)
		for i := range p {
			n := p[i]

			if i == len(p)-1 {
				// Must be correct, nothing after to check
				// fmt.Println(p, p[len(p)/2])
				rtn += p[len(p)/2]
			} else {
				// Check all pages appear after
				after := p[i+1:]
				for _, a := range after {
					if _, ok := rules[n][a]; !ok {
						wrongs = append(wrongs, p)
						continue outer
					}
				}
			}
		}
	}
	return rtn, wrongs
}

// Solution for Part 2 of the challenge
func Part2(rules map[int]map[int]int, wrongs [][]int) int {
	rtn := 0
	for _, page := range wrongs {
		p := make([]int, len(page))
		copy(p, page)
		i := 0
	outer:
		for {
			if i >= len(p) {
				panic("unreachable")
			}

			n := p[i]

			if i == len(p)-1 {
				// Must be correct, nothing after to check
				rtn += p[len(p)/2]
				break
			} else {
				// Check all pages appear after
				after := p[i+1:]
				for j, a := range after {
					if _, ok := rules[n][a]; !ok {
						p[i], p[i+j+1] = p[i+j+1], p[i] // Swap pages
						continue outer                  // Try again
					}
				}
				i++
			}
		}
	}
	return rtn
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (map[int]map[int]int, [][]int, error) {
	sections := strings.Split(input, "\n\n")

	// Rules
	rules := make(map[int]map[int]int)
	for _, line := range strings.Split(sections[0], "\n") {
		nums := strings.Split(line, "|")
		n1, err := strconv.Atoi(nums[0])
		if err != nil {
			return map[int]map[int]int{}, [][]int{}, fmt.Errorf("couldn't parse first number in line %q: %v", line, err)
		}
		n2, err := strconv.Atoi(nums[1])
		if err != nil {
			return map[int]map[int]int{}, [][]int{}, fmt.Errorf("couldn't parse second number in line %q: %v", line, err)
		}
		if _, ok := rules[n1]; ok {
			rules[n1][n2]++
		} else {
			rules[n1] = make(map[int]int)
			rules[n1][n2]++
		}
	}

	// Pages
	pages := make([][]int, 0)
	for _, line := range strings.Split(sections[1], "\n") {
		page := make([]int, 0)
		nums := strings.Split(line, ",")
		for _, s := range nums {
			n, err := strconv.Atoi(s)
			if err != nil {
				return map[int]map[int]int{}, [][]int{}, fmt.Errorf("couldn't parse page number in line %q: %v", line, err)
			}
			page = append(page, n)
		}
		pages = append(pages, page)
	}
	return rules, pages, nil
}
