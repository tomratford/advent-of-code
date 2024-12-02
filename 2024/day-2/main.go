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

	fmt.Println(len(Part1(p)))
	fmt.Println(Part2(p, Part1(p)))
}

// Structs and types

/* Any structs required for the challenge go here */

// Solution for Part 1 of the challenge
func Part1(input [][]int) []int {
	rtn := make([]int, 0, len(input))
	for index, i := range input {
		maxdiff := 0
		tmp := -1
		for _, n := range i {
			if tmp == -1 {
				tmp = n
			} else if tmp == n {
				maxdiff = -1
				break
			} else {
				if diff := n - tmp; diff > 0 {
					if diff > maxdiff {
						maxdiff = diff
					}
				} else {
					diff = -diff
					if diff > maxdiff {
						maxdiff = diff
					}
				}
				tmp = n
			}
		}
		if 0 < maxdiff && maxdiff <= 3 && (slices.IsSorted(i)) {
			// Safe
			rtn = append(rtn, index)
		} else {
			i2 := Reverse(i)
			if 0 < maxdiff && maxdiff <= 3 && (slices.IsSorted(i2)) {
				rtn = append(rtn, index)
			}
		}
	}
	return rtn
}

func Reverse(s []int) []int {
	a := make([]int, 0, len(s))
	for i := len(s) - 1; i >= 0; i-- {
		a = append(a, s[i])
	}
	return a
}

// Solution for Part 2 of the challenge
func Part2(input [][]int, part1 []int) int {
	remove := make(map[int]int)
	for _, p := range part1 {
		remove[p]++
	}

	for i := 0; i < 5; i++ {
		// remove one value from the list
		removed := make([][]int, 0, len(input)-len(part1))
		og_input := make([]int, 0, len(removed))

		for j := range input {
			report := make([]int, len(input[j]))
			copy(report, input[j])
			if _, ok := remove[j]; ok {
				continue
			} else {
				var add []int
				if i == 0 {
					add = report[0:]
				} else if i == 4 {
					add = report[:4]
				} else {
					add = append(report[:i], report[(i+1):]...)
				}
				removed = append(removed, add)
				og_input = append(og_input, j)
			}
		}
		sol := Part1(removed)
		for _, s := range sol {
			k := og_input[s]
			fmt.Println(input[k], i)
			remove[k]++
		}
	}
	return len(remove)
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([][]int, error) {
	lines := strings.Split(input, "\n")
	rtn := make([][]int, 0, len(lines))
	for _, l := range lines {
		if l == "" {
			continue
		}
		nums := make([]int, 0, 5)
		words := strings.Split(l, " ")
		for _, w := range words {
			i, err := strconv.Atoi(w)
			if err != nil {
				return [][]int{}, fmt.Errorf("Couldn't parse digit, %v", err)
			}
			nums = append(nums, i)
		}
		rtn = append(rtn, nums)
	}
	return rtn, nil
}
