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
		if CheckSlice(i) {
			rtn = append(rtn, index)
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

func CheckSlice(i []int) bool {
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
		return true
	} else {
		i2 := Reverse(i)
		if 0 < maxdiff && maxdiff <= 3 && (slices.IsSorted(i2)) {
			return true
		}
	}
	return false
}

// Solution for Part 2 of the challenge
func Part2(input [][]int, part1 []int) int {
	for i, rpt := range input {
		if slices.Contains(part1, i) {
			continue
		}

		//fmt.Println(rpt, ":")
		for j := range rpt {
			rpt2 := make([]int, len(rpt))
			copy(rpt2, rpt)
			if j == 0 {
				//fmt.Println(rpt[(j + 1):])
				if CheckSlice(rpt2[(j + 1):]) {
					part1 = append(part1, i)
					break
				}
			} else if j == len(rpt)-1 {
				//fmt.Println(rpt[:j])
				if CheckSlice(rpt2[:j]) {
					part1 = append(part1, i)
					break
				}
				// else {
				// 	fmt.Println(rpt)
				// }
			} else {
				slc := append(rpt2[:j], rpt2[j+1:]...)
				//fmt.Println(slc)
				if CheckSlice(slc) {
					part1 = append(part1, i)
					break
				}
			}
		}
		//fmt.Println("====")
	}
	return len(part1)
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
