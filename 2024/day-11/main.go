/*
Advent of code; Year YYYY, Day XX

# Some notes on the challenge or solution here

usage:

	go run main.go path/to/input.txt
*/
package main

import (
	"fmt"
	"math"
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

	//fmt.Println(Part1(p, 25))
	fmt.Println(Part2(p))
}

// Structs and types

/* Any structs required for the challenge go here */

// Solution for Part 1 of the challenge
func Part1(input []float64, count int) int { // Dumb solution
	ns := make([]float64, len(input))
	copy(ns, input)
	for blink := 0; blink < count; blink++ {
		i := 0
		for i < len(ns) {
			x := ns[i]
			if x == 0 {
				ns[i] = 1
				i++
			} else if len := math.Floor(math.Log10(x) + 1); math.Mod(len, 2) == 0 {
				left, right := Split(x, len)
				ns[i] = left
				ns = slices.Insert(ns, i+1, right)
				i += 2
			} else {
				ns[i] *= 2024
				i++
			}
		}
	}
	return len(ns)
}

func Split(x float64, len float64) (float64, float64) {
	half_len_int := int(len) / 2
	left_side := math.Floor(x / math.Pow10(half_len_int))
	right_side := x - left_side*math.Pow10(half_len_int)
	return left_side, right_side
}

// Solution for Part 2 of the challenge
func Part2(input []float64) int {
	lens := make(chan int, len(input))
	for _, n := range input {
		go func() {
			lens <- Part1([]float64{n}, 75)
		}()
	}
	rtn := 0
	for i := 0; i < len(input); i++ {
		rtn += <-lens
	}
	close(lens)
	return rtn
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([]float64, error) {
	nums_str := strings.Split(input, " ")
	nums := make([]float64, 0, len(nums_str))
	for _, n_s := range nums_str {
		n, err := strconv.ParseFloat(n_s, 64)
		if err != nil {
			return []float64{}, fmt.Errorf("Couldn't parse %q: %v", n_s, err)
		}
		nums = append(nums, n)
	}
	return nums, nil
}
