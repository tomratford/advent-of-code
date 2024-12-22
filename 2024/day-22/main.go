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

	//fmt.Println(TrySequence(p, []int{-2, 1, -1, 3}))

	//fmt.Println(Part1(p))
	fmt.Println(Part2(p))
}

// Structs and types

/* Any structs required for the challenge go here */

// Solution for Part 1 of the challenge
func Part1(input []int) int {
	rtn := 0
	for _, i := range input {
		rtn += Calc2000thNumber(i)
	}
	return rtn
}

func Calc2000thNumber(x int) int {
	for i := 0; i < 2000; i++ {
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
	}

	return x
}

// Solution for Part 2 of the challenge
func Part2(input []int) int {
	bananas := make(chan int, 19*19*19*19)
	for i := -9; i < 10; i++ {
		for j := -9; j < 10; j++ {
			for k := -9; k < 10; k++ {
				for z := -9; z < 10; z++ {
					go func() {
						bananas <- TrySequence(input, []int{i, j, k, z})
					}()
				}
			}
		}
	}
	best := 0
	for i := 0; i < 19*19*19*19; i++ {
		if got := <-bananas; got > best {
			best = got
		}
	}
	return best
}

func TrySequence(input []int, seq []int) int {
	rtn := 0
	for _, i := range input {
		rtn += GetNumAfterSequence(i, seq)
	}
	return rtn
}

func GetNumAfterSequence(x int, seq []int) int {
	lastfour := make([]int, 0, 4)
	lastx := x
	for i := 0; i < 2000; i++ {
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
		if len(lastfour) < 4 {
			lastfour = append(lastfour, x%10-lastx%10)
		} else {
			lastfour = append(lastfour[1:], x%10-lastx%10)
		}
		if slices.Equal(lastfour, seq) {
			return x % 10
		}
		lastx = x
	}
	return 0
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([]int, error) {
	lines := strings.Split(input, "\n")
	rtn := make([]int, 0, len(lines))
	for _, l := range lines {
		if l == "" {
			continue
		}
		if n, err := strconv.Atoi(l); err != nil {
			return []int{}, fmt.Errorf("couldn't parse %q: %v", l, err)
		} else {
			rtn = append(rtn, n)
		}
	}
	return rtn, nil
}
