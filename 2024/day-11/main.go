/*
Advent of code; Year YYYY, Day XX

# Some notes on the challenge or solution here

usage:

	go run main.go path/to/input.txt
*/
package main

import (
	"fmt"
	"maps"
	"math"
	"math/big"
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

	//fmt.Println(Part1(p))
	fmt.Println(Part2(p))
}

// Structs and types

/* Any structs required for the challenge go here */

// Solution for Part 1 of the challenge
func Part1(input []float64) int {
	lens := make(chan int, len(input))
	for _, n := range input {
		go func() {
			lens <- Part1Inner([]float64{n})
		}()
	}
	rtn := 0
	for i := 0; i < len(input); i++ {
		rtn += <-lens
	}
	close(lens)
	return rtn
}

func Part1Inner(input []float64) int { // Dumb solution
	ns := make([]float64, len(input))
	copy(ns, input)
	for blink := 0; blink < 25; blink++ {
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
func Part2(input []float64) string {
	stones := make(map[float64]*big.Int)
	for _, i := range input {
		if _, ok := stones[i]; !ok {
			stones[i] = big.NewInt(1)
		} else {
			stones[i] = stones[i].Add(stones[i], big.NewInt(1))
		}
	}

	for blinks := 0; blinks < 75; blinks++ {
		curr_stones := maps.Clone(stones)
		for n, v := range curr_stones {
			left, right := NextState(n)

			if _, ok := stones[left]; !ok {
				stones[left] = big.NewInt(0)
			}
			stones[left] = stones[left].Add(stones[left], v)
			if right != -1 {
				if _, ok := stones[right]; !ok {
					stones[right] = big.NewInt(0)
				}
				stones[right] = stones[right].Add(stones[left], v)
			}
			stones[n].Sub(stones[n], v)
		}
	}

	var rtn big.Int
	for n, v := range stones {
		if n == -1 {
			continue
		}
		(&rtn).Add(&rtn, v)
	}
	return rtn.String()
}

func NextState(x float64) (float64, float64) {
	if x == 0 {
		return 1, -1
	} else if len := math.Floor(math.Log10(x) + 1); math.Mod(len, 2) == 0 {
		return Split(x, len)
	} else {
		return x * 2024, -1
	}
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
