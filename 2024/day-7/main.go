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
	"strconv"
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
}

// Structs and types

type Calibration struct {
	Target   int
	Inputs []int
}

// Solution for Part 1 of the challenge
func Part1(input []Calibration) int {
	rtn := 0
	for _, i := range input {
		rtn += FindOperator(i.Target, 0, i.Inputs)
	}
	return rtn
}

func FindOperator(target int, got int, inputs []int) int {
	if len(inputs) == 0 {
		if got == target {
			return target
		} else {
			return 0
		}
	}
	plus := FindOperator(target, got + inputs[0], inputs[1:])
	minus := FindOperator(target, got * inputs[0], inputs[1:])
	if plus == target {
		return plus
	}
	if minus == target {
		return minus
	}
	return 0
}

// Solution for Part 2 of the challenge
func Part2(input string) int {
	return 1
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([]Calibration, error) {
	lines := strings.Split(input, "\n")
	rtn := make([]Calibration, 0, len(lines))
	for _, l := range lines {
		if l == "" {
			continue
		}
		nums := strings.Split(l, ": ")
		target, err := strconv.Atoi(nums[0])
		if err != nil {
			return []Calibration{}, fmt.Errorf("Couldn't parse target in line %q, %v", l, err)
		}
		inputs_raw := strings.Split(nums[1], " ")
		inputs := make([]int, 0, len(inputs_raw))
		for _, i := range inputs_raw {
			input, err := strconv.Atoi(i)
			if err != nil {
				return []Calibration{}, fmt.Errorf("Couldn't parse target %q in line %q, %v", i, l, err)
			}
			inputs = append(inputs, input)
		}
		rtn = append(rtn, Calibration{
			target,
			inputs,
		})
	}
	return rtn, nil
}
