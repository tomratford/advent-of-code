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

	p, err := Parse(string(input))
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Println(Part1(p))
	fmt.Println(Part2(p))
}

// Structs and types

type Visit struct {
	Location string
	DAC      bool
	FFT      bool
}

// Solution for Part 1 of the challenge
func Part1(input map[string][]string) int {
	part1 := 0
	states := make(map[string]int)
	states["you"] = 1
	for {
		newStates := make(map[string]int)
		if _, ok := states["out"]; ok {
			newStates["out"] = states["out"]
		}
		for s := range states {
			for _, b := range input[s] {
				if _, ok := newStates[b]; !ok {
					newStates[b] = states[s]
				} else {
					newStates[b] += states[s]
				}
			}
		}
		//fmt.Println(newStates)
		if maps.Equal(states, newStates) {
			part1 = newStates["out"]
			break
		}
		states = newStates
	}

	return part1
}

// Solution for Part 2 of the challenge
func Part2(input map[string][]string) int {
	part2 := 0
	states := make(map[Visit]int)
	states[Visit{"svr", false, false}] = 1
	for {
		newStates := make(map[Visit]int)
		if _, ok := states[Visit{"out", true, true}]; ok {
			newStates[Visit{"out", true, true}] = states[Visit{"out", true, true}]
		}
		for s := range states {
			for _, b := range input[s.Location] {
				newVisit := Visit{b, s.DAC, s.FFT}
				if b == "dac" {
					newVisit.DAC = true
				}
				if b == "fft" {
					newVisit.FFT = true
				}
				if _, ok := newStates[newVisit]; !ok {
					newStates[newVisit] = states[s]
				} else {
					newStates[newVisit] += states[s]
				}
			}
		}
		//fmt.Println(newStates)
		if maps.Equal(states, newStates) {
			part2 = newStates[Visit{"out", true, true}]
			break
		}
		states = newStates
	}

	return part2
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (map[string][]string, error) {
	lines := strings.Split(input, "\n")
	rtn := make(map[string][]string)
	for _, l := range lines {
		if l == "" {
			continue
		}
		groups := strings.Split(l, ": ")
		rtn[groups[0]] = strings.Split(groups[1], " ")
	}
	return rtn, nil
}
