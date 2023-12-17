/*
Advent of code; Year 2023, Day 12

	  NOTE: you could just do this as a dynamic programming problem and implement
	  a cache or something but I am a little nerd and I knew there was a 'smart'
	  way to solve this.

	  This solutions uses non-deterministic finite automata.
	I knew I recognised this problem from somewhere - and low and behold someone on
	the solutions reddit megathread had reminded me what the hell that is. I will
	try to comment the code (if only for my own interest in the future) but the
	gist is this:
	 * We have a set of states (made up of '.', '#') defined by the 1,1,3 bit of
	   our input. i.e. 1,1,3 = .#.#.###
	 * We have our inputs, we work through our states using the inputs. a dot can
	   loop on itself infinitely. i.e. if we are on a dot, 3 more dots keeps up on
	   that same dot state.
	 * Count how many are on the final state - that is our permutations

usage:

	go run main.go path/to/input.txt
*/
package main

import (
	"fmt"
	"maps"
	"os"
	//	"slices"
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

	fmt.Println(Part1(p))
}

// Structs and types

const (
	BROKEN = 1
	UNKNOWN = 0
	EMPTY = -1
)

type Row struct {
	line []int
	sizes []int
}

// Solution for Part 1 of the challenge
func Part1(input []Row) int {
	score := 0
	for _, r := range input {
		states := MakeStates(r.sizes)
		state_map := make(map[int]int,len(states))
		state_map[0] = 1 // Start on the empty state
		for _, c := range append(r.line, EMPTY) {
			new_state_map := maps.Clone(state_map)
			for n, v := range state_map {
				if n >= len(states) {
					continue // skip values that are too large
				}
				switch c {
				case EMPTY:
					if states[n] == BROKEN {
						new_state_map[n] -= v
						if states[n+1] == EMPTY { // Only move if the next state is an empty state
							new_state_map[n+1] += v
						}
					}
					// else its already on an dot/empty state and we continue on said state
				case BROKEN:
					if state_map[n] == BROKEN { // If we are on a broken
						new_state_map[n] -= v
						if n+1 < len(states) && states[n+1] == BROKEN { // Only move if the next state is an empty state
							new_state_map[n+1] += v
						}
					} else { // if empty state
						new_state_map[n] -= v
						new_state_map[n+1] += v // We don't need to check as an empty state is always followed by a broken state
					}
				case UNKNOWN:
					// We are always going to proceed to the next state with a broken
					if states[n] == BROKEN {
						new_state_map[n] -= v
					} // We may be able to stay on our current state if we are on a empty
					
					if n+1 < len(states) {
						new_state_map[n+1] += v
					}
				default:
					panic("shouldn't happen")
				}
			}
			state_map = new_state_map
		}
		score += state_map[len(states)-1]
	}
	return score
}

func MakeStates(ns []int) []int {
	states := []int{EMPTY} // Start at a empty state irregardless
	for _, n := range ns {
		for i:=0;i<n;i++ {
			states = append(states, BROKEN)
		}
		states = append(states, EMPTY) // This means our last entry will always be empty
	}
	return states
}

// Solution for Part 2 of the challenge
func Part2(input string) int {
	return 1
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([]Row, error) {
	lines := strings.Split(input, "\n")
	nums := make([]Row, len(lines)-1)
	for i, line := range lines[:len(lines)-1] {
		xs, ns, err := ParseLine(line)
		if err != nil {
			return []Row{}, err
		}
		nums[i] = Row{xs, ns}
	}
	return nums, nil
}

func ParseLine(input string) ([]int, []int, error) {
	part, req, found := strings.Cut(input, " ")
	if !found {
		return []int{}, []int{}, fmt.Errorf("no space found in line")
	}
	xs := make([]int, len(part))
	for i, c := range part {
		switch c {
		case '#':
			xs[i] = BROKEN
		case '?':
			xs[i] = UNKNOWN
		case '.':
			xs[i] = EMPTY
		}
	}

	nums_str := strings.Split(req, ",")
	ns := make([]int, len(nums_str))
	for i, a := range nums_str {
		x, err := strconv.Atoi(a)
		if err != nil {
			return []int{}, []int{}, err
		}
		ns[i] = x
	}

	return xs, ns, nil
}
