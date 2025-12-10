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

	fmt.Println(Part1(p))
}

// Structs and types
type Machine struct {
	Indicator []bool
	Length    int
	Buttons   [][]int
	Joltage   []int
}

func Toggle(current []bool, swaps []int) []bool {
	rtn := slices.Clone(current)
	for _, s := range swaps {
		rtn[s] = !current[s]
	}
	return rtn
}

func Score(want, got []bool) int {
	score := 0
	for i := range want {
		if want & got {
			score++ // dont penalise going backwards?
		}
	}
	return score
}

// Solution for Part 1 of the challenge
func Part1(input []Machine) int {
	// Expand the buttons until we reach a solution? Is this brute force? maybe?
	part1 := 0
	for _, m := range input {
		current_score := 0
		zero_light := slices.Repeat([]bool{false}, m.Length)
		queue := make([][][]int, 0)
		for _, b := range m.Buttons {
			queue = append(queue, [][]int{b})
		}
		for {
			newQueue := make([][][]int, 0)
			for _, buttons := range queue {
				curr := slices.Clone(zero_light)
				for _, button := range buttons {
					curr = Toggle(curr, button)
				}
				if slices.Equal(curr, m.Indicator) {
					part1 += len(buttons)
				} else {
					if s := Score(m.Indicator, curr); s >= current_score {
						for _, b := range m.Buttons {
							new := slices.Clone(buttons)
							new = append(new, b)
							new2 := slices.Clone(new)
							slices.Reverse(new2)
							newQueue = append(newQueue, new)
						}
					}
				}
			}
			queue = newQueue
		}
	}

	return part1
}

// Solution for Part 2 of the challenge
func Part2(input string) int {
	return 1
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([]Machine, error) {
	lines := strings.Split(input, "\n")
	machines := make([]Machine, 0, len(lines))
	for _, l := range lines {
		if l == "" {
			continue
		}
		m := Machine{}
		m.Buttons = make([][]int, 0)
		words := strings.Split(l, " ")
		for i, w := range words {
			if i == 0 {
				// Indicator
				m.Length = len(w) - 2
				m.Indicator = make([]bool, 0, m.Length)
				for _, c := range w {
					if c == '.' {
						m.Indicator = append(m.Indicator, false)
					} else if c == '#' {
						m.Indicator = append(m.Indicator, true)
					}
				}
			} else if i == len(words)-1 {
				// Joltage
				w2 := w[1 : len(w)-1] // remove { }
				digits := strings.Split(w2, ",")
				m.Joltage = make([]int, 0, len(digits))
				for _, d := range digits {
					n, err := strconv.Atoi(d)
					if err != nil {
						return []Machine{}, err
					}
					m.Joltage = append(m.Joltage, n)
				}
			} else {
				// Button
				w2 := w[1 : len(w)-1] // remove ( )
				digits := strings.Split(w2, ",")
				button := make([]int, 0, len(digits))
				for _, d := range digits {
					n, err := strconv.Atoi(d)
					if err != nil {
						return []Machine{}, err
					}
					button = append(button, n)
				}
				m.Buttons = append(m.Buttons, button)
			}
		}
		machines = append(machines, m)
	}

	return machines, nil
}
