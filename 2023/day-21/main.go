/*
Advent of code; Year YYYY, Day XX

# Some notes on the challenge or solution here

usage:

	go run main.go path/to/input.txt
*/
package main

import (
	"fmt"
	"image"
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

	start, rocks, b, err := Parse(string(input))
	if err != nil {
		fmt.Println(err)
		return
	}
	BOUNDS = b

	size, err := strconv.Atoi(os.Args[2])
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Println(Part1(start, rocks, size))
}

// Structs and types

var UP = image.Point{0, -1}
var DOWN = image.Point{0, 1}
var LEFT = image.Point{-1, 0}
var RIGHT = image.Point{1, 0}

var BOUNDS image.Rectangle

type State struct {
	Pt   image.Point
	Step int
}

// func (s *State) isEven() bool {
// 	return s.Step%2 == 0
// }

// Solution for Part 1 of the challenge
func Part1(start image.Point, rocks map[image.Point]int, distance int) int {
	states := make([]State, 0, distance^4)
	states = append(states, State{start, 0})

	history := make(map[image.Point]int)
	history[start] = 0
	for len(states) > 0 {
		s := states[0]
		states = states[1:]

		for _, p := range getNeighbours(s.Pt, rocks) {
			newState := State{
				p,
				s.Step + 1,
			}
			if _, ok := history[p]; !ok && s.Step+1 <= distance {
				history[p] = s.Step + 1
				states = append(states, newState)
			}
		}
	}

	keep := make(map[image.Point]int)
	for k, v := range history {
		if v%2 == 0 {
			keep[k]++
		}
	}

	//fmt.Println(history)
	return len(keep)
}

func getNeighbours(pt image.Point, rocks map[image.Point]int) []image.Point {
	return slices.DeleteFunc([]image.Point{
		pt.Add(UP),
		pt.Add(DOWN),
		pt.Add(LEFT),
		pt.Add(RIGHT),
	}, func(pt image.Point) bool {
		_, ok := rocks[pt]
		return !pt.In(BOUNDS) || ok
	})
}

// Solution for Part 2 of the challenge
func Part2(input string) int {
	return 1
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (image.Point, map[image.Point]int, image.Rectangle, error) {
	x := 0
	y := 0
	bigx := strings.Index(input, "\n")
	var start image.Point
	rocks := make(map[image.Point]int)
	for _, r := range input {
		if r == '\n' {
			x = 0
			y++
			continue
		}

		if r == '#' {
			rocks[image.Pt(x, y)]++
		}
		if r == 'S' {
			start = image.Pt(x, y)
		}
		x++
	}
	return start, rocks, image.Rect(0, 0, bigx, y), nil
}
