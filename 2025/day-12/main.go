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
	"iter"
	"os"
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

	fmt.Println(p)
}

// Structs and types

type SubBoard struct {
	Board     [3][3]bool // keyable in a struct
	Inserting Shape
}

// The idea is to store a map of 3x3 subboards and a shape to be inserted
// into them, which support the DP approach. We then take a 3x3 subboard
// and see if any of the shapes allowed can fit into it. We will have to
// try all rotations (4), and all rotations with an x-axis flip and y-axis
// flip. This will be 12 total attempts for each subboard. If there are
// repeats this is accounted for by the DP approach.
var DP = make(map[SubBoard]SubBoard)

func (s SubBoard) Fit() (SubBoard, error) {
	if m, ok := DP[s]; ok {
		return m, nil
	}
	new := SubBoard{}
	for i := range 3 {
		for j := range 3 {
			if xor(s.Board[i][j], s.Inserting.Points[i][j]) {
				new.Board[i][j] = true
			} else {
				return SubBoard{}, fmt.Errorf("Can't fit in board") // return zero board
			}
		}
	}
	return new, nil
}

type Board struct {
	Bounds image.Point
	Filled [][]bool
	Size   int
	Shapes [6]int
}

func NewBoard(bounds image.Point, shapes [6]int) Board {
	b := make([][]bool, bounds.Y)
	for i := range bounds.Y {
		b[i] = make([]bool, bounds.X)
	}
	return Board{bounds, b, bounds.X * bounds.Y, shapes}
}

func (b Board) SubBoards(s Shape) iter.Seq[SubBoard] {
	return func(yield func(SubBoard) bool) {
		for i := 0; i+3 < b.Bounds.X; i++ {
			for j := 0; j+3 < b.Bounds.Y; j++ {
				sb := b.Filled[j : j+2][i : i+2]
				v := SubBoard{}
				copy(v.Board[0][:], sb[0])
				copy(v.Board[1][:], sb[1])
				copy(v.Board[2][:], sb[2])
				v.Inserting = s

				if !yield(v) {
					return
				}
			}
		}
	}
}

func (b Board) CanFit() bool {

}

type Shape struct {
	Points [3][3]bool
	Size   int
}

var SHAPES [6]Shape

/* Any structs required for the challenge go here */
func xor(a, b bool) bool {
	return (a || b) && !(a && b)
}

// Solution for Part 1 of the challenge
func Part1(bounds []image.Point, allowed [][6]int) int {
	part1 := 0
	for i := range bounds {
		board := NewBoard(bounds[i], allowed[i])
		if board.CanFit() {
			part1++
		}
	}
	return part1
}

// Solution for Part 2 of the challenge
func Part2(input string) int {
	return 1
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (string, error) {
	return input, nil
}
