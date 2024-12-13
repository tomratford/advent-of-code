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
	"math"
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

type Problem struct {
	A image.Point
	B image.Point

	Prize image.Point
}

// Solution for Part 1 of the challenge
func Part1(input []Problem) int {
	// Solve as a system of linear equations. Giving solution
	// (A.x A.y)^-1 (Prize.X)
	// (B.x B.y)    (Prize.Y)
	rtn := 0
	for _, i := range input {
		// Get inverse
		det := i.B.Y*i.A.X - i.B.X*i.A.Y
		A_sol := float64(i.B.Y*i.Prize.X-i.A.Y*i.Prize.Y) / float64(det)
		B_sol := float64(-i.B.X*i.Prize.X+i.A.X*i.Prize.Y) / float64(det)

		// If solution not a int, no sol
		if math.Floor(A_sol) == A_sol && math.Floor(B_sol) == B_sol {
			rtn += int(3*A_sol + B_sol)
		}
	}
	return rtn
}

// Solution for Part 2 of the challenge
func Part2(input []Problem) int {
	input2 := make([]Problem, len(input))
	copy(input2, input)
	for i := range input2 {
		input2[i].Prize.X += 10000000000000
		input2[i].Prize.Y += 10000000000000
	}
	return Part1(input2)
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([]Problem, error) {
	problems := strings.Split(input, "\n\n")
	rtn := make([]Problem, 0, len(problems))
	for _, p := range problems {
		x := Problem{}
		r := strings.NewReader(p)
		fmt.Fscanf(r, "Button A: X+%d, Y+%d\nButton B: X+%d, Y+%d\nPrize: X=%d, Y=%d", &x.A.X, &x.B.X, &x.A.Y, &x.B.Y, &x.Prize.X, &x.Prize.Y)
		rtn = append(rtn, x)
	}
	return rtn, nil
}
