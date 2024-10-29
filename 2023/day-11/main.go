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
	"slices"
)

type Pt = image.Point

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

	//Part1(p)
	Part2(p)
}

// Structs and types

/* Any structs required for the challenge go here */

// Solution for Part 1 of the challenge
func Part1(input []Pt) int {
	xs := allXs(input)
	needsDouble := make([]int, 0, slices.Max(xs))
	for i := range slices.Max(xs) {
		if !slices.Contains(xs, i) {
			needsDouble = append(needsDouble, i)
		}
	}
	doubledInput := slices.Clone(input)
	for _, x := range needsDouble {
		for i, pt := range input {
			if pt.X > x {
				doubledInput[i].X += 1
			}
		}
	}
	result := 0
	for i, pt := range doubledInput {
		tmp := getDistances(pt, doubledInput[i:len(doubledInput)])
		result += tmp
	}
	fmt.Println(result)
	return 1
}

func getDistances(st Pt, pts []Pt) int {
	x := 0
	for _, pt := range pts {
		x += inner(st.Sub(pt))
	}
	return x
}

func inner(x Pt) int {
	return int(math.Abs(float64(x.X)) + math.Abs(float64(x.Y)))
}

func allXs(x []Pt) []int {
	y := make([]int, 0, len(x))
	for _, pt := range x {
		y = append(y, pt.X)
	}
	return y
}

// Solution for Part 2 of the challenge
func Part2(input []Pt) int {
	xs := allXs(input)
	needsDouble := make([]int, 0, slices.Max(xs))
	for i := range slices.Max(xs) {
		if !slices.Contains(xs, i) {
			needsDouble = append(needsDouble, i)
		}
	}
	doubledInput := slices.Clone(input)
	for _, x := range needsDouble {
		for i, pt := range input {
			if pt.X > x {
				doubledInput[i].X += (1000000 - 1)
			}
		}
	}
	result := 0
	for i, pt := range doubledInput {
		tmp := getDistances(pt, doubledInput[i:len(doubledInput)])
		result += tmp
	}
	fmt.Println(result)
	return 1
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([]Pt, error) {
	x := 0
	y := 0
	galaxies := make([]Pt, 0, len(input)/10) // Realistic maximum size

	one_this_row := false
	for _, r := range input {
		if r == '#' {
			galaxies = append(galaxies, image.Pt(x, y))
			one_this_row = true
		}
		x += 1
		if r == '\n' {
			x = 0
			y += 1
			if !one_this_row {
				//y += 1 // Part 1
				y += (1000000 - 1)
			}
			one_this_row = false
		}
	}
	return galaxies, nil
}
