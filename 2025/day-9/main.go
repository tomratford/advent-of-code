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
	fmt.Println(Part2(p))
}

// Structs and types

/* Any structs required for the challenge go here */

// Solution for Part 1 of the challenge
func Part1(input []image.Point) int {
	rects := make([]image.Rectangle, 0, len(input)*len(input)/2)
	for i, a := range input {
		for _, b := range input[i+1:] {
			rects = append(rects, image.Rectangle{a, b})
		}
	}

	part1 := 0
	for _, r := range rects {
		p := r.Size()
		size := int((math.Abs(float64(p.X)) + 1.0) * (math.Abs((float64(p.Y))) + 1.0))
		if size > part1 {
			//fmt.Println(p, r)
			part1 = size
		}
	}
	return part1
}

// Implement my own comparison because go's doesn't include eq

func OnBorder(r, s image.Rectangle) bool {
	if r.In(s) {
		return (r.Min.X == s.Min.X || r.Max.X == s.Max.X) && (r.Min.Y == s.Min.Y || r.Max.Y == s.Max.Y)
	} else {
		return false
	}
}

// Solution for Part 2 of the challenge
func Part2(input []image.Point) int {
	rects := make([]image.Rectangle, 0, len(input)*len(input)/2)
	for i, a := range input {
		for _, b := range input[i+1:] {
			rects = append(rects, image.Rectangle{a, b})
		}
	}

	//zero := image.Rectangle{}
	edges := make([]image.Rectangle, 0, len(input))
	for i := range input[1:] {
		edges = append(edges, image.Rectangle{input[i], input[i+1]}.Canon())
	}

	part2 := 0
	for _, r := range rects {
		ecount := 0
		for _, e := range edges {
			if OnBorder(e, r) {
				ecount++
			}
		}
		if ecount >= 3 {
			p := r.Size()
			size := int((math.Abs(float64(p.X)) + 1.0) * (math.Abs((float64(p.Y))) + 1.0))
			if size > part2 {
				fmt.Println(p, r)
				part2 = size
			}
		}
	}
	return part2
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([]image.Point, error) {
	split_input := strings.Split(input, "\n")
	points := make([]image.Point, 0, len(split_input))
	for _, l := range split_input {
		if l == "" {
			continue
		}
		pt := image.Point{}
		for i, c := range strings.Split(l, ",") {
			num, err := strconv.Atoi(c)
			if err != nil {
				return []image.Point{}, fmt.Errorf("Parsing error: %q in line %q", err, l)
			}
			if i == 0 {
				pt.X = num
			} else if i == 1 {
				pt.Y = num
			} else {
				return []image.Point{}, fmt.Errorf("Parsing error in %q", l)
			}
		}
		points = append(points, pt)
	}
	return points, nil
}
