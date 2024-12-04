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

	coords, starts, starts2, err := Parse(string(input))
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Println(Part1(coords, starts))
	fmt.Println(Part2(coords, starts2))
}

// Structs and types

var North = image.Pt(0, -1)
var South = image.Pt(0, 1)
var West = image.Pt(-1, 0)
var East = image.Pt(1, 0)
var North_West = image.Pt(-1, -1)
var North_East = image.Pt(1, -1)
var South_West = image.Pt(-1, 1)
var South_East = image.Pt(1, 1)

var DIRS = []image.Point{
	North,
	North_East,
	East,
	South_East,
	South,
	South_West,
	West,
	North_West,
}

// Solution for Part 1 of the challenge
func Part1(coords map[image.Point]rune, starts []image.Point) int {
	rtn := 0
	for _, s := range starts {
		for _, d := range DIRS {
			pt2 := s.Add(d)
			if rune2, ok := coords[pt2]; ok && rune2 == 'M' {
				pt3 := s.Add(d.Mul(2))
				if rune3, ok := coords[pt3]; ok && rune3 == 'A' {
					pt4 := s.Add(d.Mul(3))
					if rune4, ok := coords[pt4]; ok && rune4 == 'S' {
						//fmt.Println(s, pt2, pt3, pt4)
						rtn += 1
					}
				}
			}
		}
	}
	return rtn
}

// Solution for Part 2 of the challenge
func Part2(coords map[image.Point]rune, starts []image.Point) int {
	rtn := 0
	for _, s := range starts {
		pt1 := s.Add(North_West)
		pt2 := s.Add(South_West)
		pt3 := s.Add(North_East)
		pt4 := s.Add(South_East)

		if r_nw, ok := coords[pt1]; ok {
			if r_sw, ok := coords[pt2]; ok {
				if r_ne, ok := coords[pt3]; ok {
					if r_se, ok := coords[pt4]; ok {
						// West is all M
						if r_nw == 'M' && r_sw == 'M' && r_se == 'S' && r_ne == 'S' {
							rtn += 1
							// South is all M
						} else if r_sw == 'M' && r_se == 'M' && r_ne == 'S' && r_nw == 'S' {
							rtn += 1
							// East is all M
						} else if r_se == 'M' && r_ne == 'M' && r_nw == 'S' && r_sw == 'S' {
							rtn += 1
							// North is all M
						} else if r_ne == 'M' && r_nw == 'M' && r_se == 'S' && r_sw == 'S' {
							rtn += 1
						}
					}
				}
			}
		}
	}
	return rtn
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (map[image.Point]rune, []image.Point, []image.Point, error) {
	coords := make(map[image.Point]rune)
	starts := make([]image.Point, 0, len(input)/10)
	starts2 := make([]image.Point, 0, len(input)/10)

	x := 0
	y := 0
	for _, r := range input {
		if r == '\n' {
			y++
			x = 0
			continue
		}

		pt := image.Pt(x, y)

		coords[pt] = r

		if r == 'X' {
			starts = append(starts, pt)
		}
		if r == 'A' {
			starts2 = append(starts2, pt)
		}
		x++
	}

	return coords, starts, starts2, nil
}
