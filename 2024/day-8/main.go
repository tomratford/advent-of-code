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

	p, antenna, err := Parse(string(input))
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Println(Part1(p, antenna))
	fmt.Println(Part2(p, antenna))
}

// Structs and types

var BOUNDS image.Rectangle

// Solution for Part 1 of the challenge
func Part1(input map[rune][]image.Point, antenna map[image.Point]rune) int {
	antinodes := make(map[image.Point]int)
	for freq, antennas := range input {
		// Copy to avoid memory issues
		a := make([]image.Point, len(antennas))
		copy(a, antennas)
		for i := range a {
			for j := range a {
				if i == j {
					continue
				}
				antinode, in_bounds := Antinode(a[i], a[j])
				if in_bounds {
					if v, ok := antenna[antinode]; !ok || (ok && v != freq) {
						antinodes[antinode]++
					}
				}
			}
		}
	}
	return len(antinodes)
}

func Antinode(x, y image.Point) (image.Point, bool) {
	d := x.Sub(y)
	rtn := x.Add(d.Mul(-2))
	return rtn, rtn.In(BOUNDS)
}

// Solution for Part 2 of the challenge
func Part2(input map[rune][]image.Point, antenna map[image.Point]rune) int {
	antinodes := make(map[image.Point]int)
	for _, antennas := range input {
		// Copy to avoid memory issues
		a := make([]image.Point, len(antennas))
		copy(a, antennas)
		for i := range a {
			for j := range a {
				if i == j {
					continue
				}
				antinodes[a[i]]++
				antinodes[a[j]]++
				d := a[i].Sub(a[j])
				for p := a[i].Add(d.Mul(-2)); p.In(BOUNDS); p = p.Add(d.Mul(-1)) {
					antinodes[p]++
				}
			}
		}
	}
	return len(antinodes)
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (map[rune][]image.Point, map[image.Point]rune, error) {
	x := 0
	y := 0
	biggest_x := 0

	rtn := make(map[rune][]image.Point)
	antenna := make(map[image.Point]rune)
	for _, r := range input {
		if r == '\n' {
			if biggest_x == 0 {
				biggest_x = x
			}
			x = 0
			y++
			continue
		} else if r != '.' {
			pt := image.Pt(x, y)

			if _, ok := rtn[r]; !ok {
				rtn[r] = make([]image.Point, 0, 10)
			}
			rtn[r] = append(rtn[r], pt)

			antenna[pt] = r
		}
		x++
	}
	BOUNDS = image.Rect(0, 0, biggest_x, y)
	return rtn, antenna, nil
}
