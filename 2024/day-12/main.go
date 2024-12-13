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

	fmt.Println(p, BOUNDS)
	fmt.Println(Part1(p))
}

// Structs and types

var (
	UP    = image.Pt(0, -1)
	DOWN  = image.Pt(0, 1)
	RIGHT = image.Pt(1, 0)
	LEFT  = image.Pt(-1, 0)
)

var BOUNDS image.Rectangle

// Solution for Part 1 of the challenge
func Part1(input map[image.Point]rune) int {
	seen := make(map[image.Point]int)
	rtn := 0

	sortGarden := func(a, b image.Point) int {
		if a.X < b.X {
			return -1
		} else if a.X == b.X {
			if a.Y < b.Y {
				return -1
			} else if a.Y == b.Y {
				return 0
			} else {
				return 1
			}
		} else {
			return 1
		}
	}
	for pt := range input {
		if _, ok := seen[pt]; !ok {
			// New garden
			seen[pt]++
			garden := append([]image.Point{pt}, GetNeighbours(pt, input, seen)...)
			slices.SortFunc(garden, sortGarden)
			for _, g := range garden {
				seen[g]++
				neighbours := GetNeighbours(g, input, seen)
				for _, n := range neighbours {
					seen[n]++
				}
				garden = append(garden, neighbours...)
			}
			slices.SortFunc(garden, sortGarden)
			area := len(garden)
			perim := 0
			for _, g := range garden {
				newseen := make(map[image.Point]int)
				// newseen[g]++
				fmt.Println(g, 4-len(GetNeighbours(g, input, newseen)))
				perim += 4 - len(GetNeighbours(g, input, newseen))
			}
			fmt.Println(garden, area, perim, area*perim)
			rtn += area * perim
		}
	}

	return rtn
}

func GetNeighbours(pt image.Point, gardens map[image.Point]rune, seen map[image.Point]int) []image.Point {
	pts := []image.Point{
		pt.Add(UP),
		pt.Add(DOWN),
		pt.Add(RIGHT),
		pt.Add(LEFT),
	}

	r := gardens[pt]
	return slices.DeleteFunc(pts, func(p image.Point) bool {
		v, ok := gardens[p]
		_, s := seen[p]
		// keep not seen and in bounds (ok) and same rune
		return s || !ok || v != r
	})
}

// Solution for Part 2 of the challenge
func Part2(input string) int {
	return 1
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (map[image.Point]rune, error) {
	rtn := make(map[image.Point]rune)

	x := 0
	y := 0

	max_x := 0

	for _, r := range input {
		if r == '\n' {
			if max_x == 0 {
				max_x = x
			}
			x = 0
			y++
			continue
		}

		rtn[image.Pt(x, y)] = r

		x++
	}

	BOUNDS = image.Rect(0, 0, max_x, y)

	return rtn, nil
}
