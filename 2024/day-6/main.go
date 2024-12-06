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

	objects, start, err := Parse(string(input))
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Println(Part1(objects, start))
	fmt.Println(Part2(objects, start))
}

// Structs and types

var NORTH = image.Pt(0, -1)
var SOUTH = image.Pt(0, 1)
var WEST = image.Pt(-1, 0)
var EAST = image.Pt(1, 0)

var ROTATE_MAP = map[image.Point]image.Point{
	NORTH: EAST,
	EAST:  SOUTH,
	SOUTH: WEST,
	WEST:  NORTH,
}

type Guard struct {
	Pt  image.Point
	Dir image.Point
}

var BOUNDS image.Rectangle

// Solution for Part 1 of the challenge
func Part1(objects map[image.Point]int, start image.Point) int {

	history := make(map[image.Point]int)

	g := Guard{start, NORTH}
	for g.Pt.In(BOUNDS) {
		history[g.Pt]++

		new_pt := g.Pt.Add(g.Dir)
		if _, ok := objects[new_pt]; ok {
			new_dir := ROTATE_MAP[g.Dir]
			new_pt = g.Pt.Add(new_dir)
			g.Dir = new_dir
		}

		g.Pt = new_pt
	}
	return len(history)
}

// Solution for Part 2 of the challenge
func Part2(objects map[image.Point]int, start image.Point) int {

	barrels := make(map[image.Point]int)
	history := make(map[Guard]int)

	g := Guard{start, NORTH}
	for g.Pt.In(BOUNDS) {
		history[g]++

		new_pt := g.Pt.Add(g.Dir)

		right := ROTATE_MAP[g.Dir]
		for x := g.Pt; x.In(BOUNDS); x = x.Add(right) {
			if _, ok := history[Guard{x, right}]; ok {
				barrels[new_pt]++
				break
			}
		}

		if _, ok := objects[new_pt]; ok {
			g.Dir = ROTATE_MAP[g.Dir]
			continue
		}

		g.Pt = new_pt
	}
	fmt.Println(history)
	fmt.Println(barrels)
	return len(barrels)
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (map[image.Point]int, image.Point, error) {
	x := 0
	y := 0
	objects := make(map[image.Point]int)
	var start image.Point
	for _, r := range input {
		if r == '\n' {
			if y == 0 {
				BOUNDS.Max.X = x
			}
			x = 0
			y++
			continue
		}

		if r == '^' {
			start = image.Pt(x, y)
		}
		if r == '#' {
			objects[image.Pt(x, y)]++
		}

		x++
	}
	BOUNDS.Max.Y = y + 1
	return objects, start, nil
}
