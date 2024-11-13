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

	dirs, dist, cols, err := Parse(string(input))
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Println(Part1(dirs, dist))
	fmt.Println(Part2(cols))
}

// Structs and types

type Direction struct {
	image.Point
}

func (d Direction) Eq(x Direction) bool { return d.Point.Eq(x.Point) }

func (d Direction) Mul(x int) image.Point { return d.Point.Mul(x) }

func (d Direction) String() string {
	switch {
	case d.Eq(UP):
		return "↑"
	case d.Eq(RIGHT):
		return "→"
	case d.Eq(LEFT):
		return "←"
	case d.Eq(DOWN):
		return "↓"
	}
	return "X"
}

var UP = Direction{image.Point{0, -1}}
var DOWN = Direction{image.Point{0, 1}}
var RIGHT = Direction{image.Point{1, 0}}
var LEFT = Direction{image.Point{-1, 0}}

var DIRMAP = map[string]Direction{
	"R": RIGHT,
	"L": LEFT,
	"D": DOWN,
	"U": UP,
}

var DIRMAP2 = map[string]Direction{
	"0": RIGHT,
	"2": LEFT,
	"1": DOWN,
	"3": UP,
}

type Color string

func newColor(s string) (Color, error) {
	if s[1] != '#' {
		return "", fmt.Errorf("input is not a color hex: %s", s)
	}
	return Color(s[1 : len(s)-1]), nil
}

/* Any structs required for the challenge go here */

// Solution for Part 1 of the challenge
func Part1(dirs []Direction, dist []int) int {
	edges := make([]image.Point, 0, len(dirs))
	boundary_points := 0
	pos := image.Point{0, 0}
	for i := 0; i < len(dirs); i++ {
		boundary_points += dist[i]
		edges = append(edges, pos.Add(dirs[i].Mul(dist[i])))
		pos = edges[len(edges)-1]
	}
	twoA := shoelace(edges)
	interior_points := (twoA / 2) + 1 - (boundary_points / 2)

	return interior_points + boundary_points
}

func shoelace(edges []image.Point) int {
	edges = append(edges, edges[0])
	twoA := 0
	for i := 0; i < len(edges)-1; i++ {
		twoA += det(edges[i], edges[i+1])
	}
	return twoA
}

// Simple det function for shoelace
func det(a, b image.Point) int {
	return a.X*b.Y - a.Y*b.X
}

// Solution for Part 2 of the challenge
func Part2(cols []Color) int {
	dirs := make([]Direction, 0, len(cols))
	dist := make([]int, 0, len(cols))
	for _, c := range cols {
		d, i := c.decode()
		dirs = append(dirs, d)
		dist = append(dist, i)
	}
	return Part1(dirs, dist)
}

func (c Color) decode() (Direction, int) {
	len_hex := c[1:6]
	dir_hex := c[6]

	i, err := strconv.ParseInt(string(len_hex), 16, 64)
	if err != nil {
		panic(err)
	}

	return DIRMAP2[string(dir_hex)], int(i)
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([]Direction, []int, []Color, error) {
	lines := strings.Split(input, "\n")

	dirs := make([]Direction, 0, len(lines))
	dist := make([]int, 0, len(lines))
	cols := make([]Color, 0, len(lines))

	for _, l := range lines {
		words := strings.Split(l, " ")
		if len(words) != 3 {
			fmt.Println("skipping line:", l)
			continue
		}
		d, ok := DIRMAP[words[0]]
		if !ok {
			fmt.Println("can't map to direction:", words[0], "in line", l)
		}
		i, err := strconv.Atoi(words[1])
		if err != nil {
			fmt.Println("can't parse number:", words[1], "in line", l)
			continue
		}
		col, err := newColor(words[2])
		if err != nil {
			fmt.Println("can't parse color:", words[2], "in line", l)
			continue
		}

		dirs = append(dirs, d)
		dist = append(dist, i)
		cols = append(cols, col)
	}
	return dirs, dist, cols, nil
}
