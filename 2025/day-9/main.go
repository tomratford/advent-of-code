/*
Advent of code; Year YYYY, Day XX

# Some notes on the challenge or solution here

usage:

	go run main.go path/to/input.txt
*/
package main

import (
	"cmp"
	"fmt"
	"image"
	"math"
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

func RectPoints(r image.Rectangle) []image.Point {
	r = r.Canon()
	pts := make([]image.Point, 0)
	for i := r.Min.X; i <= r.Max.X; i++ {
		for j := r.Min.Y; j <= r.Max.Y; j++ {
			pts = append(pts, image.Pt(i, j))
		}
	}
	return pts
}

func PointIn(p image.Point, r image.Rectangle) bool {
	return r.Min.X <= p.X && p.X <= r.Max.X &&
		r.Min.Y <= p.Y && p.Y <= r.Max.Y
}

func GetPoints(points []image.Point, edges []image.Rectangle) []image.Point {
	min_x := slices.MinFunc(points, func(a, b image.Point) int {
		return cmp.Compare(a.X, b.X)
	}).X
	min_y := slices.MinFunc(points, func(a, b image.Point) int {
		return cmp.Compare(a.Y, b.Y)
	}).Y
	max_x := slices.MaxFunc(points, func(a, b image.Point) int {
		return cmp.Compare(a.X, b.X)
	}).X
	max_y := slices.MaxFunc(points, func(a, b image.Point) int {
		return cmp.Compare(a.Y, b.Y)
	}).Y

	rtn := make([]image.Point, 0)
	for j := min_y; j <= max_y; j++ {
		edge_hit := make([]image.Rectangle, 0)
		for i := min_x; i <= max_x; i++ {
			p := image.Pt(i, j)
			for _, e := range edges {
				if PointIn(p, e) && !slices.Contains(edge_hit, e) {
					edge_hit = append(edge_hit, e)
				}
			}
			if len(edge_hit)%2 == 1 {
				rtn = append(rtn, p)
			}
			//fmt.Println(p, edge_hit)
		}
	}

	for _, e := range edges {
		for _, p := range RectPoints(e) {
			if !slices.Contains(rtn, p) {
				rtn = append(rtn, p)
			}
		}
	}
	return rtn
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
	edges = append(edges, image.Rectangle{input[0], input[len(input)-1]}.Canon())

	interior := GetPoints(input, edges)

	//fmt.Println(interior)

	part2 := 0
Exit:
	for _, r := range rects {
		r_int := RectPoints(r)
		for _, p := range r_int {
			if !slices.Contains(interior, p) {
				continue Exit
			}
		}
		p := r.Size()
		size := int((math.Abs(float64(p.X)) + 1.0) * (math.Abs((float64(p.Y))) + 1.0))
		if size > part2 {
			//fmt.Println(p, r)
			part2 = size
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
