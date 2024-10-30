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

	Part1(p)
}

// Structs and types

type Pt = image.Point

/* Any structs required for the challenge go here */

// Solution for Part 1 of the challenge
func Part1(input [][]Pt) int {
	sol := 0
	for i, points := range input {
		foundY, y := checkVertical(points)
		if foundY {
			sol += int(100 * math.Round(y))
			// fmt.Printf("y = %f\n", y)
			continue
		}
		foundX, x := checkHorizontal(points)
		if foundX {
			sol += int(math.Round(x))
			// fmt.Printf("x = %f\n", x)
			continue
		}
		fmt.Println(i)
		panic("no sol found")
	}
	fmt.Println(sol)
	return 1
}

/*
Check for a possible horizontal reflection

Rotate all points by 90 degrees and check for vertical
*/
func checkHorizontal(points []Pt) (bool, float64) {
	newPoints := make([]Pt, 0, len(points))
	for _, p := range points {
		newPoints = append(newPoints, Pt{-p.Y, p.X}) // 90 degrees counter-clockwise
	}
	check, x := checkVertical(newPoints)
	if !check {
		return false, -1
	}
	return true, x

	// // dead code to reverse if necessary
	// reversePoints := make([]Pt, 0, len(newPoints))
	// for _, p := range newPoints {
	// 	reversePoints = append(reversePoints, Pt{p.Y, -p.X}) // 90 degrees clockwise
	// }
}

/*
Checks for a possible vertical reflection.

In theory, a reflection is equal if the points in the interior rectangle is the same?
So, if we reflect at y=2.5, we only care about points between y = [0,5] to check.
*/
func checkVertical(points []Pt) (bool, float64) {
	maxY := slices.MaxFunc(points, func(a, b Pt) int { return cmp.Compare(a.Y, b.Y) })

	for y := 0.5; y < float64(maxY.Y); y++ {
		new, old := reflect(points, y, maxY.Y)
		if hasElements(new, old) {
			return true, y
		}
	}
	// if we have gone past the maximum point
	return false, -1
}

func hasElements(x, y []Pt) bool {
	if len(x) != len(y) {
		return false
	}
	xmap := make(map[Pt]int, len(x))
	for _, xval := range x {
		xmap[xval]++
	}
	for _, yval := range y {
		if _, ok := xmap[yval]; !ok {
			return false
		}
	}
	return true
}

// Return the new reflected and truncated old slice of points
func reflect(points []Pt, y float64, maxY int) ([]Pt, []Pt) {
	new_points := slices.Clone(points)
	points_to_keep := make([]int, 0, len(new_points)) // Points to keep to check
	for i, p := range points {
		new_points[i].Y = p.Y + int(2*(y-float64(p.Y)))
		ceiling := int(2 * y)
		floor := 0
		if new_points[i].Y <= ceiling && new_points[i].Y >= floor {
			points_to_keep = append(points_to_keep, i)
		}
	}
	rtn1 := make([]Pt, 0, len(points_to_keep)) // new points
	rtn2 := make([]Pt, 0, len(points_to_keep)) // truncated old
	for i := 0; i < len(points_to_keep); i++ {
		rtn1 = append(rtn1, new_points[points_to_keep[i]])
		rtn2 = append(rtn2, points[points_to_keep[i]])
	}
	return rtn1, rtn2
}

// Solution for Part 2 of the challenge
func Part2(input string) int {
	return 1
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([][]Pt, error) {
	n := strings.Count(input, "\n\n") // Newlines
	rtn := make([][]Pt, 0, n)

	indiv := make([]Pt, 0)
	x := 0
	y := 0
	newshape := false
	for _, r := range input {
		if r == '#' {
			newshape = false
			indiv = append(indiv, Pt{x, y})
		}
		if r == '.' {
			newshape = false
		}
		x += 1
		if r == '\n' {
			x = 0
			y += 1
			if newshape {
				rtn = append(rtn, slices.Clone(indiv)) // add to rtn
				indiv = make([]Pt, 0)                  // start anew
				y = 0
			}
			newshape = true
		}
	}
	rtn = append(rtn, slices.Clone(indiv)) // add to rtn

	return rtn, nil
}
