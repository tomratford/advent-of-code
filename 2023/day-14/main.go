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
	"maps"
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

	r, c, rect, err := Parse(string(input))
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Println(Part1(r, c, rect))

	fmt.Println(Part2(r, c, rect))

}

// Structs and types

type Pt = image.Point

var NORTH = Pt{0, -1}
var WEST = Pt{-1, 0}
var SOUTH = Pt{0, 1}
var EAST = Pt{1, 0}

/* Any structs required for the challenge go here */

// Solution for Part 1 of the challenge
func Part1(round []Pt, cubes []Pt, rect image.Rectangle) int {
	newRound := move(NORTH, round, cubes, rect)
	rtn := 0
	for _, r := range newRound {
		rtn += rect.Max.Y - r.Y
	}
	return rtn
}

func compPt(a, b Pt) int {
	if a.Y < b.Y {
		return -1
	} else if a.Y == b.Y {
		if a.X < b.X {
			return -1
		} else if a.X == b.X {
			return 0
		} else {
			return 1
		}
	} else {
		return 1
	}
}

func move(direction Pt, round []Pt, cubes []Pt, rect image.Rectangle) []Pt {
	cubem := make(map[Pt]int)
	for _, c := range cubes {
		cubem[c]++
	}

	newRound := make([]Pt, 0, len(round))
	slices.SortFunc(round, func(a, b Pt) int {
		if direction.Eq(SOUTH) || direction.Eq(EAST) {
			return -1 * compPt(a, b)
		} else {
			return compPt(a, b)
		}
	})
	for _, r := range round {
		tmp := r
		for {
			r2 := tmp.Add(direction)                    // Move <direction> 1
			if _, ok := cubem[r2]; ok || !r2.In(rect) { // If on a cube block or out of bounds
				cubem[tmp] += 1                  // block new round blocks from using this space
				newRound = append(newRound, tmp) // append last point
				break                            // move onto the next round block
			}
			tmp = r2 // update tmp to the new value
		}
	}

	return newRound
}

func prettyPrint(round []Pt, cubes []Pt, rect image.Rectangle) {
	rounm := make(map[Pt]int)
	for _, r := range round {
		rounm[r]++
	}
	cubem := make(map[Pt]int)
	for _, c := range cubes {
		cubem[c]++
	}

	for j := range rect.Max.Y {
		for i := range rect.Max.X {
			if _, ok := rounm[Pt{i, j}]; ok {
				fmt.Print("O")
			} else if _, ok := cubem[Pt{i, j}]; ok {
				fmt.Print("#")
			} else {
				fmt.Print(".")
			}
		}
		fmt.Print("\n")
	}
}

// Solution for Part 2 of the challenge
func Part2(round []Pt, cubes []Pt, rect image.Rectangle) int {
	tmp := round
	history := make(map[int][]Pt, 500)
	max := 1_000_000_000
	for i := 0; i < max; i++ {
		new := move(EAST, move(SOUTH, move(WEST, move(NORTH, tmp, cubes, rect), cubes, rect), cubes, rect), cubes, rect)
		// Get keys in guaranteed low -> high sort order
		keys := slices.Collect(maps.Keys(history))
		slices.Sort(keys)
		for _, j := range keys {
			if sliceCheck(new, history[j]) {
				//fmt.Printf("\r%d", j)
				oldi := i
				i += (((max - j - 1) / (i - j)) - 1) * (i - j)
				fmt.Printf("Loop found at i=%d, j=%d, skipping to %d\n", oldi, j, i)
			}
		}
		history[i] = new
		tmp = new
	}
	prettyPrint(tmp, cubes, rect)
	rtn := 0
	for _, r := range tmp {
		rtn += rect.Max.Y - r.Y
	}
	return rtn
}

func sliceCheck(xs, ys []Pt) bool {
	xm := make(map[Pt]int)
	for _, x := range xs {
		xm[x]++
	}
	for _, y := range ys {
		if _, ok := xm[y]; ok {
			delete(xm, y)
		} else {
			return false
		}
	}
	return len(xm) == 0
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([]Pt, []Pt, image.Rectangle, error) {
	round := make([]Pt, 0, len(input)/5)
	cubes := make([]Pt, 0, len(input)/6)
	x := 0
	y := 0
	for _, r := range input {
		switch r {
		case 'O':
			round = append(round, Pt{x, y})
			x += 1
		case '#':
			cubes = append(cubes, Pt{x, y})
			x += 1
		case '\n':
			x = 0
			y += 1
		default:
			x += 1
		}
	}
	x = strings.Index(input, "\n")
	return round, cubes, image.Rect(0, 0, x, y), nil
}
