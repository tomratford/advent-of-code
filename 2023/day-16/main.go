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

	p, r, err := Parse(string(input))
	BOUNDS = r
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Println(Part1(p))
}

// Structs and types

var UP = image.Point{0, -1}
var LEFT = image.Point{-1, 0}
var DOWN = image.Point{0, 1}
var RIGHT = image.Point{1, 0}

// Global bound set after parsing
var BOUNDS image.Rectangle

type Beam struct {
	Point     image.Point // Where is it now
	Direction image.Point // Where's it going
}

func (b Beam) String() string {
	var symb string
	if b.Direction.Eq(UP) {
		symb = "↑"
	} else if b.Direction.Eq(RIGHT) {
		symb = "→"
	} else if b.Direction.Eq(LEFT) {
		symb = "←"
	} else if b.Direction.Eq(DOWN) {
		symb = "↓"
	} else {
		panic("we love a good unreachable case here")
	}
	return fmt.Sprintf("(%d,%d)%s", b.Point.X, b.Point.Y, symb)
}

// Print map to help debugging
func prettyPrint(objs map[image.Point]rune) {
	for j := range BOUNDS.Max.Y {
		for i := range BOUNDS.Max.X {
			if v, ok := objs[image.Point{i, j}]; ok {
				fmt.Printf(string(v))
			} else {
				fmt.Print(".")
			}
		}
		fmt.Print("\n")
	}
}
func prettyPrint2(objs map[image.Point]int) {
	fmt.Print(" ")
	for i := range BOUNDS.Max.X {
		fmt.Print(i % 10)
	}
	fmt.Print("\n")
	for j := range BOUNDS.Max.Y {
		fmt.Print(j % 10)
		for i := range BOUNDS.Max.X {
			if _, ok := objs[image.Point{i, j}]; ok {
				fmt.Printf("#")
			} else {
				fmt.Print(".")
			}
		}
		fmt.Print("\n")
	}
}

// Solution for Part 1 of the challenge
func Part1(objs map[image.Point]rune) int {
	// initialise starting beam
	beams := []Beam{
		{
			image.Point{0, 0},
			RIGHT,
		},
	}
	history := make(map[Beam]int, BOUNDS.Max.X*BOUNDS.Max.Y)

	for len(beams) > 0 {
		beam := beams[0]
		for {
			if v, ok := objs[beam.Point]; ok {
				switch v {
				case '|':
					if beam.Direction.Eq(RIGHT) || beam.Direction.Eq(LEFT) {
						new_b := Beam{
							beam.Point,
							UP,
						}
						beam.Direction = DOWN
						beams = append(beams, new_b)
					}
				case '-':
					if beam.Direction.Eq(UP) || beam.Direction.Eq(DOWN) {
						new_b := Beam{
							beam.Point,
							LEFT,
						}
						beam.Direction = RIGHT
						beams = append(beams, new_b)
					}
				case '\\':
					if beam.Direction.Eq(UP) {
						beam.Direction = LEFT
					} else if beam.Direction.Eq(RIGHT) {
						beam.Direction = DOWN
					} else if beam.Direction.Eq(LEFT) {
						beam.Direction = UP
					} else if beam.Direction.Eq(DOWN) {
						beam.Direction = RIGHT
					} else {
						panic("we love a good unreachable case here")
					}
				case '/':
					if beam.Direction.Eq(UP) {
						beam.Direction = RIGHT
					} else if beam.Direction.Eq(RIGHT) {
						beam.Direction = UP
					} else if beam.Direction.Eq(LEFT) {
						beam.Direction = DOWN
					} else if beam.Direction.Eq(DOWN) {
						beam.Direction = LEFT
					} else {
						panic("we love a good unreachable case here")
					}
				}
			}

			history[beam]++
			beam.Point = beam.Point.Add(beam.Direction)

			if !beam.Point.In(BOUNDS) {
				fmt.Println("OUT:", beam)
				break
			} else if _, ok := history[beam]; ok {
				fmt.Println("HIT:", beam)
				break
			}
		}

		// Exit or continue
		if len(beams) == 1 {
			break
		} else {
			beams = beams[1:]
		}
	}
	hit := make(map[image.Point]int)
	for k := range history {
		hit[k.Point]++
	}
	fmt.Println(len(hit)) // minus one to remove the {-1,0} starting point
	return 1
}

func sliceCheck(xs, ys []image.Point) bool {
	if len(xs) != len(ys) {
		return false
	}
	xm := make(map[image.Point]int)
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

// Solution for Part 2 of the challenge
func Part2(input string) int {
	return 1
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (map[image.Point]rune, image.Rectangle, error) {
	x := 0
	y := 0
	objs := make(map[image.Point]rune, len(input)-strings.Count(input, ".")-strings.Count(input, "\n"))
	for _, r := range input {
		if r == '\n' {
			x = 0
			y++
		} else if r == '.' {
			x++
		} else {
			objs[image.Point{x, y}] = r
			x++
		}
	}
	return objs, image.Rect(0, 0, strings.Index(input, "\n"), y), nil
}
