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

	fmt.Println(Part1(p, Beam{
		image.Point{-1, 0},
		RIGHT,
	}))

	fmt.Println(Part2(p))
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
func prettyPrint2(objs map[image.Point]rune, history map[Beam]int) {
	fmt.Print(" ")
	for i := range BOUNDS.Max.X {
		fmt.Print(i % 10)
	}
	fmt.Print("\n")
	for j := range BOUNDS.Max.Y {
		fmt.Print(j % 10)
		for i := range BOUNDS.Max.X {
			if v, ok := objs[image.Point{i, j}]; ok {
				fmt.Printf(string(v))
			} else if _, ok := history[Beam{
				image.Point{i, j},
				RIGHT,
			}]; ok {
				fmt.Print(">")
			} else if _, ok := history[Beam{
				image.Point{i, j},
				LEFT,
			}]; ok {
				fmt.Print("<")
			} else if _, ok := history[Beam{
				image.Point{i, j},
				DOWN,
			}]; ok {
				fmt.Print("v")
			} else if _, ok := history[Beam{
				image.Point{i, j},
				UP,
			}]; ok {
				fmt.Print("^")
			} else {
				fmt.Print(".")
			}
		}
		fmt.Print("\n")
	}
}

// Solution for Part 1 of the challenge
func Part1(objs map[image.Point]rune, start Beam) int {
	// initialise starting beam
	beams := []Beam{start}
	history := make(map[Beam]int, BOUNDS.Dx()*BOUNDS.Dy())
	for len(beams) > 0 {
		// // Pretty print in terminal
		// defer fmt.Printf("\033[%dB\015", BOUNDS.Max.Y+1)
		// prettyPrint2(objs, history)
		// fmt.Printf("\033[%dA\015", BOUNDS.Max.Y+1)
		// time.Sleep(199_999_999)

		// update beam position and find those which have stopped or hit a loop
		for i := len(beams) - 1; i >= 0; i-- { // work backwards to avoid panic removing objects
			history[beams[i]]++                                     // Add the beam to the history
			beams[i].Point = beams[i].Point.Add(beams[i].Direction) // Move the beam in its direction

			// Check if we've hit a known point or we're still in bounds
			if v, ok := history[beams[i]]; ok || !beams[i].Point.In(BOUNDS) {
				if _, ok2 := objs[beams[i].Point]; (ok2 && v == 1) && ok { // if we've only hit the object once
					// This accounts for if a beam moves into a object that we've already left, i.e. we enter up, leave west, or arrive west, enter up
					// do naut
				} else {
					if i+1 > len(beams) { // i.e last point
						beams = beams[:i]
					} else {
						beams = append(beams[:i], beams[i+1:]...)
					}
					continue // skip to next beam
				}
			}

			// check if they have hit an object, and update directions
			if v, ok := objs[beams[i].Point]; ok {
				switch v {
				case '|':
					if beams[i].Direction.Eq(RIGHT) || beams[i].Direction.Eq(LEFT) {
						new_b := Beam{
							beams[i].Point,
							UP,
						}
						beams[i].Direction = DOWN
						beams = append(beams, new_b)
					}
				case '-':
					if beams[i].Direction.Eq(UP) || beams[i].Direction.Eq(DOWN) {
						new_b := Beam{
							beams[i].Point,
							LEFT,
						}
						beams[i].Direction = RIGHT
						beams = append(beams, new_b)
					}
				case '\\':
					if beams[i].Direction.Eq(UP) {
						beams[i].Direction = LEFT
					} else if beams[i].Direction.Eq(RIGHT) {
						beams[i].Direction = DOWN
					} else if beams[i].Direction.Eq(LEFT) {
						beams[i].Direction = UP
					} else if beams[i].Direction.Eq(DOWN) {
						beams[i].Direction = RIGHT
					} else {
						panic("we love a good unreachable case here")
					}
				case '/':
					if beams[i].Direction.Eq(UP) {
						beams[i].Direction = RIGHT
					} else if beams[i].Direction.Eq(RIGHT) {
						beams[i].Direction = UP
					} else if beams[i].Direction.Eq(LEFT) {
						beams[i].Direction = DOWN
					} else if beams[i].Direction.Eq(DOWN) {
						beams[i].Direction = LEFT
					} else {
						panic("we love a good unreachable case here")
					}
				}
			}
		}
	}
	hit := make(map[image.Point]int)
	for k := range history {
		hit[k.Point]++
	}
	//fmt.Println(len(hit) - 1) // minus one to remove the {-1,0} starting point
	return len(hit) - 1
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
func Part2(objs map[image.Point]rune) int {
	beams := make([]Beam, 0, 2*BOUNDS.Dx()+2*BOUNDS.Dy())
	// left/right start
	for i := range BOUNDS.Dy() {
		beams = append(beams, Beam{
			image.Point{-1, i},
			RIGHT,
		}, Beam{
			image.Point{BOUNDS.Dx() + 1, i},
			LEFT,
		})
	}
	// up/down start
	for i := range BOUNDS.Dx() {
		beams = append(beams, Beam{
			image.Point{i, -1},
			DOWN,
		}, Beam{
			image.Point{i, BOUNDS.Dy() + 1},
			UP,
		})
	}
	rtn := 0
	for _, b := range beams {
		tmp := Part1(objs, b)
		if tmp > rtn {
			rtn = tmp
		}
	}
	return rtn
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
