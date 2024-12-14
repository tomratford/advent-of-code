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
	"image/color"
	"image/png"
	"os"
	"slices"
	"strings"
	"sync"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Println("usage: go run main.go path/to/input.txt")
		return
	}

	switch os.Args[1] {
	case "sample.txt":
		BOUNDS = image.Rect(0, 0, 11, 7)
	case "input.txt":
		BOUNDS = image.Rect(0, 0, 101, 103)
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
	Part2(p)
}

// Structs and types

var BOUNDS image.Rectangle

type Robot struct {
	pos, vel image.Point
}

func (r *Robot) incPos() {
	r.pos = r.PosAfter(1)
}

func (r Robot) PosAfter(seconds int) image.Point {
	raw_pos := r.pos.Add(r.vel.Mul(seconds))
	pos := image.Point{raw_pos.X % BOUNDS.Max.X, raw_pos.Y % BOUNDS.Max.Y}
	if pos.X < 0 {
		pos.X = BOUNDS.Max.X + pos.X
	}
	if pos.Y < 0 {
		pos.Y = BOUNDS.Max.Y + pos.Y
	}
	return pos
}

// Solution for Part 1 of the challenge
func Part1(input []Robot) int {
	var topleft, topright, bottomleft, bottomright int
	xlim := BOUNDS.Max.X / 2
	ylim := BOUNDS.Max.Y / 2
	//fmt.Printf("xlim: %d, ylim %d\n", xlim, ylim)
	for _, r := range input {
		pos := r.PosAfter(100)
		//fmt.Printf("%v -> %v\n", r, pos)
		if pos.X < xlim {
			if pos.Y < ylim {
				topleft++
			} else if pos.Y > ylim {
				bottomleft++
			}
		} else if pos.X > xlim {
			if pos.Y < ylim {
				topright++
			} else if pos.Y > ylim {
				bottomright++
			}
		}
	}
	return topleft * topright * bottomleft * bottomright
}

// Solution for Part 2 of the challenge
func Part2(input []Robot) {
	var wg sync.WaitGroup
	wg.Add(10000)
	for i := range 10000 {
		go func() {
			robots := make([]image.Point, 0, len(input))
			for _, r := range input {
				robots = append(robots, r.PosAfter(i))
			}
			OutputImage(robots, i)
			wg.Done()
		}()
	}
	wg.Wait()
}

func OutputImage(input []image.Point, iter int) {
	img := image.NewNRGBA(BOUNDS)
	for j := 0; j < BOUNDS.Max.Y; j++ {
		for i := 0; i < BOUNDS.Max.X; i++ {
			if slices.ContainsFunc(input, func(r image.Point) bool {
				return r.Eq(image.Pt(i, j))
			}) {
				img.Set(i, j, color.Black)
			} else {
				img.Set(i, j, color.White)
			}
		}
	}

	f, err := os.Create(fmt.Sprintf("images/iter%.4d.png", iter))
	if err != nil {
		panic(err)
	}

	if err := png.Encode(f, img); err != nil {
		f.Close()
		panic(err)
	}

	if err := f.Close(); err != nil {
		panic(err)
	}
}

func PrettyPrint(input []Robot) {
	for j := 0; j < BOUNDS.Max.Y; j++ {
		for i := 0; i < BOUNDS.Max.X; i++ {
			if slices.ContainsFunc(input, func(r Robot) bool {
				return r.pos.Eq(image.Pt(i, j))
			}) {
				fmt.Print("#")
			} else {
				fmt.Print(".")
			}
		}
		fmt.Print("\n")
	}
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([]Robot, error) {
	lines := strings.Split(input, "\n")
	rtn := make([]Robot, 0, len(lines))
	for _, l := range lines {
		if l == "" {
			continue
		}
		var xpos, ypos, xvel, yvel int
		_, err := fmt.Sscanf(l, "p=%d,%d v=%d,%d", &xpos, &ypos, &xvel, &yvel)
		if err != nil {
			return []Robot{}, fmt.Errorf("couldn't parse line %q: %v", l, err)
		}
		rtn = append(rtn, Robot{image.Point{xpos, ypos}, image.Point{xvel, yvel}})
	}
	return rtn, nil
}
