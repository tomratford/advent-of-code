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
	"sync"
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

	sol_p1, history := Part1(objects, start)
	fmt.Println(sol_p1)
	fmt.Println(Part2(objects, history, start))
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
func Part1(objects map[image.Point]int, start image.Point) (int, map[image.Point]int) {

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
	return len(history), history
}

// Solution for Part 2 of the challenge
type Counter struct {
	mu    sync.Mutex
	value int
}

func (c *Counter) Inc() {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.value++
}

func (c *Counter) Value() int {
	return c.value
}

func Part2(objects map[image.Point]int, route map[image.Point]int, start image.Point) int {
	c := &Counter{}

	var wg sync.WaitGroup
	wg.Add(len(route) - 1)

	for new_obj := range route {
		if new_obj.Eq(start) {
			continue // Skip starting point
		}
		go func() {
			new_objects := make(map[image.Point]int)
			for k := range objects {
				new_objects[k]++
			}
			new_objects[new_obj]++

			if DidItLoop(new_objects, start) {
				c.Inc()
			}
			wg.Done()
		}()
	}
	wg.Wait()

	return c.Value()
}

func DidItLoop(objects map[image.Point]int, start image.Point) bool {
	history := make(map[Guard]int)
	g := Guard{start, NORTH}
	for g.Pt.In(BOUNDS) {
		if _, ok := history[g]; ok {
			return true // it looped
		}
		history[g]++

		new_pt := g.Pt.Add(g.Dir)

		if _, ok := objects[new_pt]; ok {
			new_dir := ROTATE_MAP[g.Dir]
			new_pt = g.Pt.Add(new_dir)
			g.Dir = new_dir
			continue
		}

		g.Pt = new_pt
	}
	return false // it just left the route as normal
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
