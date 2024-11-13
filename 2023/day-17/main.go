/*
Advent of code; Year 2023, Day 17

	As I see this, a Djikstra solution with a alternate "getNeighbours" function of
	sorts should be more than enough to sastify this challenge

usage:

	go run main.go path/to/input.txt
*/
package main

import (
	"container/heap"
	"fmt"
	"image"
	"os"
	"slices"
	"strconv"
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
	if err != nil {
		fmt.Println(err)
		return
	}
	BOUNDS = r

	source := image.Pt(0, 0)
	target := image.Pt(BOUNDS.Max.X-1, BOUNDS.Max.Y-1)

	fmt.Println(Part1(p, source, target))
	fmt.Println(Part2(p, source, target))

}

// Structs and types

var BOUNDS image.Rectangle

type Direction = image.Point

var LEFT = Direction{-1, 0}
var RIGHT = Direction{1, 0}
var DOWN = Direction{0, 1}
var UP = Direction{0, -1}

var DIRMAP = map[Direction][2]Direction{
	LEFT:  {DOWN, UP},
	RIGHT: {UP, DOWN},
	DOWN:  {LEFT, RIGHT},
	UP:    {LEFT, RIGHT},
}

type Crucible struct {
	Pos image.Point
	Dir Direction
}

func (c Crucible) Move(d Direction, amount int) (Crucible, error) {
	if new_pos := c.Pos.Add(d.Mul(amount)); new_pos.In(BOUNDS) {
		return Crucible{
			new_pos,
			d,
		}, nil
	} else {
		return Crucible{}, fmt.Errorf("Out of bounds")
	}
}

func (c Crucible) String() string {
	var symb string
	if c.Dir.Eq(UP) {
		symb = "↑"
	} else if c.Dir.Eq(RIGHT) {
		symb = "→"
	} else if c.Dir.Eq(LEFT) {
		symb = "←"
	} else if c.Dir.Eq(DOWN) {
		symb = "↓"
	} else {
		symb = fmt.Sprintf("Error: %v", c.Dir)
	}
	return fmt.Sprintf("(%d,%d)%s", c.Pos.X, c.Pos.Y, symb)
}

func prettyPrint(objs map[image.Point]int, route []image.Point) {
	for j := range BOUNDS.Max.Y {
		for i := range BOUNDS.Max.X {
			p := image.Point{i, j}
			if slices.Contains(route, p) {
				fmt.Printf("\033[31m%2d\033[0m ", objs[p])
			} else {
				fmt.Printf("%2d ", objs[p])
			}
		}
		fmt.Print("\n")
	}
}

// Solution for Part 1 of the challenge
func Part1(graph map[image.Point]int, source image.Point, target image.Point) int {
	Q := make(PQ[Crucible], 0, len(graph))

	dist := make(map[Crucible]int)
	prev := make(map[image.Point]image.Point)

	Q.GPush(
		Crucible{
			source,
			RIGHT,
		},
		0)
	Q.GPush(
		Crucible{
			source,
			DOWN,
		},
		0)

	dist[Crucible{
		source,
		RIGHT,
	}] = 0
	dist[Crucible{
		source,
		DOWN,
	}] = 0

	for len(Q) > 0 {
		u, heat := Q.GPop()
		//fmt.Printf("Popd %v with %d\n", u, heat)

		if u.Pos.Eq(target) {
			// p := target
			// route := make([]image.Point, 0, len(graph))
			// for {
			// 	route = append(route, p)
			// 	v, ok := prev[p]
			// 	if !ok {
			// 		break
			// 	}
			// 	p = v
			// }
			// prettyPrint(graph, route)
			return dist[u]
		}
		for _, d := range DIRMAP[u.Dir] {
			alt := heat
			lastPos := u.Pos
			for i := 1; i <= 3; i++ {
				v, err := u.Move(d, i)
				if err == nil {
					alt += graph[v.Pos]
					if x, ok := dist[v]; !ok || (ok && alt < x) {
						dist[v] = alt
						prev[v.Pos] = lastPos
						//fmt.Printf("Push %v with %d\n", v, alt)
						Q.GPush(v, alt)
					}
					lastPos = v.Pos
				}
			}
		}
	}
	return -1
}

// Priority queue implementation, from https://github.com/mnml/aoc/blob/main/2023/17/1.go
type pqi[T any] struct {
	v T
	p int
}

type PQ[T any] []pqi[T]

func (q PQ[_]) Len() int           { return len(q) }
func (q PQ[_]) Less(i, j int) bool { return q[i].p < q[j].p }
func (q PQ[_]) Swap(i, j int)      { q[i], q[j] = q[j], q[i] }
func (q *PQ[T]) Push(x any)        { *q = append(*q, x.(pqi[T])) }
func (q *PQ[_]) Pop() (x any)      { x, *q = (*q)[len(*q)-1], (*q)[:len(*q)-1]; return x }
func (q *PQ[T]) GPush(v T, p int)  { heap.Push(q, pqi[T]{v, p}) }
func (q *PQ[T]) GPop() (T, int)    { x := heap.Pop(q).(pqi[T]); return x.v, x.p }

// Solution for Part 2 of the challenge
func Part2(graph map[image.Point]int, source image.Point, target image.Point) int {
	Q := make(PQ[Crucible], 0, len(graph))

	dist := make(map[Crucible]int)
	prev := make(map[image.Point]image.Point)

	Q.GPush(
		Crucible{
			source,
			RIGHT,
		},
		0)
	Q.GPush(
		Crucible{
			source,
			DOWN,
		},
		0)

	dist[Crucible{
		source,
		RIGHT,
	}] = 0
	dist[Crucible{
		source,
		DOWN,
	}] = 0

	for len(Q) > 0 {
		u, heat := Q.GPop()
		//fmt.Printf("Popd %v with %d\n", u, heat)

		if u.Pos.Eq(target) {
			// p := target
			// route := make([]image.Point, 0, len(graph))
			// for {
			// 	route = append(route, p)
			// 	v, ok := prev[p]
			// 	if !ok {
			// 		break
			// 	}
			// 	p = v
			// }
			// prettyPrint(graph, route)
			return dist[u]
		}
		for _, d := range DIRMAP[u.Dir] {
			alt := heat
			lastPos := u.Pos
			for i := 1; i <= 10; i++ {
				v, err := u.Move(d, i)
				if err == nil {
					alt += graph[v.Pos]
					if x, ok := dist[v]; (!ok || (ok && alt < x)) && i > 3 {
						dist[v] = alt
						prev[v.Pos] = lastPos
						//fmt.Printf("Push %v with %d\n", v, alt)
						Q.GPush(v, alt)
					}
					lastPos = v.Pos
				}
			}
		}
	}
	return -1
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (map[image.Point]int, image.Rectangle, error) {
	x := 0
	y := 0
	lastx := 0
	rtn := make(map[image.Point]int, len(input)) // slight overallocation but whatever
	for _, r := range input {
		if r == '\n' {
			lastx = x
			x = 0
			y++
			continue
		}

		i, err := strconv.Atoi(string(r))
		if err != nil {
			return map[image.Point]int{}, image.Rectangle{}, err
		}
		rtn[image.Pt(x, y)] = i

		x++
	}
	return rtn, image.Rect(0, 0, lastx, y), nil
}
