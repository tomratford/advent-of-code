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
	"maps"
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
}

// Structs and types

var BOUNDS image.Rectangle

type Direction = image.Point

var LEFT = Direction{1, 0}
var RIGHT = Direction{-1, 0}
var DOWN = Direction{0, 1}
var UP = Direction{0, -1}

var DIRMAP = map[Direction][3]Direction{
	LEFT:  [3]Direction{LEFT, DOWN, UP},
	RIGHT: [3]Direction{RIGHT, UP, DOWN},
	DOWN:  [3]Direction{DOWN, LEFT, RIGHT},
	UP:    [3]Direction{UP, LEFT, RIGHT},
}

type Crucible struct {
	Pos   image.Point
	Dir   Direction
	N_Dir int
}

func (c Crucible) Move(d Direction, cost int) (Crucible, error) {
	var new_n_dir int
	if c.Dir.Eq(d) {
		if c.N_Dir == 3 {
			return Crucible{}, fmt.Errorf("Cannot move more than 3 in the same direction")
		} else {
			new_n_dir = c.N_Dir + 1
		}
	} else {
		new_n_dir = 1
	}

	if new_pos := c.Pos.Add(d); new_pos.In(BOUNDS) {
		return Crucible{
			new_pos,
			d,
			new_n_dir,
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
	return fmt.Sprintf("(%d,%d)%d%s", c.Pos.X, c.Pos.Y, c.N_Dir, symb)
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
	dist, _ := Dijkstra(graph, source, target)

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

	fmt.Println(dist)

	//prettyPrint(graph, route)

	for i := 0; i <= 3; i++ {
		x, ok := dist[Crucible{
			target,
			RIGHT,
			i,
		}]
		if ok {
			return x
		}
	}

	for i := 0; i <= 3; i++ {
		x, ok := dist[Crucible{
			target,
			DOWN,
			i,
		}]
		if ok {
			return x
		}
	}

	return -1
}

func Dijkstra(graph map[image.Point]int, source image.Point, target image.Point) (map[Crucible]int, map[Crucible]Crucible) {

	dist := make(map[Crucible]int, len(graph))
	prev := make(map[Crucible]Crucible, len(graph))

	pq := make(PriorityQueue, len(graph))

	source_right := Crucible{
		source,
		RIGHT,
		0,
	}

	source_down := Crucible{
		source,
		DOWN,
		0,
	}

	dist[source_right] = 0
	dist[source_down] = 0

	pq.VPush(source_right, 0)
	pq.VPush(source_down, 0)

	for len(pq) > 0 {
		u, _ := pq.VPop()
		vs := getNeighbours(u)
		for _, v := range vs {
			alt := dist[u] + graph[v.Pos]
			if dv, ok := dist[*v]; ok && alt < dv {
				prev[*v] = u
				dist[*v] = alt
				pq.VPush(*v, alt)
			}
		}
	}

	return dist, prev
}

func getNeighbours(u Crucible) []*Crucible {
	ifEq := func(d1 Direction, d2 Direction, n int) int {
		if d1.Eq(d2) {
			return n + 1
		} else {
			return 1
		}
	}

	u_P := u.Pos
	u_D := u.Dir
	u_N := u.N_Dir

	neighbours := map[image.Point]*Crucible{
		LEFT: {
			u_P.Add(LEFT),
			LEFT,
			ifEq(LEFT, u_D, u_N),
		},
		RIGHT: {
			u_P.Add(RIGHT),
			RIGHT,
			ifEq(RIGHT, u_D, u_N),
		},
		DOWN: {
			u_P.Add(DOWN),
			DOWN,
			ifEq(DOWN, u_D, u_N),
		},
		UP: {
			u_P.Add(UP),
			UP,
			ifEq(UP, u_D, u_N),
		},
	}

	for k, v := range neighbours {
		if v.N_Dir > 3 {
			delete(neighbours, k)
		} else if !v.Pos.In(BOUNDS) {
			delete(neighbours, k)
		}
	}

	return slices.Collect(maps.Values(neighbours))
}

// Priority queue implementation, modified from https://pkg.go.dev/container/heap#example-package-PriorityQueue

// An Item is something we manage in a priority queue.
type Item struct {
	value    Crucible // The value of the item; arbitrary.
	priority int      // The priority of the item in the queue.
	// The index is needed by update and is maintained by the heap.Interface methods.
	index int // The index of the item in the heap.
}

// A PriorityQueue implements heap.Interface and holds Items.
type PriorityQueue []*Item

func (pq PriorityQueue) Len() int { return len(pq) }

func (pq PriorityQueue) Less(i, j int) bool {
	// We want Pop to give us the highest, not lowest, priority so we use greater than here.
	return pq[i].priority > pq[j].priority
}

func (pq PriorityQueue) Swap(i, j int) {
	pq[i], pq[j] = pq[j], pq[i]
	pq[i].index = i
	pq[j].index = j
}

func (pq *PriorityQueue) Push(x any) {
	n := len(*pq)
	item := x.(*Item)
	item.index = n
	*pq = append(*pq, item)
}

func (pq *PriorityQueue) Pop() any {
	old := *pq
	n := len(old)
	item := old[n-1]
	old[n-1] = nil  // don't stop the GC from reclaiming the item eventually
	item.index = -1 // for safety
	*pq = old[0 : n-1]
	return item
}

// update modifies the priority and value of an Item in the queue.
func (pq *PriorityQueue) update(item *Item, value Crucible, priority int) {
	item.value = value
	item.priority = priority
	heap.Fix(pq, item.index)
}

func (pq *PriorityQueue) VPush(v Crucible, p int) {
	pq.Push(&Item{v, p, 0}) // OK To do as Push changes 0 to n
}

func (pq *PriorityQueue) VPop() (v Crucible, p int) {
	x := pq.Pop().(*Item)
	return x.value, x.priority
}

// Solution for Part 2 of the challenge
func Part2(input string) int {
	return 1
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
