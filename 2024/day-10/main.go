/*
Advent of code; Year YYYY, Day XX

# Some notes on the challenge or solution here

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

	p, err := Parse(string(input))
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Println(Part1(p))
	fmt.Println(Part2(p))
}

// Structs and types

var (
	UP    = image.Pt(0, -1)
	DOWN  = image.Pt(0, 1)
	RIGHT = image.Pt(1, 0)
	LEFT  = image.Pt(-1, 0)
)

var BOUNDS image.Rectangle

// Priority queue implementation
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

// Solution for Part 1 of the challenge
func Part1(input map[image.Point]int) int {
	rtn := 0
	for k, v := range input {
		if v == 0 {
			rtn += Dijkstra(k, input)
		}
	}
	return rtn
}

func Dijkstra(start image.Point, topo map[image.Point]int) int {
	Q := make(PQ[image.Point], 0, len(topo))

	dist := make(map[image.Point]int)
	prev := make(map[image.Point]image.Point)

	dist[start] = 0
	Q.GPush(start, 0)

	rtn := make(map[image.Point]int)

	for len(Q) > 0 {
		u, d := Q.GPop()

		if u_val, ok := topo[u]; ok && u_val == 9 {
			rtn[u]++
		}

		for _, v := range get_neighbours(u, topo) {
			alt := d + topo[v]
			if v_dist, ok := dist[v]; !ok || (ok && alt < v_dist) {
				dist[v] = alt
				prev[v] = u
				Q.GPush(v, alt)
			}
		}
	}
	return len(rtn)
}

func get_neighbours(pt image.Point, topo map[image.Point]int) []image.Point {
	pts := []image.Point{
		pt.Add(UP),
		pt.Add(DOWN),
		pt.Add(RIGHT),
		pt.Add(LEFT),
	}

	return slices.DeleteFunc(pts, func(p image.Point) bool {
		v1, ok1 := topo[pt]
		v2, ok2 := topo[p]

		return !(ok1 && ok2 && v2-v1 == 1)
	})
}

// Solution for Part 2 of the challenge
func Part2(input map[image.Point]int) int {
	rtn := 0
	for k, v := range input {
		if v == 0 {
			rtn += DFS(input, k)
		}
	}
	return rtn
}

func DFS(topo map[image.Point]int, u image.Point) int {
	if topo[u] == 9 {
		return 1
	}
	rtn := 0
	for _, v := range get_neighbours(u, topo) {
		rtn += DFS(topo, v)
	}
	return rtn
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (map[image.Point]int, error) {
	x := 0
	y := 0

	max_x := 0

	rtn := make(map[image.Point]int)

	for _, r := range input {
		if r == '\n' {
			if max_x == 0 {
				max_x = x
			}
			x = 0
			y++
			continue
		}

		n, err := strconv.Atoi(string(r))
		if err != nil {
			return map[image.Point]int{}, fmt.Errorf("Could't parse rune %q: %v", r, err)
		}

		rtn[image.Pt(x, y)] = n

		x++
	}

	BOUNDS = image.Rect(0, 0, max_x, y+1)

	return rtn, nil
}
