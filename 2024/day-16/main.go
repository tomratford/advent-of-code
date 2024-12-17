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
	"os"
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

	start, end, err := Parse(string(input))
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Println(Part1(start, end))
	fmt.Println(Part2(start, end))
}

// Structs and types

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

// Used to store where we've searched
type Route struct {
	Pt  complex128
	Dir complex128
}

func (r Route) Moves(walls map[complex128]int) ([]Route, []int) {
	moves := make([]Route, 0, 3)
	costs := make([]int, 0, 3)

	// Keep moving case
	pt1 := r.Pt + r.Dir
	if _, ok := walls[pt1]; !ok {
		moves = append(moves, Route{pt1, r.Dir})
		costs = append(costs, 1)
	}
	// Rotate clockwise
	pt2 := r.Pt + (r.Dir * complex(0, 1))
	if _, ok := walls[pt2]; !ok {
		moves = append(moves, Route{pt2, r.Dir * complex(0, 1)})
		costs = append(costs, 1001)
	}
	// Rotate anti-clockwise
	pt3 := r.Pt + (r.Dir * complex(0, -1))
	if _, ok := walls[pt3]; !ok {
		moves = append(moves, Route{pt3, r.Dir * complex(0, -1)})
		costs = append(costs, 1001)
	}

	return moves, costs
}

var (
	NORTH = complex(0, -1)
	SOUTH = complex(0, 1)
	EAST  = complex(1, 0)
	WEST  = complex(-1, 0)
)

var WALLS map[complex128]int

// Solution for Part 1 of the challenge
func Part1(start, end complex128) int {
	score, _ := Dijkstra(start, end, WALLS)
	return score
}

func Dijkstra(start, end complex128, walls map[complex128]int) (int, map[complex128]complex128) {
	Q := make(PQ[Route], 0, len(walls))

	dist := make(map[complex128]int)
	prev := make(map[complex128]complex128)

	dist[start] = 0
	Q.GPush(Route{start, EAST}, 0)

	for len(Q) > 0 {
		u, d := Q.GPop()

		if u.Pt == end {
			return d, prev
		}

		moves, costs := u.Moves(walls)
		for i := 0; i < len(moves); i++ {
			alt := d + costs[i]
			v := moves[i]
			if v_dist, ok := dist[v.Pt]; !ok || (ok && alt < v_dist) {
				dist[v.Pt] = alt
				prev[v.Pt] = u.Pt
				Q.GPush(v, alt)
			}
		}
	}
	return -1, map[complex128]complex128{}
}

// Solution for Part 2 of the challenge
func Part2(start, end complex128) int {

	score, route := get_route(start, end, WALLS)
	blockers := make([][]complex128, 0, len(route))
	for k := range route {
		blockers = append(blockers, []complex128{k})
	}

	for i := 0; i < len(blockers); i++ {
		mywalls := make(map[complex128]int)
		for k, v := range WALLS {
			mywalls[k] = v
		}
		for _, k := range blockers[i] {
			mywalls[k]++
		}
		score2, route2 := get_route(start, end, mywalls)
		if score2 != score {
			continue
		}
		for k := range route2 {
			if _, ok := route[k]; !ok {
				route[k]++
				blockers = append(blockers, append(blockers[i], k))
			}
		}
	}

	return len(route)
}

func get_route(start, end complex128, walls map[complex128]int) (int, map[complex128]int) {
	score, prev := Dijkstra(start, end, walls)
	route := make(map[complex128]int)
	p := end
	for {
		route[p]++
		if p2, ok := prev[p]; !ok {
			break
		} else {
			p = p2
		}
	}
	return score, route
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (complex128, complex128, error) {
	x := 0.0
	y := 0.0
	walls := make(map[complex128]int)
	var start, end complex128
	for _, r := range input {
		if r == '\n' {
			x = 0
			y++
			continue
		}

		if r == '#' {
			walls[complex(x, y)]++
		}
		if r == 'S' {
			start = complex(x, y)
		}
		if r == 'E' {
			end = complex(x, y)
		}
		x++
	}
	WALLS = walls
	return start, end, nil
}
