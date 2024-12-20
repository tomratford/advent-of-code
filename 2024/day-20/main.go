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
	"math"
	"os"
	"slices"
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

	start, end, walls, err := Parse(string(input))
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Println(Part1(start, end, walls))

	fmt.Println(Part2(start, end, walls))
}

// Structs and types

const (
	UP    = complex(0, -1)
	DOWN  = complex(0, 1)
	LEFT  = complex(1, 0)
	RIGHT = complex(-1, 0)
)

var BOUNDS complex128

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
func Part1(start, end complex128, walls map[complex128]int) int {
	// Get initial solution
	n1, prev, dist := Dijkstra(start, end, walls, getNeighbours)
	fmt.Println(n1)
	route := getRoute(prev, n1, start, end)
	rtn := make(map[int]int)
	// Delete each wall adjacent to route
	checked := make(map[[2]complex128]int)
	for i, r1 := range route {
		for _, r2 := range route[i:] {
			// If we are only two square away (i.e. can delete a wall)
			if x := r2 - r1; (math.Abs(real(x)) == 2 && imag(x) == 0) || (math.Abs(imag(x)) == 2 && real(x) == 0) {
				// potential wall
				w := complex((real(r1)+real(r2))/2, (imag(r1)+imag(r2))/2)
				if _, ok := walls[w]; ok {
					if _, ok := checked[[2]complex128{r1, r2}]; !ok {
						dist_saved := dist[r2] - dist[r1] - 2
						rtn[dist_saved]++
						checked[[2]complex128{r1, r2}]++
					}
				}
			}
		}
	}
	rtn2 := 0
	for k, v := range rtn {
		if k >= 100 {
			rtn2 += v
		}
	}
	return rtn2
}

func Dijkstra(start, end complex128, walls map[complex128]int, neighbour_fn func(complex128, map[complex128]int) []complex128) (int, map[complex128]complex128, map[complex128]int) {
	Q := make(PQ[complex128], 0, len(walls))

	dist := make(map[complex128]int)
	prev := make(map[complex128]complex128)

	dist[start] = 0
	Q.GPush(start, 0)

	for len(Q) > 0 {
		u, d := Q.GPop()

		if u == end {
			return d, prev, dist
		}

		moves := neighbour_fn(u, walls)
		for i := 0; i < len(moves); i++ {
			alt := d + 1
			v := moves[i]
			if v_dist, ok := dist[v]; !ok || (ok && alt < v_dist) {
				dist[v] = alt
				prev[v] = u
				Q.GPush(v, alt)
			}
		}
	}
	return -1, map[complex128]complex128{}, map[complex128]int{}
}

func getRoute(prev map[complex128]complex128, length int, start, end complex128) []complex128 {
	route := make([]complex128, 0, length)
	route = append(route, end)
	for {
		p := route[len(route)-1]
		if v, ok := prev[p]; ok {
			route = append(route, v)
		} else {
			break
		}
	}
	slices.Reverse(route)
	return route
}

func getNeighboursCheat(cheat complex128) func(complex128, map[complex128]int) []complex128 {
	return func(pt complex128, walls map[complex128]int) []complex128 {
		pts := []complex128{
			pt + UP,
			pt + DOWN,
			pt + RIGHT,
			pt + LEFT,
		}

		return slices.DeleteFunc(pts, func(p complex128) bool {
			_, ok := walls[p]
			// deletes if true so negate whole
			return !((!ok || ok && p == cheat) && real(p) >= 0 && real(p) <= real(BOUNDS) && imag(p) >= 0 && imag(p) <= imag(BOUNDS))
		})
	}
}

func getNeighbours(pt complex128, walls map[complex128]int) []complex128 {
	pts := []complex128{
		pt + UP,
		pt + DOWN,
		pt + RIGHT,
		pt + LEFT,
	}

	return slices.DeleteFunc(pts, func(p complex128) bool {
		_, ok := walls[p]
		// deletes if true so negate whole
		return !(!ok && real(p) >= 0 && real(p) <= real(BOUNDS) && imag(p) >= 0 && imag(p) <= imag(BOUNDS))
	})
}

// Solution for Part 2 of the challenge
func Part2(start, end complex128, walls map[complex128]int) int {
	// Get initial solution
	n1, prev, dist := Dijkstra(start, end, walls, getNeighbours)
	fmt.Println(n1)
	route := getRoute(prev, n1, start, end)
	rtn := make(map[int]int)
	// Delete each wall adjacent to route
	checked := make(map[[2]complex128]int)
	for i, r1 := range route {
		for _, r2 := range route[i:] {
			if d := manhattanDistance(r2, r1); d <= 20 && d >= 2 {
				if _, ok := checked[[2]complex128{r1, r2}]; !ok {
					dist_saved := dist[r2] - dist[r1] - int(d)
					rtn[dist_saved]++
					checked[[2]complex128{r1, r2}]++
				}
			}
		}
	}
	fmt.Println(rtn)
	rtn2 := 0
	for k, v := range rtn {
		if k >= 100 {
			rtn2 += v
		}
	}
	return rtn2
}

func manhattanDistance(c1, c2 complex128) float64 {
	// Extract real and imaginary parts
	real1, imag1 := real(c1), imag(c1)
	real2, imag2 := real(c2), imag(c2)

	// Calculate Manhattan distance
	return math.Abs(real1-real2) + math.Abs(imag1-imag2)
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (complex128, complex128, map[complex128]int, error) {
	x := 0.0
	y := 0.0

	max_x := 0.0

	rtn := make(map[complex128]int)

	var start, end complex128

	for _, r := range input {
		if r == '\n' {
			if max_x == 0 {
				max_x = x
			}
			x = 0
			y++
			continue
		}

		if r == 'S' {
			start = complex(x, y)
		} else if r == 'E' {
			end = complex(x, y)
		} else if r == '#' {
			rtn[complex(x, y)]++
		}
		x++
	}

	BOUNDS = complex(max_x, y+1)

	return start, end, rtn, nil
}
