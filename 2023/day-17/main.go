/*
Advent of code; Year 2023, Day 17

	As I see this, a Djikstra solution with a alternate "getNeighbours" function of
	sorts should be more than enough to sastify this challenge

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

var LEFT = image.Point{1, 0}
var RIGHT = image.Point{-1, 0}
var DOWN = image.Point{0, 1}
var UP = image.Point{0, -1}

var BOUNDS image.Rectangle

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
	dist, prev := Dijkstra(graph, source, target)

	p := target
	route := make([]image.Point, 0, len(graph))
	for {
		route = append(route, p)
		v, ok := prev[p]
		if !ok {
			break
		}
		p = v
	}

	prettyPrint(graph, route)

	return dist[target]
}

func Dijkstra(graph map[image.Point]int, source image.Point, target image.Point) (map[image.Point]int, map[image.Point]image.Point) {

	dist := make(map[image.Point]int, len(graph))
	prev := make(map[image.Point]image.Point, len(graph))

	Q := make([]image.Point, 0, len(graph))

	for v := range graph {
		dist[v] = 999_999_999
		Q = append(Q, v)
	}
	dist[source] = 0

	for len(Q) > 0 {

		// u ← vertex in Q with minimum dist[u]
		var u image.Point
		mindist := -1
		for _, k := range Q {
			v, ok := dist[k]
			if ok && (v < mindist || mindist == -1) {
				mindist = v
				u = k
			}
		}

		i := slices.Index(Q, u)
		if i == -1 {
			fmt.Println(Q, u)
		}
		if i+1 >= len(Q) {
			Q = Q[:i]
		} else {
			Q = append(Q[:i], Q[i+1:]...)
		}

		neighbours := getNeighbours(u, Q, prev)
		// for each neighbor v of u still in Q
		for _, v := range neighbours {
			if alt := dist[u] + graph[v]; alt < dist[v] {
				dist[v] = alt
				prev[v] = u
			}
		}
	}

	return dist, prev
}

func getNeighbours(u image.Point, Q []image.Point, prev map[image.Point]image.Point) []image.Point {
	inner := func(n image.Point) bool {
		return !(slices.Contains(Q, n) && n.In(BOUNDS))
	}
	neighbours := map[image.Point]image.Point{
		LEFT:  u.Add(LEFT),
		RIGHT: u.Add(RIGHT),
		DOWN:  u.Add(DOWN),
		UP:    u.Add(UP),
	}
	// u is 1 behind the future
	if u1, ok := prev[u]; ok { // 2 behind
		if u2, ok := prev[u1]; ok { // 3 behind
			if u3, ok := prev[u2]; ok { // 4 behind
				for k := range neighbours {
					d := u3.Sub(neighbours[k])
					if d.X == 0 || d.Y == 0 {
						delete(neighbours, k)
					}
				}
			} else {
				for k := range neighbours {
					d := u3.Sub(neighbours[k])
					if d.X == 0 || d.Y == 0 {
						delete(neighbours, k)
					}
				}
			}
		}
	}
	return slices.DeleteFunc(slices.Collect(maps.Values(neighbours)), inner)
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
