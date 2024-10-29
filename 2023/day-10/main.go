/*
Advent of code; Year YYYY, Day XX

# Some notes on the challenge or solution here

usage:

	go run main.go path/to/input.txt
*/
package main

import (
	"container/list"
	"fmt"
	"image"
	"maps"
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

	pipes, start, err := Parse(string(input))
	if err != nil {
		fmt.Println(err)
		return
	}

	Part1(pipes, start)
	Part2(pipes, start)
}

// Structs and types

/* Any structs required for the challenge go here */

// Solution for Part 1 of the challenge
func Part1(pipes map[image.Point]rune, start image.Point) int {
	visited, _ := BFS(pipes, start)
	fmt.Println(len(visited) / 2)
	return 1
}

/*
Depth First Search to find cycle
*/
func DFS(pipes map[image.Point]rune, start image.Point) ([]image.Point, map[image.Point]image.Point) {
	S := make([]image.Point, 0, len(pipes))
	S = append(S, start)
	visited := make(map[image.Point]bool, len(pipes))
	parents := make(map[image.Point]image.Point, len(pipes))
	for len(S) > 0 {
		v := S[len(S)-1]
		S = S[0 : len(S)-1]
		if _, ok := visited[v]; !ok {
			visited[v] = true
			adjacent := getNeighbours(pipes, v)
			for _, w := range adjacent {
				if _, ok := visited[w]; !ok {
					parents[w] = v
					S = append(S, w)
				}
			}
		}
	}
	return slices.Collect(maps.Keys(visited)), parents
}

/*
Breadths First Search to find cycle
*/
func BFS(pipes map[image.Point]rune, start image.Point) ([]image.Point, map[image.Point]image.Point) {
	Q := list.New()
	Q.PushBack(start)
	visited := make(map[image.Point]bool, len(pipes))
	visited[start] = true
	parents := make(map[image.Point]image.Point, len(pipes))
	for Q.Len() > 0 {
		v := Q.Front()
		v_pt := v.Value.(image.Point)
		Q.Remove(Q.Front())
		adjacent := getNeighbours(pipes, v_pt)
		for _, w := range adjacent {
			if _, ok := visited[w]; ok {
				continue
			}
			visited[w] = true
			parents[w] = v_pt
			Q.PushBack(w)
		}
	}
	return slices.Collect(maps.Keys(visited)), parents
}

func getNeighbours(pipes map[image.Point]rune, p image.Point) []image.Point {
	cs := make([]image.Point, 0, 4) // possible connections
	up := p.Add(image.Pt(0, -1))
	down := p.Add(image.Pt(0, 1))
	left := p.Add(image.Pt(-1, 0))
	right := p.Add(image.Pt(1, 0))
	switch val := pipes[p]; val {
	case 'S':
		{
			if c := pipes[up]; c == '|' || c == '7' || c == 'F' {
				cs = append(cs, up)
			}
			if c := pipes[down]; c == '|' || c == 'L' || c == 'J' {
				cs = append(cs, down)
			}
			if c := pipes[left]; c == '-' || c == 'L' || c == 'F' {
				cs = append(cs, left)
			}
			if c := pipes[right]; c == '-' || c == '7' || c == 'J' {
				cs = append(cs, right)
			}
		}
	case '|':
		{
			if c := pipes[up]; c == '|' || c == '7' || c == 'F' {
				cs = append(cs, up)
			}
			if c := pipes[down]; c == '|' || c == 'L' || c == 'J' {
				cs = append(cs, down)
			}
		}
	case '-':
		{
			if c := pipes[left]; c == '-' || c == 'L' || c == 'F' {
				cs = append(cs, left)
			}
			if c := pipes[right]; c == '-' || c == '7' || c == 'J' {
				cs = append(cs, right)
			}
		}
	case 'L':
		{
			if c := pipes[up]; c == '|' || c == '7' || c == 'F' {
				cs = append(cs, up)
			}
			if c := pipes[right]; c == '-' || c == '7' || c == 'J' {
				cs = append(cs, right)
			}
		}
	case 'J':
		{
			if c := pipes[up]; c == '|' || c == '7' || c == 'F' {
				cs = append(cs, up)
			}
			if c := pipes[left]; c == '-' || c == 'L' || c == 'F' {
				cs = append(cs, left)
			}
		}
	case '7':
		{
			if c := pipes[down]; c == '|' || c == 'L' || c == 'J' {
				cs = append(cs, down)
			}
			if c := pipes[left]; c == '-' || c == 'L' || c == 'F' {
				cs = append(cs, left)
			}
		}
	case 'F':
		{
			if c := pipes[down]; c == '|' || c == 'L' || c == 'J' {
				cs = append(cs, down)
			}
			if c := pipes[right]; c == '-' || c == '7' || c == 'J' {
				cs = append(cs, right)
			}
		}
	case '.':
		{
			// Do nothing
		}
	}
	return cs
}

// Solution for Part 2 of the challenge
func Part2(pipes map[image.Point]rune, start image.Point) int {
	_, parents := BFS(pipes, start)
	routemap, otherPoint := reverseMap(parents, start)
	route := makeRoute(routemap, otherPoint, start)
	edges := getEdges(pipes, route)
	twoA := shoelace(edges)
	A := twoA / 2
	if A < 0 {
		A *= -1
	}
	b := len(route) + 1
	i := A + 1 - b/2
	fmt.Println(i)
	return 1
}

func shoelace(edges []image.Point) int {
	edges = append(edges, edges[0])
	twoA := 0
	for i := 0; i < len(edges)-1; i++ {
		twoA += det(edges[i], edges[i+1])
	}
	return twoA
}

// Simple det function for shoelace
func det(x, y image.Point) int {
	return x.X*y.Y - x.Y*y.X
}

/*
Get edges/direction changes

Filters based on whether it's a turn in the route
*/
func getEdges(pipes map[image.Point]rune, route []image.Point) []image.Point {
	x := make([]image.Point, 0, len(route))
	for _, p := range route {
		v := pipes[p]
		if v == '-' || v == '|' {
			// do nothing
		} else {
			x = append(x, p)
		}
	}
	return x
}

// Make route
func makeRoute(routemap map[image.Point]image.Point, otherPoint image.Point, start image.Point) []image.Point {
	route := make([]image.Point, 0, len(routemap))
	route = append(route, start)
	point := start
	for {
		newpoint, ok := routemap[point]
		if !ok {
			break
		}
		route = append(route, newpoint)
		point = newpoint
	}
	route2 := make([]image.Point, 0, len(routemap))
	route2 = append(route2, otherPoint)
	point = otherPoint
	for {
		newpoint, ok := routemap[point]
		if !ok {
			break
		}
		route2 = append(route2, newpoint)
		point = newpoint
	}
	slices.Reverse(route2)
	route = append(route, route2...)
	return route
}

// Reverse map
func reverseMap[T comparable, V comparable](x map[T]V, start V) (map[V]T, T) {
	y := make(map[V]T, len(x))
	var altRoute T
	for key, value := range x {
		if oldval, check := y[value]; check {
			if value != start {
				panic("Not unique")
			} else {
				altRoute = oldval
			}
		}
		y[value] = key
	}
	return y, altRoute
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (map[image.Point]rune, image.Point, error) {
	p := make(map[image.Point]rune)
	x := 0
	y := 0
	var start image.Point
	for _, v := range input {
		if v == 'S' {
			start = image.Pt(x, y)
		}
		p[image.Pt(x, y)] = v
		x += 1
		if v == '\n' {
			x = 0
			y += 1
		}
	}
	return p, start, nil
}
