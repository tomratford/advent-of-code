/*
Advent of code; Year YYYY, Day XX

# Some notes on the challenge or solution here

usage:

	go run main.go path/to/input.txt
*/
package main

import (
	"fmt"
	"maps"
	"math"
	"os"
	"slices"
	"strconv"
	"strings"
)

var STOP_CRITERIA = 10

func main() {
	if len(os.Args) < 2 {
		fmt.Println("usage: go run main.go path/to/input.txt")
		return
	}

	if os.Args[1] == "input.txt" {
		STOP_CRITERIA = 1000
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

	fmt.Println(Solve(p))
}

// Structs and types

type ThreePoint struct {
	x, y, z int
}

func Euclid(a, b ThreePoint) float64 {
	// sqrt(sum((a - b)^2))
	sum := math.Pow(float64(a.x-b.x), 2.0) + math.Pow(float64(a.y-b.y), 2.0) + math.Pow(float64(a.z-b.z), 2.0)
	return math.Sqrt(float64(sum))
}

type Edge struct {
	a, b   ThreePoint
	length float64
}

func GetEdges(input []ThreePoint) []Edge {
	edges := make([]Edge, 0, (len(input)^2)/2)
	for i, c := range input {
		for _, k := range input[i+1:] {
			edges = append(edges, Edge{c, k, Euclid(c, k)})
		}
	}
	return edges
}

func CmpEdge(a, b Edge) int {
	if a.length < b.length {
		return -1
	} else if a.length > b.length {
		return 1
	} else {
		return 0
	}
}

// Implement disjoint set for Kruskal's Algorithm
// https://en.wikipedia.org/wiki/Kruskal%27s_algorithm
// https://en.wikipedia.org/wiki/Disjoint-set_data_structure

var FOREST = make([]*Element, 0)

type Element struct {
	parent *Element
	rank   int
	data   ThreePoint
}

func MakeSet(x ThreePoint) {
	if slices.IndexFunc(FOREST, func(e *Element) bool {
		return e.data == x
	}) == -1 {
		e := &Element{}
		e.parent = e
		e.data = x
		FOREST = append(FOREST, e)
	}
}

func Find(x *Element) *Element {
	if x.parent != x {
		x.parent = Find(x.parent)
		return x.parent
	} else {
		return x
	}
}

func Union(x, y *Element) {
	x = Find(x)
	y = Find(y)

	if x.data == y.data {
		return
	}

	if x.rank < y.rank {
		x.parent = y
	} else {
		y.parent = x
		if x.rank == y.rank {
			x.rank++
		}
	}
}

func Search(x ThreePoint) *Element {
	for _, f := range FOREST {
		if f.data == x {
			return f
		}
	}
	return nil
}

// Solution for the challenge
func Solve(input []ThreePoint) int {
	edges := GetEdges(input)
	slices.SortFunc(edges, CmpEdge)
	// Implement Kruskal Algorithm
	for _, v := range input {
		MakeSet(v)
	}

	part2 := 0
	for i, e := range edges {
		if i == STOP_CRITERIA {
			F2 := make([]*Element, len(FOREST))
			for i, f := range FOREST {
				F2[i] = Find(f)
			}

			score := make(map[*Element]int)
			for _, f := range F2 {
				if _, ok := score[f]; !ok {
					score[f] = 1
				} else {
					score[f]++
				}
			}

			sorted_score := slices.Collect(maps.Values(score))
			slices.Sort(sorted_score)
			slices.Reverse(sorted_score)

			part1 := 1
			for _, s := range sorted_score[0:3] {
				part1 *= s
			}
			fmt.Println(part1)
		}
		u := Search(e.a)
		v := Search(e.b)
		if Find(u) != Find(v) {
			Union(u, v)
			part2 = u.data.x * v.data.x
		}
	}

	return part2
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([]ThreePoint, error) {
	lines := strings.Split(input, "\n")
	points := make([]ThreePoint, 0, len(lines))
	for _, l := range lines {
		if l == "" {
			continue
		}
		numbers := strings.Split(l, ",")
		n_number := make([]int, 0, 3)
		for _, number := range numbers {
			num, err := strconv.Atoi(number)
			if err != nil {
				return []ThreePoint{}, fmt.Errorf("number couldn't parse in line %q: %v", l, err)
			}
			n_number = append(n_number, num)
		}
		points = append(points, ThreePoint{n_number[0], n_number[1], n_number[2]})
	}
	return points, nil
}
