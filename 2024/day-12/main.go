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

// Solution for Part 1 of the challenge
func Part1(input map[image.Point]rune) int {
	seen := make(map[image.Point]int)
	rtn := 0

	sortGarden := func(a, b image.Point) int {
		if a.X < b.X {
			return -1
		} else if a.X == b.X {
			if a.Y < b.Y {
				return -1
			} else if a.Y == b.Y {
				return 0
			} else {
				return 1
			}
		} else {
			return 1
		}
	}
	for pt := range input {
		if _, ok := seen[pt]; !ok {
			// New garden
			garden := []image.Point{pt}
			//slices.SortFunc(garden, sortGarden)
			for i := 0; i < len(garden); i++ {
				g := garden[i]
				seen[g]++
				neighbours := GetNeighbours(g, input, seen)
				for _, n := range neighbours {
					seen[n]++
				}
				garden = append(garden, neighbours...)
			}
			slices.SortFunc(garden, sortGarden)
			area := len(garden)
			perim := 0
			for _, g := range garden {
				newseen := make(map[image.Point]int)
				perim += 4 - len(GetNeighbours(g, input, newseen))
			}
			rtn += area * perim
		}
	}

	return rtn
}

func GetNeighbours(pt image.Point, gardens map[image.Point]rune, seen map[image.Point]int) []image.Point {
	pts := []image.Point{
		pt.Add(UP),
		pt.Add(DOWN),
		pt.Add(RIGHT),
		pt.Add(LEFT),
	}

	r := gardens[pt]
	return slices.DeleteFunc(pts, func(p image.Point) bool {
		v, ok := gardens[p]
		_, s := seen[p]
		// keep not seen and in bounds (ok) and same rune
		return s || !ok || v != r
	})
}

// Solution for Part 2 of the challenge
func Part2(input map[image.Point]rune) int {
	seen := make(map[image.Point]int)
	rtn := 0

	sortGarden := func(a, b image.Point) int {
		if a.X < b.X {
			return -1
		} else if a.X == b.X {
			if a.Y < b.Y {
				return -1
			} else if a.Y == b.Y {
				return 0
			} else {
				return 1
			}
		} else {
			return 1
		}
	}
	for pt := range input {
		if _, ok := seen[pt]; !ok {
			// New garden
			garden := []image.Point{pt}
			//slices.SortFunc(garden, sortGarden)
			for i := 0; i < len(garden); i++ {
				g := garden[i]
				seen[g]++
				neighbours := GetNeighbours(g, input, seen)
				for _, n := range neighbours {
					seen[n]++
				}
				garden = append(garden, neighbours...)
			}
			slices.SortFunc(garden, sortGarden)
			area := len(garden)
			n_corners := 0
			fmt.Println("====")
			for _, g := range garden {
				prev_corners := n_corners
				neighbours := GetNeighbours(g, input, map[image.Point]int{})
				if len(neighbours) == 4 {
					// check diagonals
					if !slices.Contains(garden, g.Add(LEFT).Add(UP)) {
						n_corners++
					}
					if !slices.Contains(garden, g.Add(LEFT).Add(DOWN)) {
						n_corners++
					}
					if !slices.Contains(garden, g.Add(RIGHT).Add(UP)) {
						n_corners++
					}
					if !slices.Contains(garden, g.Add(RIGHT).Add(DOWN)) {
						n_corners++
					}
				} else if len(neighbours) == 1 { // i.e edge
					n_corners += 2
				} else if len(neighbours) == 2 {
					// If the neighbours are above and below, or below and above
					if (slices.Contains(neighbours, g.Add(LEFT)) && slices.Contains(neighbours, g.Add(RIGHT))) || (slices.Contains(neighbours, g.Add(UP)) && slices.Contains(neighbours, g.Add(DOWN))) {
						n_corners += 0
					} else {
						// New corner
						n_corners += 1
						// if the garden contains a diagonal (i.e a square) dont add, otherwise add another corner
						n_intersect := Intersect(neighbours[0], neighbours[1], input)
						if n_intersect == 2 {
							n_corners += 0
						} else {
							// Another new corner
							n_corners += 1
						}
					}
				} else if len(neighbours) == 3 {
					// Check intersection
					ab := Intersect(neighbours[0], neighbours[1], input)
					ac := Intersect(neighbours[0], neighbours[2], input)
					bc := Intersect(neighbours[1], neighbours[2], input)

					if ab+ac+bc == 3 {
						n_corners += 2
					} else if ab+ac+bc == 4 {
						n_corners += 1
					} else if ab+ac+bc == 5 {
						n_corners += 0
					}
				}
				added_corners := n_corners - prev_corners
				if added_corners > 0 {
					fmt.Printf("Point %v added %d corners\n", g, added_corners)
				}
			}
			// If no corners
			var n_sides int
			if n_corners == 0 {
				n_sides = 4
			} else {
				n_sides = n_corners
			}
			fmt.Printf("Garden: %s, Area: %d, No. sides: %d\n", string(input[garden[0]]), area, n_sides)
			rtn += area * n_sides
		}
	}

	return rtn
}

func Intersect(x, y image.Point, gardens map[image.Point]rune) int {
	n1 := GetNeighbours(x, gardens, map[image.Point]int{})
	n2 := GetNeighbours(y, gardens, map[image.Point]int{})
	rtn := 0
	for _, i := range n1 {
		for _, j := range n2 {
			if i.Eq(j) {
				rtn++
			}
		}
	}
	return rtn
}

func SideCount(n_sides int, seen, left []image.Point, gardens map[image.Point]rune) int {
	if len(left) == 0 {
		return n_sides
	}

	g := left[0]
	neighbours := GetNeighbours(g, gardens, map[image.Point]int{})
	if len(neighbours) == 4 { // I.e. surrounded by points, no new sides
		n_sides += 0
	} else if len(neighbours) == 1 { // i.e continued direction
		n_sides += 0
	} else if len(neighbours) == 2 {
		// If the neighbours are above and below, or below and above
		if (slices.Contains(neighbours, g.Add(LEFT)) && slices.Contains(neighbours, g.Add(RIGHT))) || (slices.Contains(neighbours, g.Add(UP)) && slices.Contains(neighbours, g.Add(DOWN))) {
			n_sides += 0
		} else {
			if slices.Contains(seen, g.Add(LEFT).Add(UP)) || slices.Contains(seen, g.Add(LEFT).Add(DOWN)) || slices.Contains(seen, g.Add(LEFT).Add(UP)) || slices.Contains(seen, g.Add(LEFT).Add(DOWN)) {
			}
			n_sides += 2
		}
	} else if len(neighbours) == 3 {
		n_sides += 2
	}
	return SideCount(n_sides, append(seen, g), left[1:], gardens)
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (map[image.Point]rune, error) {
	rtn := make(map[image.Point]rune)

	x := 0
	y := 0

	max_x := 0

	for _, r := range input {
		if r == '\n' {
			if max_x == 0 {
				max_x = x
			}
			x = 0
			y++
			continue
		}

		rtn[image.Pt(x, y)] = r

		x++
	}

	BOUNDS = image.Rect(0, 0, max_x, y)

	return rtn, nil
}
