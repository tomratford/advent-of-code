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
	"strings"
	"time"
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

	walls, boxes, robot, instructions, err := Parse(string(input))
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Println(Part1(walls, boxes, robot, instructions))
	fmt.Println(Part2(string(input)))
}

// Structs and types

var (
	UP    = image.Pt(0, -1)
	DOWN  = image.Pt(0, 1)
	RIGHT = image.Pt(1, 0)
	LEFT  = image.Pt(-1, 0)
)

var BOUNDS image.Rectangle

type Robot struct {
	pos image.Point
}

func (r *Robot) move(dir image.Point, walls map[image.Point]int, boxes map[image.Point]image.Point) {
	newpos := r.pos.Add(dir)
	if _, ok := walls[newpos]; ok {
		return
	}
	if v, ok := boxes[newpos]; ok {
		if v.Eq(image.Point{}) {
			if !move_box(newpos, dir, walls, boxes) {
				return
			}
		} else { // Part 2 move
			if !move_boxes(newpos, dir, walls, boxes) {
				return
			}
		}
	}

	r.pos = newpos
}

func move_box(pos, dir image.Point, walls map[image.Point]int, boxes map[image.Point]image.Point) bool {
	newpos := pos.Add(dir)
	if _, ok := walls[newpos]; ok {
		return false
	}
	if _, ok := boxes[newpos]; ok && !move_box(newpos, dir, walls, boxes) {
		return false
	}

	delete(boxes, pos)
	boxes[newpos] = image.Point{}

	return true
}

func move_boxes(pos1, dir image.Point, walls map[image.Point]int, boxes map[image.Point]image.Point) bool {
	pos2 := boxes[pos1]
	newpos1 := pos1.Add(dir)
	newpos2 := pos2.Add(dir)
	if _, ok := walls[newpos1]; ok {
		return false
	}
	if _, ok := walls[newpos2]; ok {
		return false
	}

	old_boxes := make(map[image.Point]image.Point)
	for k, v := range boxes {
		old_boxes[k] = v
	}

	newbox1, ok1 := boxes[newpos1]
	newbox2, ok2 := boxes[newpos2]
	if ok1 && ok2 {
		if newbox1.Eq(pos1) {
			// moving horizontal, need to make sure we don't pick the box we are on (else infinite recursion)
			if !move_boxes(newbox2, dir, walls, boxes) {
				return false
			}
		} else if newbox2.Eq(pos2) {
			// moving horizontal, need to make sure we don't pick the box we are on (else infinite recursion)
			if !move_boxes(newbox1, dir, walls, boxes) {
				return false
			}
		} else if boxes[newbox1].Eq(newbox2) {
			// two boxes perfectly stacked vertically
			if !move_boxes(newbox1, dir, walls, boxes) {
				return false
			}
		} else {
			// Two boxes imperfectly stacked
			movebox1 := move_boxes(newbox1, dir, walls, boxes)
			movebox2 := move_boxes(newbox2, dir, walls, boxes)
			if (!movebox1) || (!movebox2) {
				boxes = old_boxes
				return false
			}
		}
	} else if ok1 { // single box imperfectly stacked
		if !newbox1.Eq(pos1) {
			if !move_boxes(newbox1, dir, walls, boxes) {
				return false
			}
		}
	} else if ok2 {
		if !newbox2.Eq(pos2) {
			if !move_boxes(newbox2, dir, walls, boxes) {
				return false
			}
		}
	}

	delete(boxes, pos1)
	delete(boxes, pos2)
	boxes[newpos1] = newpos2
	boxes[newpos2] = newpos1
	return true
}

// Solution for Part 1 of the challenge
func Part1(walls map[image.Point]int, boxes map[image.Point]image.Point, robot Robot, instructions []image.Point) int {
	for _, i := range instructions {
		robot.move(i, walls, boxes)
	}
	rtn := 0
	for p := range boxes {
		rtn += p.X + 100*p.Y
	}
	return rtn
}

// Solution for Part 2 of the challenge
func Part2(input string) int {
	dbl_hash := strings.Replace(input, "#", "##", -1)
	dbl_o := strings.Replace(dbl_hash, "O", "[]", -1)
	dbl_dot := strings.Replace(dbl_o, ".", "..", -1)
	dbl_rbt := strings.Replace(dbl_dot, "@", "@.", -1)

	walls, boxes, robot, instructions, _ := Parse(dbl_rbt)

	//PrettyPrint(walls, boxes, robot)

	for _, i := range instructions {
		robot.move(i, walls, boxes)
		PrettyPrint(walls, boxes, robot)
		time.Sleep(50 * time.Millisecond)
	}

	rtn := 0
	for p, v := range boxes {
		if p.X < v.X {
			rtn += p.X + 100*p.Y
		}
	}
	return rtn
}

func PrettyPrint(walls map[image.Point]int, boxes map[image.Point]image.Point, robot Robot) {
	for j := 0; j < BOUNDS.Max.Y; j++ {
		for i := 0; i < BOUNDS.Max.X; i++ {
			pt := image.Pt(i, j)
			if robot.pos.Eq(pt) {
				fmt.Print("@")
			} else if _, ok := walls[pt]; ok {
				fmt.Print("#")
			} else if v, ok := boxes[pt]; ok {
				if v.Eq(image.Point{}) {
					fmt.Print("O")
				} else if v.X < i {
					fmt.Print("]")
				} else {
					fmt.Print("[")
				}
			} else {
				fmt.Print(".")
			}
		}
		fmt.Print("\n")
	}
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (map[image.Point]int, map[image.Point]image.Point, Robot, []image.Point, error) {
	walls := make(map[image.Point]int)
	boxes := make(map[image.Point]image.Point)

	robot := Robot{}

	sections := strings.Split(input, "\n\n")

	x := 0
	y := 0
	max_x := 0
	for _, r := range sections[0] {
		if r == '\n' {
			if max_x == 0 {
				max_x = x
			}
			x = 0
			y++
			continue
		}

		if r == '#' {
			walls[image.Pt(x, y)]++
		}

		if r == 'O' {
			boxes[image.Pt(x, y)] = image.Point{}
		}

		// Part 2 bit
		if r == '[' {
			boxes[image.Pt(x, y)] = image.Pt(x+1, y)
		}
		if r == ']' {
			boxes[image.Pt(x, y)] = image.Pt(x-1, y)
		}

		if r == '@' {
			robot.pos = image.Pt(x, y)
		}

		x++
	}

	BOUNDS = image.Rect(0, 0, max_x, y+1)

	instructions := make([]image.Point, 0, len(sections[1]))
	for _, r := range sections[1] {
		if r == '\n' {
			continue
		}
		switch r {
		case '<':
			instructions = append(instructions, LEFT)
		case '>':
			instructions = append(instructions, RIGHT)
		case '^':
			instructions = append(instructions, UP)
		case 'v':
			instructions = append(instructions, DOWN)
		}
	}
	return walls, boxes, robot, instructions, nil
}
