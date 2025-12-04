/*
Advent of code; Year 2025, Day 04

# Making a fun gif thing instead

usage:

	go run main.go path/to/input.txt
*/
package main

import (
	"fmt"
	"image"
	"image/color"
	"image/gif"
	"os"
	"strings"
)

var BOUNDS image.Rectangle
var GIF_FRAMES []*image.Paletted

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

	fmt.Println(Solve(p, 0))
	MakeGif()
}

// Structs and types

func GetAdj(c image.Point) []image.Point {
	out := make([]image.Point, 0, 8)
	for i := -1; i <= 1; i++ {
		for j := -1; j <= 1; j++ {
			if i == 0 && j == 0 {
				continue
			}
			out = append(out, image.Point{c.X + i, c.Y + j})
		}
	}
	return out
}

func CopyMap[K comparable, V any](m map[K]V) map[K]V {
	if m == nil {
		return nil
	}
	out := make(map[K]V, len(m))
	for k, v := range m {
		out[k] = v
	}
	return out
}

func CompMap[K, V comparable](a, b map[K]V) bool {
	if a == nil || b == nil {
		return a == nil && b == nil
	}
	if len(a) != len(b) {
		return false
	}
	for k, va := range a {
		vb, ok := b[k]
		if !ok {
			return false
		}
		if va != vb {
			return false
		}
	}
	return true
}

// Solution for Part 2 of the challenge
func Solve(input map[image.Point]rune, iter int) int {
	ImageFromMap(input)
	score := 0
	input_copy := CopyMap(input)
	for k, v := range input {
		if v == '.' {
			continue
		} else {
			count := 0
			for _, a := range GetAdj(k) {
				if input[a] == '@' {
					count += 1
				}
			}
			if count < 4 {
				input_copy[k] = '.'
				score++
			}
		}
	}
	if !CompMap(input, input_copy) {
		return score + Solve(input_copy, iter+1)
	} else {
		fmt.Println(iter)
		return score
	}
}

func ImageFromMap(m map[image.Point]rune) {
	img := image.NewPaletted(BOUNDS, color.Palette{color.Black, color.White})
	for j := 0; j < BOUNDS.Max.Y; j++ {
		for i := 0; i < BOUNDS.Max.X; i++ {
			if m[image.Point{i, j}] == '.' {
				img.Set(i, j, color.Black)
			} else {
				img.Set(i, j, color.White)
			}
		}
	}
	if GIF_FRAMES == nil {
		GIF_FRAMES = make([]*image.Paletted, 0, 60)
	}
	GIF_FRAMES = append(GIF_FRAMES, img)
}

func MakeGif() {
	f, err := os.Create("sol.gif")
	if err != nil {
		panic(err)
	}

	delay := make([]int, len(GIF_FRAMES))
	for i := range delay {
		delay[i] = 10
	}

	img := &gif.GIF{
		GIF_FRAMES,
		delay,
		0,
		nil,
		image.Config{},
		0,
	}

	if err := gif.EncodeAll(f, img); err != nil {
		f.Close()
		panic(err)
	}

	if err := f.Close(); err != nil {
		panic(err)
	}
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (map[image.Point]rune, error) {
	lines := strings.Split(input, "\n")
	grid := make(map[image.Point]rune)
	for j, l := range lines {
		if l == "" {
			continue
		}
		for i, c := range l {
			grid[image.Point{i, j}] = c
			BOUNDS = image.Rect(0, 0, i, j)
		}
	}
	return grid, nil
}
