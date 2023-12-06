package main

import (
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
	"unicode"
	"slices"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Println("No file passed")
		return
	}

	input, err := os.ReadFile(os.Args[1])
	if err != nil {
		fmt.Println(err)
		return
	}

	numbers, partpoints, err := Parse(string(input))
	if err != nil {
		fmt.Println(err)
		return
	}
	
	fmt.Println(Part1(numbers, partpoints))

	gears := ParseGears(string(input))
	fmt.Println(Part2(numbers, gears))
}

func Part1(numbers []Number, points []Point) int {
	partial := 0
	for _, number := range numbers {
		if slices.ContainsFunc(points, func(p1 Point) bool {
			return slices.ContainsFunc(number.indices, func(p2 Point) bool {
				return Adjacent(p1, p2)
			})
		}) {
			partial = partial + number.number
		}
	}
	return partial
}

func Part2(numbers []Number, gears []Point) int {
	partial := 0
	for _, gear := range gears {
		index := 0
		temp := [2]int{} // to store the two numbers in
		for _, number := range numbers { // For a number
			if slices.ContainsFunc(number.indices, func(p Point) bool { // If any point it next to a gear
				return Adjacent(p, gear) 
			}) {
				temp[index] = number.number // save it
				index = index + 1 // For the second number
			}
		}
		if index == 2 {
			partial = partial + (temp[0] * temp[1])
		}
	}
	return partial
}

type Number struct {
	number int
	indices []Point
}

func Parse(input string) ([]Number, []Point, error) {
	numbers := []Number{}
	part_points := []Point{}

	var i float64 = 1
	var j float64 = 1
	
	var temp strings.Builder
	temp_points := []Point{}
	
	for _, char := range input {
		switch {
		case unicode.IsDigit(char):
			temp.WriteRune(char)
			temp_points = append(temp_points, Point{i,j})
		case char == '\n':
			i = 0
			j = j + 1
		default:
			if num_str := temp.String(); num_str != "" {
				num, err := strconv.Atoi(num_str)
				if err != nil {
					return []Number{}, []Point{}, err
				}
				numbers = append(numbers, Number{
					number: num,
					indices: slices.Clone(temp_points),
				})
				temp.Reset()
				temp_points = slices.Delete(temp_points, 0, len(temp_points))
			}
			if char != '.' {
				part_points = append(part_points, Point{i,j})
			}
		}
		i = i + 1
	}
	
	return numbers, part_points, nil
}

func ParseGears(input string) ([]Point) {
	gears := []Point{}
	var i float64 = 1
	var j float64 = 1
	for _, char := range input {
		if char == '*' {
			gears = append(gears, Point{i,j})
		} else if char == '\n' {
			i = 0
			j = j + 1
		}
		i = i + 1
	}
	return gears
}

type Point struct {
	x float64
	y float64
}

func Adjacent(p1 Point, p2 Point) bool {
	diff := Point{
		x: math.Abs(p2.x - p1.x),
		y: math.Abs(p2.y - p1.y),
	}

	// This absolutely will be a nightmare
	if math.Sqrt(math.Pow(diff.x,2) + math.Pow(diff.y,2)) < 2 {
		return true
	} else {
		return false
	}
}
