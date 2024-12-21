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
	"os"
	"slices"
	"strings"
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

	for k1 := range Keypad {
		for k2 := range Keypad {
			KeypadRoutes[[2]rune{k1, k2}] = get_route(Keypad, k1, k2)
		}
	}

	for k1 := range Dirpad {
		for k2 := range Dirpad {
			DirpadRoutes[[2]rune{k1, k2}] = get_route(Dirpad, k1, k2)
		}
	}

	fmt.Println(Part1(p[:1]))
}

// Structs and types

var KeypadRoutes = map[[2]rune][]string{}
var DirpadRoutes = map[[2]rune][]string{}

var Keypad = map[rune]complex128{
	'7': 0 + 0i,
	'8': 1 + 0i,
	'9': 2 + 0i,
	'4': 0 + 1i,
	'5': 1 + 1i,
	'6': 2 + 1i,
	'1': 0 + 2i,
	'2': 1 + 2i,
	'3': 2 + 2i,
	'0': 1 + 3i,
	'A': 2 + 3i,
}

var Dirpad = map[rune]complex128{
	'^': 1 + 0i,
	'A': 2 + 0i,
	'<': 0 + 1i,
	'v': 1 + 1i,
	'>': 2 + 1i,
}

func get_route(pad map[rune]complex128, start, end rune) []string {
	var inner func(c complex128, s string) []string
	inner = func(c complex128, s string) []string {
		exists := false
		curr := pad[start] + c
		for v := range maps.Values(pad) {
			if v == curr {
				exists = true
				break
			}
		}
		if !exists {
			return []string{}
		} else if c == 0 {
			return []string{s}
		} else {
			rtn := []string{}
			if real(c) != 0 {
				if real(c) > 0 {
					var b strings.Builder
					b.WriteString(s)
					b.WriteRune('>')
					rtn = append(rtn, inner(c-1, b.String())...)
				} else {
					var b strings.Builder
					b.WriteString(s)
					b.WriteRune('<')
					rtn = append(rtn, inner(c+1, b.String())...)
				}
			}
			if imag(c) != 0 {
				if imag(c) > 0 {
					var b strings.Builder
					b.WriteString(s)
					b.WriteRune('v')
					rtn = append(rtn, inner(c-1i, b.String())...)
				} else {
					var b strings.Builder
					b.WriteString(s)
					b.WriteRune('^')
					rtn = append(rtn, inner(c+1i, b.String())...)
				}
			}
			return rtn
		}
	}
	diff := pad[end] - pad[start]
	return inner(diff, "")
}

var SEEN = map[string][]string{}

func GetRoutes(route_map map[[2]rune][]string, route string) []string {
	if v, ok := SEEN[route]; ok {
		return v
	}
	if len(route) == 1 {
		return []string{route[:len(route)-1]}
	} else {
		code := [2]rune{rune(route[0]), rune(route[1])}
		add := route_map[code]
		rtn := []string{}
		for _, a := range add {
			for _, route := range GetRoutes(route_map, route[1:]) {
				var b strings.Builder
				b.WriteString(a)
				b.WriteRune('A')
				b.WriteString(route)
				rtn = append(rtn, b.String())
			}
		}
		SEEN[route] = rtn
		return rtn
	}
}

func unique[T comparable](s []T) []T {
	rtn := make(map[T]int)
	for _, t := range s {
		rtn[t]++
	}
	return slices.Collect(maps.Keys(rtn))
}

// keep strings with mose repeated values
func Prune(routes []string, pad map[rune]complex128) []string {
	scores := make(map[int][]string)
	for _, s := range routes {
		changes := 0
		for i := 1; i < len(s); i++ {
			if s[i-1] != s[i] {
				changes += 100
			}
		}
		if _, ok := scores[changes]; ok {
			scores[changes] = append(scores[changes], s)
		} else {
			scores[changes] = []string{s}
		}
	}

	// get min
	min := -1
	for s := range scores {
		if min == -1 {
			min = s
		} else {
			if s < min {
				min = s
			}
		}
	}
	return scores[min]
}

// Solution for Part 1 of the challenge
func Part1(input []string) int {
	rtn := 0
	for _, code := range input {
		var b strings.Builder
		b.WriteRune('A')
		b.WriteString(code)
		// Get instructions for robot at keypad
		keypad_routes := Prune(GetRoutes(KeypadRoutes, b.String()), Dirpad)
		fmt.Println(keypad_routes)
		// Get instructions for robot at 1st dirpad
		dirpad1_routes := make([]string, 0, len(keypad_routes)*3)
		for _, kr := range keypad_routes {
			dirpad1_routes = append(dirpad1_routes, GetRoutes(DirpadRoutes, kr)...)
		}
		dirpad1_routes = Prune(dirpad1_routes, Dirpad)
		fmt.Println(dirpad1_routes)
		// Get instructions for robot at 2nd dirpad
		dirpad2_routes := make([]string, 0, len(dirpad1_routes)*3)
		for _, dr := range dirpad1_routes {
			dirpad2_routes = append(dirpad2_routes, GetRoutes(DirpadRoutes, dr)...)
		}
		dirpad2_routes = Prune(dirpad2_routes, Dirpad)
		//fmt.Println(dirpad2_routes)
		fmt.Println(len(dirpad2_routes))
		// Get instructions for 'me' at the final dirpad
		my_instructions := make([]string, 0, len(dirpad2_routes)*3)
		for _, dr := range dirpad2_routes {
			my_instructions = append(my_instructions, GetRoutes(DirpadRoutes, dr)...)
		}
		// Get smallest length instructions
		smallest_len := 999_999_999
		smallest_len_k := 0
		for k, i := range my_instructions {
			if len(i) < smallest_len {
				smallest_len = len(i)
				smallest_len_k = k
			}
		}
		fmt.Println(code, my_instructions[smallest_len_k])
		rtn += smallest_len
	}
	return rtn
}

// Solution for Part 2 of the challenge
func Part2(input string) int {
	return 1
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([]string, error) {
	return strings.Split(input, "\n"), nil
}
