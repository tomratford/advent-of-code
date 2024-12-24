/*
Advent of code; Year YYYY, Day XX

# Some notes on the challenge or solution here

usage:

	go run main.go path/to/input.txt
*/
package main

import (
	"fmt"
	"math"
	"os"
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
			Routes[[2]rune{k1, k2}] = Prune(get_route(Keypad, k1, k2), Keypad)
		}
	}

	for k1 := range Dirpad {
		for k2 := range Dirpad {
			Routes[[2]rune{k1, k2}] = Prune(get_route(Dirpad, k1, k2), Dirpad)
		}
	}

	fmt.Println(Part1(p))

	fmt.Println(Part2(p))
}

// Structs and types

var Routes = map[[2]rune][]string{}

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

// Find shortest routes between keypad values
func get_route(pad map[rune]complex128, start, end rune) []string {
	var inner func(c complex128, s string) []string
	inner = func(c complex128, s string) []string {
		exists := false
		curr := pad[end] - c
		for _, v := range pad {
			if v == curr {
				exists = true
				break
			}
		}
		if !exists {
			return []string{}
		} else if c == 0 {
			var b strings.Builder
			b.WriteString(s)
			b.WriteRune('A') // Append button presses
			return []string{b.String()}
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
		var num int
		var whatever string
		fmt.Sscanf(code, "%d%s", &num, &whatever)
		x := Part1Inner(code, 3)
		rtn += x * num
	}
	return rtn
}

func Part1Inner(s string, door int) int {
	if door == 0 {
		return len(s)
	}

	current := 'A'
	route_len := 0
	for _, c := range s {
		got := Routes[[2]rune{current, c}]
		min := math.MaxInt
		for _, g := range got {
			if got := Part1Inner(g, door-1); got < min {
				min = got
			}
		}
		route_len += min
		current = c
	}
	return route_len
}

// Solution for Part 2 of the challenge
func Part2(input []string) int {
	rtn := 0
	for _, code := range input {
		var num int
		var whatever string
		fmt.Sscanf(code, "%d%s", &num, &whatever)
		x := Part2Inner(code, 26)
		rtn += x * num
	}
	return rtn
}

type Key struct {
	string
	int
}

var SEEN = map[Key]int{}

func Part2Inner(s string, door int) int {
	if v, ok := SEEN[Key{s, door}]; ok {
		return v
	}

	if door == 0 {
		return len(s)
	}

	current := 'A'
	route_len := 0
	for _, c := range s {
		got := Routes[[2]rune{current, c}]
		min := math.MaxInt
		for _, g := range got {
			if got := Part2Inner(g, door-1); got < min {
				min = got
			}
		}
		route_len += min
		current = c
	}
	SEEN[Key{s, door}] = route_len
	return route_len
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([]string, error) {
	return strings.Split(input, "\n"), nil
}
