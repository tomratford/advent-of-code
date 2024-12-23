/*
Advent of code; Year YYYY, Day XX

# Some notes on the challenge or solution here

usage:

	go run main.go path/to/input.txt
*/
package main

import (
	"fmt"
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

	fmt.Println(Part1(p))
}

// Structs and types

type Set[T comparable] struct {
	data map[T]int
}

func (s *Set[T]) Len() int {
	return len(s.data)
}

func (s *Set[T]) String() string {
	return fmt.Sprintf("%v", s.Values())
}

func NewSet[T comparable](data ...T) *Set[T] {
	m := make(map[T]int)
	for _, s := range data {
		m[s]++
	}
	return &Set[T]{
		data: m,
	}
}

func (s *Set[T]) Add(data ...T) {
	for _, v := range data {
		s.data[v]++
	}
}

func Intersect[T comparable](s1, s2 *Set[T]) *Set[T] {
	s := NewSet[T]()
	for k1 := range s1.data {
		for k2 := range s2.data {
			if k1 == k2 {
				s.Add(k1)
			}
		}
	}
	return s
}

func (s *Set[T]) Values() []T {
	rtn := make([]T, 0, len(s.data))
	for k := range s.data {
		rtn = append(rtn, k)
	}
	return rtn
}

func (s *Set[T]) ContainsFunc(f func(V T) bool) bool {
	return slices.ContainsFunc(s.Values(), f)
}

// Solution for Part 1 of the challenge
func Part1(input map[string][]string) int {
	rtn := 0
	values := NewSet[[3]string]()
	for k, v := range input {
		for _, a := range v {
			if a == k {
				continue
			}
			for _, b := range input[a] {
				if b == k || b == a {
					continue
				}
				if slices.Contains(input[b], k) {
					tmp := []string{k, a, b}
					slices.Sort(tmp) // Ensure we don't add multiple versions
					var store [3]string
					copy(store[:], tmp)
					values.Add(store)
				}
			}
		}
	}
	fmt.Println(values)
	for _, s := range values.Values() {
		sl := s[:]
		if slices.ContainsFunc(sl, func(s string) bool { return strings.HasPrefix(s, "t") }) {
			rtn++
		}
	}
	return rtn
}

// Solution for Part 2 of the challenge
func Part2(input string) int {
	return 1
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (map[string][]string, error) {
	rtn := make(map[string][]string)
	lines := strings.Split(input, "\n")
	for _, l := range lines {
		words := strings.Split(l, "-")
		if _, ok := rtn[words[0]]; ok {
			rtn[words[0]] = append(rtn[words[0]], words[1])
		} else {
			rtn[words[0]] = []string{words[1]}
		}
		if _, ok := rtn[words[1]]; ok {
			rtn[words[1]] = append(rtn[words[1]], words[0])
		} else {
			rtn[words[1]] = []string{words[0]}
		}
	}
	return rtn, nil
}
