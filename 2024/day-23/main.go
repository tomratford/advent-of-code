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

type Set struct {
	data map[string]int
}

func (s *Set) Len() int {
	return len(s.data)
}

func (s *Set) String() string {
	rtn := make([]string, 0, len(s.data))
	for v := range s.data {
		rtn = append(rtn, v)
	}
	return fmt.Sprintf("%v", rtn)
}

func NewSet(data ...string) *Set {
	m := make(map[string]int)
	for _, s := range data {
		m[s]++
	}
	return &Set{
		data: m,
	}
}

func (s *Set) Add(data ...string) {
	for _, v := range data {
		s.data[v]++
	}
}

func Intersect(s1, s2 *Set) *Set {
	s := NewSet()
	for k1 := range s1.data {
		for k2 := range s2.data {
			if k1 == k2 {
				s.Add(k1)
			}
		}
	}
	return s
}

func (s *Set) Values() []string {
	rtn := make([]string, 0, len(s.data))
	for k := range s.data {
		rtn = append(rtn, k)
	}
	return rtn
}

func (s *Set) ContainsFunc(f func(s string) bool) bool {
	return slices.ContainsFunc(s.Values(), f)
}

func (s *Set) CountFunc(f func(s string) bool) int {
	rtn := 0
	values := s.Values()
	for _, v := range values {
		if f(v) {
			rtn++
		}
	}
	return rtn
}

// Solution for Part 1 of the challenge
func Part1(input map[string][]string) int {
	seen := make(map[string]int)
	rtn := 0
	for k, v := range input {
		s := NewSet(k)
		s.Add(v...)
	outer:
		for i := 0; i < len(v); i++ {
			for j := 0; j < len(v); j++ {
				if _, ok := seen[v[i]]; ok {
					continue outer
				}
				if _, ok := seen[v[j]]; ok {
					continue outer
				}
				si := NewSet(v[i])
				si.Add(input[v[i]]...)
				sj := NewSet(v[j])
				sj.Add(input[v[j]]...)
				overlap := Intersect(s, si)
				overlap = Intersect(overlap, sj)
				values := overlap.Values()
				slices.Sort(values)
				key := strings.Join(values, "")
				if _, ok := seen[key]; ok {
					continue outer
				}
				if overlap.Len() >= 3 {
					//fmt.Println(overlap, key)
					seen[key]++
					if t_count := overlap.CountFunc(func(s string) bool {
						return strings.Contains(s, "t")
					}); t_count > 0 {
						fmt.Println(overlap, t_count, getScore(t_count, overlap.Len()))
						rtn += getScore(t_count, overlap.Len())
					}
					break outer
				}
			}
		}
	}
	return rtn
}

// Work out the number of combinations when we have potentially more than one t
func getScore(t_count, length int) int {
	if t_count == 0 {
		return 0
	}

	c := length - t_count
	if c > 2 {
		c = 2
	}
	return combination(length-1, 2) + getScore(t_count-1, length-1)
}

// Factorial function to calculate n!
func factorial(n int) int {
	if n == 0 {
		return 1
	}
	result := 1
	for i := 1; i <= n; i++ {
		result *= i
	}
	return result
}

// Combination function to calculate n Choose R
func combination(n, r int) int {
	if r > n {
		return 0 // If r > n, the result is 0 (invalid combination)
	}
	return factorial(n) / (factorial(r) * factorial(n-r))
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
