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
	"strconv"
	"strings"

	"github.com/aclements/go-z3/z3"
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

type Button = func([]bool) []bool

// Structs and types
type Machine struct {
	Indicator  []bool
	Length     int
	Buttons    []Button
	RawButtons [][]int
	Joltage    []int
}

func Encode(current []bool) string {
	var s strings.Builder
	for _, b := range current {
		if b {
			s.WriteRune('#')
		} else {
			s.WriteRune('.')
		}
	}
	return s.String()
}

func Decode(current string) []bool {
	b := make([]bool, 0, len(current))
	for _, c := range current {
		if c == '#' {
			b = append(b, true)
		} else {
			b = append(b, false)
		}
	}
	return b
}

// Solution for Part 1 of the challenge
func Part1(input []Machine) int {
	// Expand the buttons until we reach a solution? Is this brute force? maybe?
	part1 := 0
	for _, m := range input {
		want := Encode(m.Indicator)
		states := make(map[string]int)
		initial := strings.Repeat(".", m.Length)
		states[initial] = 0
		for {
			newStates := maps.Clone(states)
			for s := range states {
				for _, b := range m.Buttons {
					got := Encode(b(Decode(s)))
					if _, ok := states[got]; !ok {
						newStates[got] = states[s] + 1
					}
				}
			}
			//fmt.Println(newStates)
			tmp_keys := slices.Collect(maps.Keys(newStates))
			if slices.Contains(tmp_keys, want) {
				//fmt.Println(want, newStates[want])
				part1 += newStates[want]
				break
			}
			states = newStates
		}
	}

	return part1
}

// Solution for Part 2 of the challenge
// Using https://github.com/willmadison/advent/blob/40df65ce677dc89c8c38afa88c90cd128d8a27d0/advent2025/factory.go#L20
func Part2(input []Machine) int {
	part2 := 0
	for _, m := range input {
		ctx := z3.NewContext(nil)
		solver := z3.NewSolver(ctx)

		intSort := ctx.IntSort()
		zero := ctx.FromInt(0, intSort).(z3.Int)
		one := ctx.FromInt(1, intSort).(z3.Int)

		buttons := make([]z3.Int, len(m.RawButtons))
		for i := range m.RawButtons {
			buttons[i] = ctx.IntConst("button_" + strconv.Itoa(i))
			solver.Assert(buttons[i].GE(zero))
		}

		for i, v := range m.Joltage {
			inc_buttons := make([]z3.Int, 0)
			for j, button := range m.RawButtons {
				for _, b := range button {
					if b == i {
						inc_buttons = append(inc_buttons, buttons[j])
					}
				}
			}

			rhs := ctx.FromInt(int64(v), intSort).(z3.Int)

			if len(inc_buttons) == 0 {
				if v > 0 {
					solver.Assert(zero.Eq(one))
				}
			} else {
				sum := inc_buttons[0]
				for _, t := range inc_buttons[1:] {
					sum = sum.Add(t)
				}
				solver.Assert(sum.Eq(rhs))
			}
		}

		tot := ctx.IntConst("total")
		if len(buttons) > 0 {
			sum_all := buttons[0]
			for _, x := range buttons[1:] {
				sum_all = sum_all.Add(x)
			}
			solver.Assert(tot.Eq(sum_all))
		} else {
			solver.Assert(tot.Eq(zero))
		}

		min_result := -1
		for {
			sat, err := solver.Check()
			if !sat || err != nil {
				break
				//panic(err)
			}

			model := solver.Model()
			res := model.Eval(tot, true)
			val, _, _ := res.(z3.Int).AsInt64()

			min_result = int(val)

			cur := ctx.FromInt(val, intSort).(z3.Int)
			solver.Assert(tot.LT(cur))
		}

		part2 += min_result
	}
	return part2
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([]Machine, error) {
	lines := strings.Split(input, "\n")
	machines := make([]Machine, 0, len(lines))
	for _, l := range lines {
		if l == "" {
			continue
		}
		m := Machine{}
		m.RawButtons = make([][]int, 0)
		m.Buttons = make([]Button, 0)
		words := strings.Split(l, " ")
		for i, w := range words {
			if i == 0 {
				// Indicator
				m.Length = len(w) - 2
				m.Indicator = make([]bool, 0, m.Length)
				for _, c := range w {
					if c == '.' {
						m.Indicator = append(m.Indicator, false)
					} else if c == '#' {
						m.Indicator = append(m.Indicator, true)
					}
				}
			} else if i == len(words)-1 {
				// Joltage
				w2 := w[1 : len(w)-1] // remove { }
				digits := strings.Split(w2, ",")
				m.Joltage = make([]int, 0, len(digits))
				for _, d := range digits {
					n, err := strconv.Atoi(d)
					if err != nil {
						return []Machine{}, err
					}
					m.Joltage = append(m.Joltage, n)
				}
			} else {
				// Button
				w2 := w[1 : len(w)-1] // remove ( )
				digits := strings.Split(w2, ",")
				button := make([]int, 0, len(digits))
				for _, d := range digits {
					n, err := strconv.Atoi(d)
					if err != nil {
						return []Machine{}, err
					}
					button = append(button, n)
				}
				m.RawButtons = append(m.RawButtons, button)
				m.Buttons = append(m.Buttons, func(current []bool) []bool {
					rtn := slices.Clone(current)
					for _, b := range button {
						if b >= len(current) {
							panic(fmt.Errorf("out of range, buttons: %v, current: %q", button, current))
						}
						rtn[b] = !current[b]
					}
					return rtn
				})
			}
		}
		machines = append(machines, m)
	}

	return machines, nil
}
