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
	"reflect"
	"strconv"
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

	inputs, gates, all_gates, err := Parse(string(input))
	if err != nil {
		fmt.Println(err)
		return
	}
	fmt.Println(Part1(inputs, gates))
	fmt.Println(Part2(inputs, gates, all_gates))
}

// Structs and types

type Gate struct {
	Op     string
	Input1 string
	Input2 string
	Output string
}

func (g Gate) String() string {
	return fmt.Sprintf("{%s %s %s -> %s}", g.Input1, g.Op, g.Input2, g.Output)
}

func (g *Gate) Execute(i map[string]int) {
	switch g.Op {
	case "OR":
		_, ok1 := i[g.Input1]
		_, ok2 := i[g.Input2]
		if ok1 && ok2 {
			i[g.Output] = i[g.Input1] | i[g.Input2]
		}
	case "XOR":
		_, ok1 := i[g.Input1]
		_, ok2 := i[g.Input2]
		if ok1 && ok2 {
			i[g.Output] = i[g.Input1] ^ i[g.Input2]
		}
	case "AND":
		_, ok1 := i[g.Input1]
		_, ok2 := i[g.Input2]
		if ok1 && ok2 {
			i[g.Output] = i[g.Input1] & i[g.Input2]
		}
	default:
		panic("How did you get here")
	}
}

// Solution for Part 1 of the challenge
func Part1(input map[string]int, gates map[string][]*Gate) int {
	// Copy map to avoid corruption in p2
	i := make(map[string]int)
	for k, v := range input {
		i[k] = v
	}

	for {
		oldi := make(map[string]int)
		for k, v := range i {
			oldi[k] = v
		}
		for k := range i {
			for _, g := range gates[k] {
				g.Execute(i)
			}
		}
		if reflect.DeepEqual(oldi, i) {
			break
		}
	}

	rtn := 0
	j := 0
	zval := func(j int) string { return fmt.Sprintf("z%.2d", j) }
	for {
		v, ok := i[zval(j)]
		if !ok {
			break
		}
		rtn += v * (1 << j)
		j++
	}
	return rtn
}

// Solution for Part 2 of the challenge
func Part2(input map[string]int, gates map[string][]*Gate, all_gates []*Gate) int {
	fmt.Println("===")
	xval := func(j int) string { return fmt.Sprintf("x%.2d", j) }
	yval := func(j int) string { return fmt.Sprintf("y%.2d", j) }
	for j := 0; j < 44; j++ {
		i := make(map[string]int)
		for k := range input {
			i[k] = 0
		}
		i[xval(0)] = 1
		i[yval(j)] = 1
		want := (1 << 0) + (1 << j)
		// rtn := make(map[int]map[string][2]*Gate)
		if Part1(i, gates) != want {
			fmt.Println("Bad number:", j)
			// Notes for future me
			// I solved this by finding all the possible changes that would fix each broken number (see below)
			// This helped me find changes which were the same across multiple items.
			// There were lots and I was hopeless so I used the subreddit to determine that the trick was to
			// look for broken XORs, I had a rough idea that this would be the case as presumably this determines
			// whether we have to carry to the next bit?
			// I then just trial and error'd various swaps until I got the solution.

			// rtn[j] = make(map[string][2]*Gate)
			// for a := 0; a < len(all_gates); a++ {
			// 	for b := a; b < len(all_gates); b++ {
			// 		all_gates[a].Output, all_gates[b].Output = all_gates[b].Output, all_gates[a].Output
			// 		if Part1(i, gates) == want {
			// 			x := []string{fmt.Sprint(all_gates[a], all_gates[b])}
			// 			slices.Sort(x)
			// 			rtn[j][fmt.Sprint(x)] = [2]*Gate{all_gates[a], all_gates[b]}
			// 			fmt.Printf("Can swap output on %v and %v\n", all_gates[a], all_gates[b])
			// 		}
			// 		all_gates[a].Output, all_gates[b].Output = all_gates[b].Output, all_gates[a].Output
			// 	}
			// }
		}
	}
	return 1
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (map[string]int, map[string][]*Gate, []*Gate, error) {
	sections := strings.Split(input, "\n\n")

	inputs := make(map[string]int)
	input_lines := strings.Split(sections[0], "\n")
	for _, input_line := range input_lines {
		words := strings.Split(input_line, ": ")
		bit, err := strconv.Atoi(words[1])
		if err != nil {
			return map[string]int{}, map[string][]*Gate{}, []*Gate{}, fmt.Errorf("couldn't parse line %q: %v", input_line, err)
		}
		inputs[words[0]] = bit
	}

	all_gates := make([]*Gate, 0, len(sections[1]))
	gates := make(map[string][]*Gate)
	for _, gate_line := range strings.Split(sections[1], "\n") {
		var op, input1, input2, output string
		_, err := fmt.Sscanf(gate_line, "%s %s %s -> %s", &input1, &op, &input2, &output)
		if err != nil {
			return map[string]int{}, map[string][]*Gate{}, []*Gate{}, fmt.Errorf("couldn't parse line %q: %v", gate_line, err)
		}
		g := Gate{
			op,
			input1,
			input2,
			output,
		}
		all_gates = append(all_gates, &g)
		if _, ok := gates[input1]; ok {
			gates[input1] = append(gates[input1], &g)
		} else {
			gates[input1] = []*Gate{&g}
		}
	}
	return inputs, gates, all_gates, nil
}
