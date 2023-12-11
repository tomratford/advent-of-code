/*
Advent of code; Year 2023, Day 08

Took me a while to realise that you're supposed to do LCM for part 2. 
   
usage:
	go run main.go path/to/input.txt
*/
package main

import (
	"fmt"
	"math"
	"math/big"
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

	instructions, t, err := Parse(string(input))
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Println(Part1(instructions, t))
	fmt.Println(Part2(instructions, t))
}

// Structs and types

type Node struct {
	left, right string
}

type Tree map[string]Node

func (n *Node) traverse(s rune) string {
	if s == 'L' {
		return n.left
	} else if s == 'R' {
		return n.right
	} else {
		fmt.Printf("s = %c\n", s)
		panic("Somehow you didn't give a valid string??")
	}
}

/* Any structs required for the challenge go here */

// Solution for Part 1 of the challenge
func Part1(instructions string, t Tree) int {
	steps := 0
	currnode := t["AAA"]
	var s string
	for {
		for _, dir := range instructions {
			s = currnode.traverse(dir)
			currnode = t[s]
			steps += 1
			if s == "ZZZ" {
				break
			}
		}
		if s == "ZZZ" {
			break
		}
	}
	
	return steps
}

// Solution for Part 2 of the challenge
func Part2(instructions string, t Tree) string {
	steps := 1

	starting_nodes := make([]string, 0, len(t)/2) // Niavely assume we won't run out of space
	for k := range t {
		if k[2] == 'A' {
			starting_nodes = append(starting_nodes, k)
		}
	}

	current_nodes := make([]Node, 0, len(starting_nodes))
	for _, k := range starting_nodes {
		current_nodes = append(current_nodes, t[k])
	}

	firstZ := make([]int, len(current_nodes))
	
	for {
		for _, dir := range instructions {
			for i, currnode := range current_nodes {
				s := currnode.traverse(dir)
				current_nodes[i] = t[s] // Move the current node along
				if s[2] == 'Z' {
					if firstZ[i] == 0 {
						firstZ[i] = steps
					}
				}
			}
			steps += 1
			if steps > 100000 { // Assume we find our first Z before 100,000)
				break
			}
		}
		if steps > 100000 { // Assume we find our first Z before 100,000)
			break
		}		
	}

	x := LCM(firstZ[0], firstZ[1:]...)

	return x.String()
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (string, Tree, error) {
	instructions, tree_raw, found := strings.Cut(input, "\n\n")
	if !found {
		return "", Tree{}, fmt.Errorf("No double return found in input string: %s", input)
	}

	t, err := ParseTree(tree_raw)
	if err != nil {
		return "", Tree{}, err
	}
	
	return instructions, t, nil
}

func ParseTree(input string) (Tree, error) {
	lines := strings.Split(input, "\n")

	t := make(Tree, len(lines))
	
	for _, line := range lines {
		if line == "" {
			break
		}
		key := line[0:3]
		left := line[7:10]
		right := line[12:15]
		
		t[key] = Node{left, right}
	}
	
	return t, nil
}

func LCM(n1 int, n2... int) big.Int {
	start := lcm(int64(n1), int64(n2[0]))
	if len(n2) == 1 {
		return start
	} else {
		for _, n := range n2[1:] {
			start = lcm(start.Int64(), int64(n))
		}

		return start
	}
}

func lcm(n1, n2 int64) big.Int {
	var z, a, b big.Int
	z.GCD(&a,&b,big.NewInt(n1),big.NewInt(n2))
	var z2 big.Int
	z2.Mul(big.NewInt(int64(math.Abs(float64(n1)))), big.NewInt(int64(math.Abs(float64(n2)))))
	var z3 big.Int
	z3.Div(&z2, &z)
	return z3
}
