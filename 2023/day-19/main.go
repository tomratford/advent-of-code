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

	"github.com/tomratford/day-19/ast"
	"github.com/tomratford/day-19/lexer"
	"github.com/tomratford/day-19/parser"
	"github.com/tomratford/day-19/token"
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

/* Any structs required for the challenge go here */

// Solution for Part 1 of the challenge
func Part1(input ast.System) int {
	score := 0

	for _, d := range input.Data {
		key := "in"
		for {
			if key == token.ACCEPT {
				score += d.Sum()
				break
			}
			if key == token.REJECT {
				break
			}

			ops := input.Workflows[key]
			for _, op := range ops {
				if op.Op_type == ast.REDIRECT {
					key = op.Redirect
					break
				} else {
					if op.Op_type == ast.GREATER_THAN {
						if v, ok := d.GetValue(string(op.Part)); ok == nil && (v > op.Value) {
							key = op.Redirect
							break
						}
					} else {
						if v, ok := d.GetValue(string(op.Part)); ok == nil && (v < op.Value) {
							key = op.Redirect
							break
						}
					}
				}
			}
		}
	}

	return score
}

// Solution for Part 2 of the challenge
func Part2(input string) int {
	return 1
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (ast.System, error) {
	l := lexer.New(input)
	p := parser.New(l)

	return p.Parse()
}
