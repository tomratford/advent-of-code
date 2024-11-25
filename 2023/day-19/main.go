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
	fmt.Println(Part2(p))
}

// Structs and types

type Range struct {
	lower, upper int
}

func (r Range) String() string { return fmt.Sprintf("%d-%d",r.lower,r.upper)}

func (r Range) len() int { return r.upper - r.lower }

func (r1 Range) within(r2 Range) bool {
	return !disjoint(r1, r2) && r2.lower < r1.lower && r1.upper < r2.upper
}

func disjoint(r1, r2 Range) bool { return r1.upper < r2.lower || r1.lower > r2.upper }

func overlap(r1, r2 Range) []Range {
	if r1.within(r2) {
		return []Range{r2}
	} else if disjoint(r1, r2) {
		return []Range{r1, r2}
	} else {
		new_range := r1
		return []Range{new_range}
	}
}

type PartRange struct {
	X, M, A, S Range
}

func (p PartRange) String() string {
	return fmt.Sprintf("X:%v,M:%v,A:%v,S:%v",p.X,p.M,p.A,p.S)
}

func NewPartRange() PartRange {
	return PartRange{
		Range{1, 4000},
		Range{1, 4000},
		Range{1, 4000},
		Range{1, 4000},
	}
}

func GetOverlap(p1, p2 PartRange) []PartRange {
	return []PartRange{}
}

func (p PartRange) Combinations() int {
	return p.X.len() * p.M.len() * p.A.len() * p.M.len()
}

func AllCombinations(ps []PartRange) int {
	rtn := 0
	for _, p := range ps {
		rtn += p.Combinations()
	}
	return rtn
}

// Solution for Part 1 of the challenge
func Part1(input ast.System) int {
	score := 0

	tree := makeTrees(input.Workflows)
	for _, d := range input.Data {
		key := "in"
		for key != token.ACCEPT && key != token.REJECT {
			key = tree[key](d)
		}
		if key == token.ACCEPT {
			score += d.Sum()
		}
	}

	return score
}

func makeTrees(input map[string][]ast.Operation) map[string]func(ast.Part) string {
	tree := make(map[string]func(ast.Part) string, len(input))
	for k, v := range input {
		tree[k] = makeTree(v)
	}
	return tree
}

func makeTree(ops []ast.Operation) func(ast.Part) string {
	return func(p ast.Part) string {
		for _, op := range ops {
			switch op.Op_type {
			case ast.REDIRECT:
				return op.Redirect
			case ast.GREATER_THAN:
				v, err := p.GetValue(op.Part)
				if err != nil {
					panic(err)
				}
				if v > op.Value {
					return op.Redirect
				}
			case ast.LESS_THAN:
				v, err := p.GetValue(op.Part)
				if err != nil {
					panic(err)
				}
				if v < op.Value {
					return op.Redirect
				}
			}
		}
		return ""
	}
}

// Solution for Part 2 of the challenge
func Part2(input ast.System) int {
	ranges := getPartRanges(input.Workflows, "in", NewPartRange())
	fmt.Println(167409079868000)
	return ranges
}

func getPartRanges(input map[string][]ast.Operation, key string, parts PartRange) int {
	// Base case
	if key == token.ACCEPT {
		fmt.Println(parts)
		return parts.Combinations()
	} else if key == token.REJECT {
		return 0
	}
	// Recursive case
	rtn := 0
	for _, op := range input[key] {
		switch op.Op_type {
		case ast.REDIRECT:
			rtn += getPartRanges(input, op.Redirect, parts)
		case ast.GREATER_THAN:
			new_parts := parts
			switch op.Part {
			case token.XPART:
				if new_parts.X.lower < op.Value+1 {
					new_parts.X.lower = op.Value + 1
					parts.X.upper = op.Value
				} else {
					return getPartRanges(input, op.Redirect, parts)
				}
			case token.MPART:
				if new_parts.M.lower < op.Value+1 {
					new_parts.M.lower = op.Value + 1
					parts.M.upper = op.Value
				} else {
					return getPartRanges(input, op.Redirect, parts)
				}
			case token.APART:
				if new_parts.A.lower < op.Value+1 {
					new_parts.A.lower = op.Value + 1
					parts.A.upper = op.Value
				} else {
					return getPartRanges(input, op.Redirect, parts)
				}
			case token.SPART:
				if new_parts.S.lower < op.Value+1 {
					new_parts.S.lower = op.Value + 1
					parts.S.upper = op.Value
				} else {
					return getPartRanges(input, op.Redirect, parts)
				}
			}
			rtn += getPartRanges(input, op.Redirect, new_parts)
		case ast.LESS_THAN:
			new_parts := parts
			switch op.Part {
			case token.XPART:
				if new_parts.X.upper > op.Value-1 {
					new_parts.X.upper = op.Value - 1
					parts.X.lower = op.Value
				} else {
					return getPartRanges(input, op.Redirect, parts)
				}
			case token.MPART:
				if new_parts.M.upper > op.Value-1 {
					new_parts.M.upper = op.Value - 1
					parts.M.lower = op.Value
				} else {
					return getPartRanges(input, op.Redirect, parts)
				}
			case token.APART:
				if new_parts.A.upper > op.Value-1 {
					new_parts.A.upper = op.Value - 1
					parts.A.lower = op.Value
				} else {
					return getPartRanges(input, op.Redirect, parts)
				}
			case token.SPART:
				if new_parts.S.upper > op.Value-1 {
					new_parts.S.upper = op.Value - 1
					parts.S.lower = op.Value
				} else {
					return getPartRanges(input, op.Redirect, parts)
				}
			}
			rtn += getPartRanges(input, op.Redirect, new_parts)
		}
	}
	return rtn
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (ast.System, error) {
	l := lexer.New(input)
	p := parser.New(l)

	return p.Parse()
}
