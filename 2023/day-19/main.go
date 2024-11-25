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
	"strings"

	"github.com/tomratford/day-19/ast"
	"github.com/tomratford/day-19/lexer"
	"github.com/tomratford/day-19/parser"
	"github.com/tomratford/day-19/token"

	"golang.org/x/text/language"
	"golang.org/x/text/message"
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

func (r Range) String() string { return fmt.Sprintf("%d-%d", r.lower, r.upper) }

func (r Range) len() int { return r.upper - r.lower + 1 }

type PartRange struct {
	X, M, A, S Range
}

func (p PartRange) String() string {
	return fmt.Sprintf("X:%v,M:%v,A:%v,S:%v", p.X, p.M, p.A, p.S)
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
	ranges := getPartRanges(input.Workflows, "in", NewPartRange(), 0)
	p := message.NewPrinter(language.English)
	p.Println(167409079868000)
	return ranges
}

func getPartRanges(input map[string][]ast.Operation, key string, parts PartRange, depth int) int {
	// Base case
	if key == token.ACCEPT {
		p := message.NewPrinter(language.English)
		p.Printf("%s%v !!%d\n", strings.Repeat(" ", depth), parts, parts.Combinations())
		return parts.Combinations()
	} else if key == token.REJECT {
		return 0
	}
	// Recursive case
	const (
		FALLTHROUGH = iota
		ADD
		IGNORE
	)
	GTComp := func(new, old *Range, value int) int {
		if old.lower > value {
			return FALLTHROUGH
		} else {
			if old.upper <= value {
				return IGNORE
			} else {
				new.lower = value + 1
				old.upper = value
				return ADD
			}
		}
	}
	LTComp := func(new, old *Range, value int) int {
		if old.upper < value {
			return FALLTHROUGH
		} else {
			if old.lower >= value {
				return IGNORE
			} else {
				new.upper = value - 1
				old.lower = value
				return ADD
			}
		}
	}
	rtn := 0
	fmt.Printf("%s==%s==\n", strings.Repeat(" ", depth), key)
	for _, op := range input[key] {
		fmt.Printf("%s%v\n", strings.Repeat(" ", depth), parts)
		switch op.Op_type {
		case ast.REDIRECT:
			rtn += getPartRanges(input, op.Redirect, parts, depth+1)
		case ast.GREATER_THAN:
			new_parts := parts
			switch op.Part {
			case token.XPART:
				switch GTComp(&new_parts.X, &parts.X, op.Value) {
				case FALLTHROUGH:
					return getPartRanges(input, op.Redirect, parts, depth+1)
				case IGNORE:
					continue
				case ADD:
					rtn += getPartRanges(input, op.Redirect, new_parts, depth+1)
				}
			case token.MPART:
				switch GTComp(&new_parts.M, &parts.M, op.Value) {
				case FALLTHROUGH:
					return getPartRanges(input, op.Redirect, parts, depth+1)
				case IGNORE:
					continue
				case ADD:
					rtn += getPartRanges(input, op.Redirect, new_parts, depth+1)
				}
			case token.APART:
				switch GTComp(&new_parts.A, &parts.A, op.Value) {
				case FALLTHROUGH:
					return getPartRanges(input, op.Redirect, parts, depth+1)
				case IGNORE:
					continue
				case ADD:
					rtn += getPartRanges(input, op.Redirect, new_parts, depth+1)
				}
			case token.SPART:
				switch GTComp(&new_parts.S, &parts.S, op.Value) {
				case FALLTHROUGH:
					return getPartRanges(input, op.Redirect, parts, depth+1)
				case IGNORE:
					continue
				case ADD:
					rtn += getPartRanges(input, op.Redirect, new_parts, depth+1)
				}
			}
		case ast.LESS_THAN:
			new_parts := parts
			switch op.Part {
			case token.XPART:
				switch LTComp(&new_parts.X, &parts.X, op.Value) {
				case FALLTHROUGH:
					return getPartRanges(input, op.Redirect, parts, depth+1)
				case IGNORE:
					continue
				case ADD:
					rtn += getPartRanges(input, op.Redirect, new_parts, depth+1)
				}
			case token.MPART:
				switch LTComp(&new_parts.M, &parts.M, op.Value) {
				case FALLTHROUGH:
					return getPartRanges(input, op.Redirect, parts, depth+1)
				case IGNORE:
					continue
				case ADD:
					rtn += getPartRanges(input, op.Redirect, new_parts, depth+1)
				}
			case token.APART:
				switch LTComp(&new_parts.A, &parts.A, op.Value) {
				case FALLTHROUGH:
					return getPartRanges(input, op.Redirect, parts, depth+1)
				case IGNORE:
					continue
				case ADD:
					rtn += getPartRanges(input, op.Redirect, new_parts, depth+1)
				}
			case token.SPART:
				switch LTComp(&new_parts.S, &parts.S, op.Value) {
				case FALLTHROUGH:
					return getPartRanges(input, op.Redirect, parts, depth+1)
				case IGNORE:
					continue
				case ADD:
					rtn += getPartRanges(input, op.Redirect, new_parts, depth+1)
				}
			}
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
