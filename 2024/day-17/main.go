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

	p, err := Parse(string(input))
	if err != nil {
		fmt.Println(err)
		return
	}

	//fmt.Println(Part1(p))
	fmt.Println(Part2(p))
}

// Structs and types

type Computer struct {
	A, B, C int

	ptr int
	ops []int

	output strings.Builder
}

func (c *Computer) NextInstruction() bool {
	if c.ptr >= len(c.ops) {
		return false
	}

	opcode := c.ops[c.ptr]
	operand := c.ops[c.ptr+1]

	instructions := []func(int){
		c.adv, c.bxl, c.bst, c.jnz, c.bxc, c.out, c.bdv, c.cdv,
	}

	instructions[opcode](operand)
	c.ptr += 2
	return true
}

func (c *Computer) combo(operand int) int {
	switch operand {
	case 0, 1, 2, 3:
		return operand
	case 4:
		return c.A
	case 5:
		return c.B
	case 6:
		return c.C
	case 7:
		panic("invalid progam!!")
	}
	return operand
}

func (c *Computer) adv(operand int) {
	cmbo_op := c.combo(operand)
	c.A = c.A / (1 << cmbo_op)
}

func (c *Computer) bxl(operand int) {
	c.B = c.B ^ operand
}

func (c *Computer) bst(operand int) {
	cmbo_op := c.combo(operand)
	c.B = cmbo_op % 8
}

func (c *Computer) jnz(operand int) {
	if c.A != 0 {
		c.ptr = operand - 2
	}
}

func (c *Computer) bxc(operand int) {
	c.B = c.B ^ c.C
}

func (c *Computer) out(operand int) {
	cmbo_op := c.combo(operand)
	c.output.WriteString(fmt.Sprintf("%d,", cmbo_op%8))
}

func (c *Computer) bdv(operand int) {
	cmbo_op := c.combo(operand)
	c.B = c.A / (1 << cmbo_op)
}

func (c *Computer) cdv(operand int) {
	cmbo_op := c.combo(operand)
	c.C = c.A / (1 << cmbo_op)
}

func (c *Computer) Output() string {
	out := c.output.String()
	return out[:len(out)-1]
}

// Solution for Part 1 of the challenge
func Part1(c Computer) string {
	//fmt.Printf("A: %v\nB: %v\nC: %v\n\n", strconv.FormatInt(int64(c.A), 2), strconv.FormatInt(int64(c.B), 2), strconv.FormatInt(int64(c.C), 2))

	for c.NextInstruction() {
		//fmt.Printf("A: %v\nB: %v\nC: %v\n\n", strconv.FormatInt(int64(c.A), 2), strconv.FormatInt(int64(c.B), 2), strconv.FormatInt(int64(c.C), 2))
	}
	return c.Output()
}

// Solution for Part 2 of the challenge
func Part2(c Computer) int {
	i := 0
	j := 2
	for {
		for {
			if slices.Equal(CalcFromNumber(i), c.ops[len(c.ops)-j:]) {
				if j == len(c.ops) {
					return i
				}
				fmt.Print(i, "->")
				i = i << 6
				fmt.Println(i)
				j += 2
				break
			}
			i++
		}
	}
}

// Reverse engineered from output
func CalcFromNumber(A int) []int {
	rtn := make([]int, 0)
	for {
		B := A % 8             // Last 3 digits of A
		B = B ^ 5              // B ^ 101
		C := A >> B            // C = A sans B digits
		B = B ^ C              // B = B ^ C
		A = A >> 3             // chop 3 off
		B = B ^ 6              // B ^ 110
		rtn = append(rtn, B%8) // Output
		if A == 0 {            // Jump (or exit)
			break
		}
	}
	return rtn
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (Computer, error) {
	sections := strings.Split(input, "\n\n")
	var A, B, C int
	fmt.Sscanf(sections[0], "Register A: %d\nRegister B: %d\nRegister C: %d", &A, &B, &C)
	opcodes := make([]int, 0, (len(sections[1])-9)/2)
	for _, r := range sections[1][9:] {
		if r == ',' {
			continue
		}
		n, err := strconv.Atoi(string(r))
		if err != nil {
			return Computer{}, fmt.Errorf("failed to passed opcodes: %v", err)
		}
		opcodes = append(opcodes, n)
	}
	Comp := Computer{}
	Comp.A = A
	Comp.B = B
	Comp.C = C
	Comp.ops = opcodes
	return Comp, nil
}
