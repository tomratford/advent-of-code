/*
Advent of code; Year YYYY, Day XX

# Some notes on the challenge or solution here

usage:

	go run main.go path/to/input.txt
*/
package main

import (
	"container/list"
	"fmt"
	"os"
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

// Solution for Part 1 of the challenge
func Part1(digits []int) int {
	d := make([]int, len(digits))
	copy(d, digits)

	rtn := 0

	// For moving between the arrays
	i := 0
	j := len(digits) - 1

	// Position in the memory
	k := 0

	for {
		if i%2 == 0 { // We are adding digits
			rtn += Score(i/2, k, d[i])
			k += d[i]
			i++
		} else { // We are using spaces
			if no_left := d[j] - d[i]; no_left == 0 {
				if i > j {
					break
				}
				//fmt.Printf("(i:%d,j:%d) ", i, j)
				rtn += Score(j/2, k, d[i])
				k += d[i]
				i++
				j-- // Skip spaces (POTENTIALLY where i is right now)
				if i >= j {
					break
				}
				j--
			} else if no_left > 0 {
				rtn += Score(j/2, k, d[i])
				k += d[i]
				d[j] = d[j] - d[i]
				i++
			} else if no_left < 0 {
				rtn += Score(j/2, k, d[j])
				k += d[j]
				d[i] = d[i] - d[j]
				j--         // skip spaces (POTENTIALLY WHERE i IS RIGHT NOW)
				if i >= j { // exit case
					break
				}
				j--
			}
		}
	}
	return rtn
}

func Score(digit, start, len int) int {
	rtn := 0
	for i := start; i < start+len; i++ {
		rtn += digit * i
	}
	fmt.Printf("%d from %d for %d = %d\n", digit, start, len, rtn)
	return rtn
}

// Solution for Part 2 of the challenge
func Part2(digits []int) int {
	// Create doubly linked list
	blocks := list.New()
	for i, d := range digits {
		if i%2 == 0 {
			blocks.PushBack(Block{
				Id:   i / 2,
				Size: d,
			})
		} else {
			blocks.PushBack(Block{
				Id:   -1,
				Size: d,
			})
		}
	}

	// for i := blocks.Front(); i != nil; i = i.Next() {
	// 	ib := i.Value.(Block)
	// 	fmt.Print(ib)
	// }
	// fmt.Println()

	// Go backwards through the list and try to reallocate
outer:
	for j := blocks.Back(); j != nil; j = j.Prev() {
		for i := blocks.Front(); i != nil; i = i.Next() {
			if i.Value == j.Value {
				break
			}
			ib := i.Value.(Block)
			jb := j.Value.(Block)
			if ib.Id == -1 { // Skip non-free blocks
				if ib.Size > jb.Size {
					// Split

					// Add new block in list
					blocks.InsertAfter(Block{
						Id:   -1,
						Size: ib.Size - jb.Size,
					}, i)
					// Update digit & size on 'i'
					ib.Id = jb.Id
					ib.Size = jb.Size
					i.Value = ib
					// Make old 'j' object free
					jb.Id = -1
					j.Value = jb
					continue outer
				} else if ib.Size == jb.Size {
					// 'Replace': Swap the free block and id'd block
					tmp := i.Value
					i.Value = j.Value
					j.Value = tmp
					continue outer
				}
			}
		}
	}

	// Go forwards through the list and score
	rtn := 0
	start := 0
	for i := blocks.Front(); i != nil; i = i.Next() {
		ib := i.Value.(Block)
		// fmt.Print(ib)
		rtn += ib.Score(start)
		start += ib.Size
	}
	// fmt.Println()

	return rtn
}

// Structs and types for Part 2
type Block struct {
	Id   int // -1 => free block
	Size int
}

func (b Block) String() string {
	if b.Id == -1 {
		return strings.Repeat(".", b.Size)
	} else {
		return strings.Repeat(fmt.Sprint(b.Id), b.Size)
	}
}

func (b Block) Score(start int) int {
	if b.Id == -1 {
		return 0
	}
	rtn := 0
	for i := start; i < start+b.Size; i++ {
		rtn += b.Id * i
	}
	return rtn
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([]int, error) {
	digits := make([]int, 0, len(input))
	for _, r := range input {
		n, err := strconv.Atoi(string(r))
		if err != nil {
			return []int{}, fmt.Errorf("couldn't parse rune %q: %v", r, err)
		}
		digits = append(digits, n)
	}
	return digits, nil
}
