package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Println("No file passed")
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

func Part1(input Problem) int {
	min := 100000000000000 // big number

	for _, seed := range input.Seeds {
		//fmt.Println("========")
		//fmt.Printf("Init = %d\n", seed)
		for _, f := range input.Funcs {
			seed = f(seed)
			//fmt.Printf("%d: %d\n", i+1, seed)
		}
		if seed < min {
			min = seed
		}
	}
	return min
}

func Part2(input Problem) int {
	min := 100000000000000 // big number

	for i:=0; i<=len(input.Seeds)/2; i += 2 {
		for j:=input.Seeds[i]; j < input.Seeds[i] + input.Seeds[i+1]; j++ {
			seed := j
			for _, f := range input.Funcs {
				seed = f(seed)
			}
			if seed < min {
				min = seed
			}
		}
	}
	return min
}

type Mapping = func(int) int

type Problem struct {
	Seeds []int
	Funcs []Mapping
}

func Parse(input string) (Problem, error) {
	split_mappings := strings.Split(input, "\n\n")

	p := Problem{}

	var err error

	p.Seeds, err = ParseSpaceNum(split_mappings[0][7:])
	if err != nil {
		return Problem{}, err
	}

	for _, map_text := range split_mappings[1:] {
		map_split := strings.Split(map_text,"\n")
		nums := [][]int{}
		for _, raw := range map_split[1:] {
			if raw == "" {
				continue
			}
			ns, err := ParseSpaceNum(raw)
			if err != nil {
				return Problem{}, err
			}
			nums = append(nums, ns)
		}
		f, err := MakeMapFunction(nums)
		if err != nil {
			return Problem{}, err
		}
		p.Funcs = append(p.Funcs, f)
	}

	return p, nil
}

func MakeMapFunction(nums [][]int) (Mapping, error) {
	// Used for errors
	identity := func(i int) int {
		return i
	}
	for _, raws := range nums {
		if len(raws) != 3 {
			return identity, fmt.Errorf("input did not split into a vector of 3 values: %v", raws)
		}
	}

	return func(i int) int {
		for _, raws := range nums {
			dest := raws[0]
			source := raws[1]
			dist := raws[2]

			if source <= i && i < source + dist {
				return dest + (i - source)
			}
		}
		return i
	}, nil
}

func ParseSpaceNum(input string) ([]int, error) {
	raws := strings.Split(input, " ")
	seeds := []int{}
	for _, n := range raws {
		num, err := strconv.Atoi(n)
		if err != nil {
			return []int{}, err
		}
		seeds = append(seeds, num)
	}
	return seeds, nil
}
