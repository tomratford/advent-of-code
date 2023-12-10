package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"unicode"
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

	r, err := Parse(string(input))
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Println(Part1(r[:len(r)-1]))
	fmt.Println(Part2(r[len(r)-1]))
}

func Part1(input []Race) int {
	score := 1
	for _, r := range input {
		score = score * len(RaceWinners(r))
	}
	return score
}

// Returns a list of possible times you could win with
func RaceWinners(r Race) []int {
	winners := make([]int, 0, r.Time)
	for i:=1; i<r.Time; i++ {
		if (r.Time - i) * i > r.Record {
			winners = append(winners, i)
		}
	}
	return winners
}

func Part2(input Race) int {
	return len(RaceWinners(input))
}

type Race struct {
	Time int
	Record int
}

func Parse(input string) ([]Race, error) {
	var part2time strings.Builder
	times := []int{}
	input = strings.TrimLeft(input, "Time:")
	input = strings.TrimSpace(input)
	for {
		if unicode.IsDigit(rune(input[0])) {
			lastdigit := strings.IndexFunc(input, func(r rune) bool {
				return !unicode.IsDigit(r)
			})
			num_raw := input[0:lastdigit]
			if num_raw == "" {
				return []Race{}, fmt.Errorf("expected to have a number, actually have: %s", input)
			}
			num, err := strconv.Atoi(num_raw)
			if err != nil {
				return []Race{}, err
			}
			part2time.WriteString(num_raw)
			input = input[lastdigit:]
			times = append(times, num)
			input = strings.TrimSpace(input)
		} else {
			break
		}
	}
	
	var part2distance strings.Builder
	distances := []int{}
	input = strings.TrimLeft(input, "Distance:")
	input = strings.TrimSpace(input)
	for {
		if len(input) == 0 {
			break
		}
		if unicode.IsDigit(rune(input[0])) {
			var num_raw string
			num_raw, input, _ = strings.Cut(input, " ")
			num, err := strconv.Atoi(num_raw)
			if err != nil {
				return []Race{}, err
			}
			part2distance.WriteString(num_raw)
			distances = append(distances, num)
			input = strings.TrimSpace(input)
		} else {
			break
		}
	}

	if len(distances) != len(times) {
		return []Race{}, fmt.Errorf("Expected to parse the name number of distance and time values: got #d=%d and #t=%d", len(distances), len(times))
	}

	races := make([]Race, len(distances))
	for i := range(distances) {
		races[i] = Race{times[i], distances[i]}
	}
	part2time_num,err := strconv.Atoi(part2time.String())
	if err != nil {
		return []Race{}, err
	}
	part2distance_num,err := strconv.Atoi(part2distance.String())
	if err != nil {
		return []Race{}, err
	}
	races = append(races, Race{part2time_num, part2distance_num})
	
	return races, nil
}
