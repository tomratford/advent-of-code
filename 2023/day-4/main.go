package main

import (
	"fmt"
	"os"
	"slices"
	"strconv"
	s "strings"
	"unicode"
	"math"
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
	fmt.Println(sumUpMap(Part2(p)))
}

func Part1(cards []Card) float64 {
	var scores float64 = 0
	for _, card := range cards {
		score := 0
		for _, drawn := range card.drawn {
			if slices.Contains(card.winning, drawn) {
				score = score + 1
			}
		}
		if score > 0 {
			scores = scores + math.Pow(2,float64(score-1))
		}
	}
	return scores
}

func Part2(cards []Card) map[int]int {
	var cardCount = make(map[int]int)
	for _, card := range cards {
		cardCount[card.n] = 1
	}
	for _, card := range cards {
		score := 0
		for _, drawn := range card.drawn {
			if slices.Contains(card.winning, drawn) {
				score = score + 1
			}
		}
		for i := 1; i <= score; i++ {
			cardCount[card.n + i] = cardCount[card.n + i] + cardCount[card.n]
		}
	}
	return cardCount
}

func sumUpMap[M ~map[K]int, K comparable](m M) int {
	tot := 0
	for _, val := range m {
		tot = tot + val
	}
	return tot
}

type Card struct {
	n int
	winning []int
	drawn []int
}

func Parse(input string) ([]Card, error) {
	lines := s.Split(input, "\n")
	cards := []Card{}
	for _, line := range lines {
		c := Card{}
		
		line = s.TrimPrefix(line, "Card")
		line = s.TrimSpace(line)
		colon := s.Index(line, ":")
		if colon == -1 {
			continue // Empty line
		}
		card_num, err := strconv.Atoi(line[0:colon])
		if err != nil {
			return []Card{}, err
		}
		line = line[colon:]
		c.n = card_num
		winning, drawn, found := s.Cut(line, "|")
		if !found {
			return []Card{}, fmt.Errorf("Expected to find a '|' in line: %s", line)
		}
		
		c.winning, err = parseSpaceDelimNumbers(winning)
		if err != nil {
			return []Card{}, err
		}
		c.drawn, err = parseSpaceDelimNumbers(drawn)
		if err != nil {
			return []Card{}, err
		}

		cards = append(cards, c)
	}
	return cards, nil
}

func parseSpaceDelimNumbers(input string) ([]int, error) {
	var temp s.Builder
	nums := []int{}
	for _, c := range input {
		if unicode.IsDigit(c) {
			temp.WriteRune(c)
		} else {
			if temp_str := s.TrimSpace(temp.String()); temp_str != "" {
				n, err := strconv.Atoi(temp_str)
				if err != nil {
					return []int{}, err
				}
				nums = append(nums, n)
				temp.Reset()
			}
		}
	}
	if temp_str := s.TrimSpace(temp.String()); temp_str != "" {
		n, err := strconv.Atoi(temp_str)
		if err != nil {
			return []int{}, err
		}
		nums = append(nums, n)
		temp.Reset()
	}
	return nums, nil
}
