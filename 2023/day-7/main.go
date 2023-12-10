/*
Advent of code; Year 2023, Day 7

# Some notes on the challenge or solution here

usage:

	go run main.go path/to/input.txt
*/
package main

import (
	"fmt"
	"maps"
	"os"
	"slices"
	"strconv"
	"strings"
	"unicode"
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

const (
	HIGHCARD = iota + 1
	ONEPAIR
	TWOPAIR
	THREEOFAKIND
	FULLHOUSE
	FOUROFAKIND
	FIVEOFAKIND
)

var (
	STRENGTH = map[byte]int{
		'2': 2,
		'3': 3,
		'4': 4,
		'5': 5,
		'6': 6,
		'7': 7,
		'8': 8,
		'9': 9,
		'T': 10,
		'J': 11,
		'Q': 12,
		'K': 13,
		'A': 14,
	}
)

type Hand struct {
	cards string
	score int
	bet int
}

/* Any structs required for the challenge go here */

// Solution for Part 1 of the challenge
func Part1(input []Hand) int {
	slices.SortFunc(input, func(a, b Hand) int {
		if a.score > b.score {
			return 1
		} else if b.score > a.score {
			return -1
		} else {
			x, err := CompareCards(a.cards, b.cards)
			if err != nil {
				panic("BAD THINGS HAPPENING")
			}
			return x
		}
	})

	fmt.Println(input)
	
	score := 0
	for i, hand := range input {
		score += (i+1) * hand.bet
	}
	return score
}

// Solution for Part 2 of the challenge
func Part2(input []Hand) int {

	// Manipulate input
	STRENGTH['J'] = 1
	for i, hand := range input {
		if strings.Contains(hand.cards, "J") {
			cards, err := AccumulateString(hand.cards)
			if err != nil {
				panic(err)
			}
			if hand.cards == "JJJJJ" {
				continue // Don't want to fuck about with this
			}

			// Remove J from the accumulate s
			Js := cards['J']
			delete(cards, 'J')

			// Add 1 to the biggest value in the deck. 
			var biggest rune = 0
			var j rune
			for j = range cards {
				if biggest == 0 {
					biggest = j
				}
				if cards[j] > cards[biggest] {
					biggest = j
				}
			}
			cards[j] += Js
			score, err := ScoreHand(cards)
			if err != nil {
				panic(err)
			}
			input[i].score = score
		}
	}
	
	return Part1(input)
}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) ([]Hand, error) {
	lines := strings.Split(input, "\n")
	hands := make([]Hand, 0, len(lines))
	for _, line := range lines[:len(lines)-1] {
		words := strings.Split(line, " ")
		a, err := AccumulateString(words[0])
		if err != nil {
			fmt.Println(lines)
			return []Hand{}, err
		}
		score, err := ScoreHand(a)
		if err != nil {
			return []Hand{}, err
		}
		bet, err := strconv.Atoi(words[1])
		if err != nil {
			return []Hand{}, err
		}		
		hands = append(hands, Hand{words[0], score, bet})
	}
	return hands, nil
}

func AccumulateString(s string) (map[rune]int, error) {
	if strings.TrimSpace(s) == "" {
		return nil, fmt.Errorf("Empty string provided as input")
	}
	if strings.ContainsFunc(s, func(r rune) bool {
		return !(unicode.IsUpper(r) || unicode.IsDigit(r))
	}) {
		return nil, fmt.Errorf("Expected string to only contain digits or uppercase letters")
	}

	acc := make(map[rune]int, len(s))

	for _, c := range s {
		acc[c] += 1
	}

	return acc, nil
}

func ScoreHand(a map[rune]int) (int, error) {
	if maps.Equal(a, map[rune]int{}) {
		return 0, fmt.Errorf("empty input provided")
	}

	counts := make(map[int]int, len(a))
	tot := 0
	for _, i := range a {
		counts[i] += 1
		tot += i
	}
	if tot != 5 {
		return 0, fmt.Errorf("Not five cards counted, a=%v", a)
	}

	switch {
	case maps.Equal(counts, map[int]int{1: 5}):
		return HIGHCARD, nil
	case maps.Equal(counts, map[int]int{1: 3, 2: 1}):
		return ONEPAIR, nil
	case maps.Equal(counts, map[int]int{1: 1, 2: 2}):
		return TWOPAIR, nil
	case maps.Equal(counts, map[int]int{1: 2, 3: 1}):
		return THREEOFAKIND, nil
	case maps.Equal(counts, map[int]int{2: 1, 3: 1}):
		return FULLHOUSE, nil
	case maps.Equal(counts, map[int]int{1: 1, 4: 1}):
		return FOUROFAKIND, nil
	case maps.Equal(counts, map[int]int{5: 1}):
		return FIVEOFAKIND, nil
	default:
		return 0, fmt.Errorf("Unaccounted for case")
	}
}

func CompareCards(s1, s2 string) (int, error) {
	if s1 == "" || s2 == "" {
		return 0, fmt.Errorf("Missing string passed: s1=\"%s\", s2=\"%s\"", s1, s2)
	}
	if len(s1) != len(s2) {
		return 0, fmt.Errorf("Length between strings not the same")
	}
	if strings.ContainsFunc(s1, func(r rune) bool {
		return !(unicode.IsUpper(r) || unicode.IsDigit(r))
	}) {
		return 0, fmt.Errorf("Expected input string to only contain digits or uppercase letters")
	}
	if strings.ContainsFunc(s2, func(r rune) bool {
		return !(unicode.IsUpper(r) || unicode.IsDigit(r))
	}) {
		return 0, fmt.Errorf("Expected input string to only contain digits or uppercase letters")
	}	

	for i := range s1 {
		switch {
		case STRENGTH[s1[i]] > STRENGTH[s2[i]]:
			return 1, nil
		case STRENGTH[s1[i]] < STRENGTH[s2[i]]:
			return -1, nil			
		}
	}
	return 1, nil
}
