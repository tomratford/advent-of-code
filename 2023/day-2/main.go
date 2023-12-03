package main

import (
	"fmt"
	"os"
	"strconv"
	s "strings"
)

type Pull struct {
	red int
	green int
	blue int
}

type Game struct {
	number int
	pulls []Pull
}

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

	g, err := Parse(string(input))
	if err != nil {
		fmt.Println(err)
		return
	}
	fmt.Println(Part1(g))
	fmt.Println(Part2(g))
}

// Part 1

func Part1(games []Game) int {
	tot := 0
	for _, game := range games {
		keep := true
		for _, pull := range game.pulls {
			keep = keep && pull.red <= 12 && pull.green <= 13 && pull.blue <= 14
		}
		if keep {
			tot += game.number
		}
	}
	return tot
}

func Part2(games []Game) int {
	tot := 0
	for _, game := range games {
		var max_red, max_blue, max_green int
		for _, pull := range game.pulls {
			max_red = max(max_red, pull.red)
			max_blue = max(max_blue, pull.blue)
			max_green = max(max_green, pull.green)
		}
		tot += max_red*max_blue*max_green
	}
	return tot
}

// Parsint text in

func ParseGameText(input string) (Game, string, error) {
	g := Game{}
	if !s.HasPrefix(input, "Game ") {
		return g, input, fmt.Errorf("Expected prefix 'Game ' in string: %s", input)
	}
	colon_loc := s.Index(input, ":")
	if colon_loc == -1 {
		return g, input, fmt.Errorf("Expected colon in input: %s", input)
	}
	i, err := strconv.ParseInt(string(input[5:colon_loc]),0,64)
	if err != nil {
		return g, input, err
	}
	g.number = int(i)
	return g, input[colon_loc+1:], nil
}

func ParsePull(input string) (Pull, error) {
	p := Pull{}
	
	subs := s.Split(s.ReplaceAll(s.TrimSpace(input),",","")," ")
	for i := 0; i < len(subs) / 2; i++ {
		n, err := strconv.ParseInt(subs[2*i], 0, 64)
		if err != nil {
			return Pull{}, fmt.Errorf("%s; text = %s", err, subs)
		}
		switch subs[2*i + 1] {
		case "green":
			p.green = int(n)
		case "red":
			p.red = int(n)
		case "blue":
			p.blue = int(n)
		}
	}

	return p, nil
}

func Parse(input string) ([]Game, error) {
	lines := s.Split(input, "\n")
	if len(lines) > 0 {
		lines = lines[:len(lines)-1]
	}
	games := []Game{}
	for _, line := range lines {
		g, remains, err := ParseGameText(line)
		if err != nil {
			return []Game{}, err
		}
		pulls_split := s.Split(remains, ";")
		for _, pull_text := range pulls_split {
			p, err := ParsePull(pull_text)
			if err != nil {
				return []Game{}, err
			}
			g.pulls = append(g.pulls, p)
		}
		games = append(games, g)
	}
	return games, nil
}
