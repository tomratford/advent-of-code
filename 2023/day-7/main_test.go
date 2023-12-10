package main

import (
	"maps"
	"testing"
)

func TestAccumulateString(t *testing.T) {
	t.Run("error on bad input", func(t *testing.T) {
		_, err := AccumulateString(" blarg ")
		if err == nil {
			t.Errorf("Expected string error to return an error")
		}
	})
	t.Run("fails on empty string", func(t *testing.T) {
		_, err := AccumulateString("")
		if err == nil {
			t.Errorf("Expected string error to return an error")
		}
	})
	t.Run("counts correctly", func(t *testing.T) {
		got, err := AccumulateString("AA43K")
		if err != nil {
			t.Errorf("Unexpected error: %s", err)
		}
		want := map[rune]int {
			'A': 2,
			'4': 1,
			'3': 1,
			'K': 1,
		}
		if !maps.Equal(got, want) {
			t.Errorf("Expected equal maps, got %v, want %v", got, want)
		}
	})
}

func TestScoreHand(t *testing.T) {
	t.Run("error on empty input", func(t *testing.T) {
		_, err := ScoreHand(map[rune]int{})
		if err == nil {
			t.Errorf("Expected test to fail on empty input")
		}
	})
	t.Run("error on input where values add up to less than 5", func(t *testing.T) {
		_, err := ScoreHand(map[rune]int{
			'Q': 1,
			'3': 1,
			'K': 1,
			'J': 1,
		})
		if err == nil {
			t.Errorf("Expected test to fail")
		}
	})
	t.Run("error on input where values add up to more than 5", func(t *testing.T) {
		_, err := ScoreHand(map[rune]int{
			'Q': 2,
			'3': 1,
			'K': 1,
			'J': 3,
		})
		if err == nil {
			t.Errorf("Expected test to fail")
		}
	})

	t.Run("correct values returned", func(t *testing.T) {
		type test struct {
			input map[rune]int
			want int
		}
		tests := []test{
			{input: map[rune]int{'2': 1, '3': 1, '4': 1, '5': 1, '6': 1}, want: HIGHCARD},
			{input: map[rune]int{'A': 2, '3': 1, '4': 1, '5': 1}, want: ONEPAIR},
			{input: map[rune]int{'2': 2, '3': 1, '4': 2}, want: TWOPAIR},
			{input: map[rune]int{'T': 3, '9': 1, '8': 1}, want: THREEOFAKIND},
			{input: map[rune]int{'2': 2, '3': 3}, want: FULLHOUSE},
			{input: map[rune]int{'A': 4, '8': 1}, want: FOUROFAKIND},
			{input: map[rune]int{'A': 5}, want: FIVEOFAKIND},
		}

		for _, tc := range tests {
			got, err := ScoreHand(tc.input)
			if err != nil {
				t.Errorf("unexpected fail: %s", err)
			}
			if tc.want != got {
				t.Errorf("expected: %v, got: %v", tc.want, got)
			}
		}
		
	})
}

func TestCompareCards(t *testing.T) {
	t.Run("both cards must be valid string", func(t *testing.T) {
		_, err := CompareCards("T98KQ","")
		if err == nil {
			t.Errorf("error expected")
		}
		_, err = CompareCards("", "T98KQ")
		if err == nil {
			t.Errorf("error expected")
		}
		_, err = CompareCards("bad", "KQT98")
		if err == nil {
			t.Errorf("error expected")
		}
	})

	t.Run("check comparisons", func(t *testing.T) {
		type test struct {
			input1 string
			input2 string
			want int
		}
		tests := []test{
			{input1: "TK9Q8", input2: "KQT98", want: -1},
			{input1: "KQT98", input2: "TK9Q8", want: 1},
			{input1: "JJJ98", input2: "23498", want: 1},
		}

		for _, tc := range tests {
			got, err := CompareCards(tc.input1, tc.input2)
			if err != nil {
				t.Errorf("unexpected fail: %s", err)
			}
			if tc.want != got {
				t.Errorf("expected: %v, got: %v", tc.want, got)
			}
		}		
	})

	t.Run("check comparisons with modified joker", func(t *testing.T) {
		type test struct {
			input1 string
			input2 string
			want int
		}

		STRENGTH['J'] = 1
		
		tests := []test{
			{input1: "JJJ98", input2: "23498", want: -1},
			{input1: "23456", input2: "2345J", want: 1},
		}

		for _, tc := range tests {
			got, err := CompareCards(tc.input1, tc.input2)
			if err != nil {
				t.Errorf("unexpected fail: %s", err)
			}
			if tc.want != got {
				t.Errorf("expected: %v, got: %v", tc.want, got)
			}
		}		
	})	
}
