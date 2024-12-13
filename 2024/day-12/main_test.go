package main

import (
	"fmt"
	"os"
	"testing"
)

func TestPart2(t *testing.T) {
	t.Run("Sample 1", func(t *testing.T) {
		input, err := os.ReadFile("sample1.txt")
		if err != nil {
			fmt.Println(err)
			return
		}

		p, err := Parse(string(input))
		if err != nil {
			fmt.Println(err)
			return
		}

		got := Part2(p)
		want := 80
		if got != want {
			t.Errorf("expected %d, got %d", want, got)
		}
	})

	t.Run("Sample 4", func(t *testing.T) {
		input, err := os.ReadFile("sample4.txt")
		if err != nil {
			fmt.Println(err)
			return
		}

		p, err := Parse(string(input))
		if err != nil {
			fmt.Println(err)
			return
		}

		got := Part2(p)
		want := 236
		if got != want {
			t.Errorf("expected %d, got %d", want, got)
		}
	})

	t.Run("Sample 5", func(t *testing.T) {
		input, err := os.ReadFile("sample5.txt")
		if err != nil {
			fmt.Println(err)
			return
		}

		p, err := Parse(string(input))
		if err != nil {
			fmt.Println(err)
			return
		}

		got := Part2(p)
		want := 368
		if got != want {
			t.Errorf("expected %d, got %d", want, got)
		}
	})

	t.Run("Sample 3", func(t *testing.T) {
		input, err := os.ReadFile("sample3.txt")
		if err != nil {
			fmt.Println(err)
			return
		}

		p, err := Parse(string(input))
		if err != nil {
			fmt.Println(err)
			return
		}

		got := Part2(p)
		want := 1206
		if got != want {
			t.Errorf("expected %d, got %d", want, got)
		}
	})
}
