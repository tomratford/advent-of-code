package main

import "testing"

func TestNextInstruction(t *testing.T) {
	t.Run("ex 1: update B register", func(t *testing.T) {
		c := Computer{
			C:   9,
			ops: []int{2, 6},
		}
		for c.NextInstruction() {
		}
		if c.B != 1 {
			t.Errorf("expected B register to be 1, got %d", c.B)
		}
	})
	t.Run("ex 2: check output", func(t *testing.T) {
		c := Computer{
			A:   10,
			ops: []int{5, 0, 5, 1, 5, 4},
		}
		for c.NextInstruction() {
		}
		got := c.output.String()
		want := "0,1,2,"
		if got != want {
			t.Errorf("expected output to be %q, got %q", want, got)
		}
	})
	t.Run("ex 3: check output & register A", func(t *testing.T) {
		c := Computer{
			A:   2024,
			ops: []int{0, 1, 5, 4, 3, 0},
		}
		for c.NextInstruction() {
		}
		got := c.Output()
		want := "4,2,5,6,7,7,7,7,3,1,0"
		if got != want {
			t.Errorf("expected output to be %q, got %q", got, want)
		}
		if c.A != 0 {
			t.Errorf("expected A register to be 0, got %d", c.A)
		}
	})
	t.Run("ex 4: update B register", func(t *testing.T) {
		c := Computer{
			B:   29,
			ops: []int{1, 7},
		}
		for c.NextInstruction() {
		}
		if c.B != 26 {
			t.Errorf("expected B register to be 26, got %d", c.B)
		}
	})
	t.Run("ex 5: update B register", func(t *testing.T) {
		c := Computer{
			B:   2024,
			C:   43690,
			ops: []int{4, 0},
		}
		for c.NextInstruction() {
		}
		if c.B != 44354 {
			t.Errorf("expected B register to be 44354, got %d", c.B)
		}
	})
}
