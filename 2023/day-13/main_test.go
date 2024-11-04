package main

import "testing"

func TestRotate(t *testing.T) {
	lines := "...##..###..##...##...#.##.#"
	linelen := 7
	t.Run("Single rotate", func(t *testing.T) {
		got, got_n := rotate(lines, linelen)
		want := ".#.#.#..#.###.##.#...#.#.#.."
		want_n := 4
		if got != want {
			t.Errorf("Expected:\n%s\nGot:\n%s\n", want, got)
		}
		if got_n != want_n {
			t.Errorf("Expected %d, got %d\n", want_n, got_n)
		}
	})

	t.Run("Double rotate", func(t *testing.T) {
		got, got_n := rotate(rotate(lines, linelen))
		want := "#.##.#...##...##..###..##..."
		want_n := linelen
		if got != want {
			t.Errorf("Expected:\n%s\nGot:\n%s\n", want, got)
		}
		if got_n != want_n {
			t.Errorf("Expected %d, got %d\n", want_n, got_n)
		}
	})

	t.Run("Triple rotate", func(t *testing.T) {
		got, got_n := rotate(rotate(rotate(lines, linelen)))
		want := "..#.#.#...#.##.###.#..#.#.#."
		want_n := 4
		if got != want {
			t.Errorf("Expected:\n%s\nGot:\n%s\n", want, got)
		}
		if got_n != want_n {
			t.Errorf("Expected %d, got %d\n", want_n, got_n)
		}
	})
}

func TestPart1Inner(t *testing.T) {
	// Tests run on input data
	t.Run("Input 0", func(t *testing.T) {
		line := "...##.#.##.###.##.###...##.##...###.#..##.##.##.#...#####.#.####....#...#.##...#.##.#....#.#.#.#.#...####"
		got := part1_inner(line, 7)
		want := 200
		if got != want {
			t.Errorf("Expected %d, got %d", want, got)
		}
	})

	t.Run("Input 1", func(t *testing.T) {
		line := "###..##.######.##..#..#.#......#.##.##.#..#.##.#..##.#.##.#..##..#.##..#...###..###..#..#...###..###..#.#.##.#..##..#.##.##.#..#.##.#..#..#..#.#......#.####..##.######.##...#.#..######..#..###....#..#..#.#.###....#..#....##.#####.#..#.###...#.....####......#...#.#.##.#.#....###.#......#.#"
		got := part1_inner(line, 17)
		want := 500
		if got != want {
			t.Errorf("Expected %d, got %d", want, got)
		}
	})

	t.Run("Input 2", func(t *testing.T) {
		line := "##.##.######.####.#..##.####.#..###.##.###.#########....#....#....###..#####."
		got := part1_inner(line, 11)
		want := 4
		if got != want {
			t.Errorf("Expected %d, got %d", want, got)
		}
	})
}

func TestPart2Inner(t *testing.T) {
	// Tests run on input data
	t.Run("Input 0", func(t *testing.T) {
		line := "...##.#.##.###.##.###...##.##...###.#..##.##.##.#...#####.#.####....#...#.##...#.##.#....#.#.#.#.#...####"
		got := part2_inner(line, 7)
		want := 1100
		if got != want {
			t.Errorf("Expected %d, got %d", want, got)
		}
	})

	t.Run("Input 1", func(t *testing.T) {
		line := "###..##.######.##..#..#.#......#.##.##.#..#.##.#..##.#.##.#..##..#.##..#...###..###..#..#...###..###..#.#.##.#..##..#.##.##.#..#.##.#..#..#..#.#......#.####..##.######.##...#.#..######..#..###....#..#..#.#.###....#..#....##.#####.#..#.###...#.....####......#...#.#.##.#.#....###.#......#.#"
		got := part2_inner(line, 17)
		want := 11
		if got != want {
			t.Errorf("Expected %d, got %d", want, got)
		}
	})

	t.Run("Input 2", func(t *testing.T) {
		line := "##.##.######.####.#..##.####.#..###.##.###.#########....#....#....###..#####."
		got := part2_inner(line, 11)
		want := 200
		if got != want {
			t.Errorf("Expected %d, got %d", want, got)
		}
	})

	t.Run("Input 5", func(t *testing.T) {
		line := "....###...#..###..####..##..##.###..###.#.###...####...######..#..#..##..###.##..##.#.....#........#.##.."
		got := part2_inner(line, 15)
		want := 5
		if got != want {
			t.Errorf("Expected %d, got %d", want, got)
		}
	})

	t.Run("Input 6", func(t *testing.T) {
		line := ".#.#.#..#.#.#.##.#.###.#.###..##.....#....###.###..#..#.#.####..##.###.##.####.####...##.#.###.###..#...."
		got := part2_inner(line, 7)
		want := 1000
		if got != want {
			t.Errorf("Expected %d, got %d", want, got)
		}
	})

	t.Run("Input 10", func(t *testing.T) {
		line := "..#.....#.###.#.####...##..#......#.#.###..###.#.#..###......#.#.##..##.#.###.####.###.####.##..##.#......#.#.#.#..###.###..###......#."
		got := part2_inner(line, 9)
		want := 1300
		if got != want {
			t.Errorf("Expected %d, got %d", want, got)
		}
	})

	t.Run("Input 11", func(t *testing.T) {
		line := "#####....####..##..##..##.###..#..#..####..##..##..###..#....#..####...#....##..###.##.###.####.####.###..##.####.##.###.##..##.####...#..#...#"
		got := part2_inner(line, 13)
		want := 7
		if got != want {
			t.Errorf("Expected %d, got %d", want, got)
		}
	})

	t.Run("Input 87", func(t *testing.T) {
		line := ".#.....#######..###########.###..##.......##..#.....###.#..###..#...####........###.#######...#...######.##...#.#####"
		got := part2_inner(line, 9)
		want := 12
		if got != want {
			t.Errorf("Expected %d, got %d", want, got)
		}
	})
}
