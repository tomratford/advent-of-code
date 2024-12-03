package main

import (
	"fmt"
	"os"
	"testing"

	"golang.org/x/exp/rand"
)

var Runes = []rune("abcdefghijklmnopqrstuvwxyz_?!0123456789")

var Digits = []rune("0123456789")

func RandMulString(n int) string {
	b := make([]rune, n)
	for i := range b {
		b[i] = Runes[rand.Intn(len(Runes))]
	}
	return string(b)
}

func BenchmarkPart1(b *testing.B) {
	input, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println(err)
		return
	}

	Part1(string(input))
}
func BenchmarkPart1_V1(b *testing.B) {
	input, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println(err)
		return
	}

	Part1_V1(string(input))
}
func BenchmarkPart1_V0(b *testing.B) {
	input, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println(err)
		return
	}

	Part1_V0(string(input))
}
