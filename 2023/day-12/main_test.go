package main

import (
	"testing"
)

func TestIsValid(t *testing.T) {
	t.Run("check invalid fail", func(t *testing.T) {
		sizes := []int{1,1,3}
		row := []int{BROKEN,BROKEN,EMPTY,EMPTY,BROKEN,BROKEN,BROKEN}
		if isValid(row, sizes) {
			t.Errorf("expected %v to fail with sizes %v", row, sizes)
		}
	})
	t.Run("check simple valid passes", func(t *testing.T) {
		sizes := []int{1,1,3}
		rows := [][]int{
			[]int{BROKEN,EMPTY,BROKEN,EMPTY,BROKEN,BROKEN,BROKEN},
		}
		for _, row := range rows {
			if !isValid(row, sizes) {
				t.Errorf("expected %v to pass with sizes %v", row, sizes)
			}
		}
	})
	t.Run("check longer valid passes", func(t *testing.T) {
		sizes := []int{1,1,3}
		rows := [][]int{
			[]int{EMPTY,EMPTY,BROKEN,EMPTY,EMPTY,BROKEN,EMPTY,EMPTY,EMPTY,BROKEN,BROKEN,BROKEN, EMPTY},
		}
		for _, row := range rows {
			if !isValid(row, sizes) {
				t.Errorf("expected %v to pass with sizes %v", row, sizes)
			}
		}
	})
	t.Run("check even longer valid passes", func(t *testing.T) {
		sizes := []int{4,1,1}
		rows := [][]int{
			[]int{BROKEN, BROKEN, BROKEN, BROKEN, EMPTY, BROKEN, EMPTY, EMPTY, EMPTY, BROKEN, EMPTY, EMPTY, EMPTY},
		}
		for _, row := range rows {
			if !isValid(row, sizes) {
				t.Errorf("expected %v to pass with sizes %v", row, sizes)
			}
		}
	})
	t.Run("check longer invalid fail", func(t *testing.T) {
		sizes := []int{3,2,1}
		row := []int{EMPTY,BROKEN,BROKEN,BROKEN,EMPTY,EMPTY,EMPTY,EMPTY,EMPTY,EMPTY,BROKEN,BROKEN}
		if isValid(row, sizes) {
			t.Errorf("expected %v to fail with sizes %v", row, sizes)
		}		
	})
}
