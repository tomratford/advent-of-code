package parser

import (
	"reflect"
	"testing"

	"github.com/tomratford/day-19/ast"
	"github.com/tomratford/day-19/lexer"
)

func TestParse(t *testing.T) {
	t.Run("parse data correctly", func(t *testing.T) {
		input := `
{x=2637,m=164,a=3096,s=383}
{x=618,m=349,a=205,s=76}
{x=191,m=9,a=375,s=86}
`
		p := New(lexer.New(input))
		got, err := p.Parse()
		if err != nil {
			t.Errorf("expected no error, got %s", err)
		}

		want := []ast.Part{
			{X: 2637, M: 164, A: 3096, S: 383},
			{X: 618, M: 349, A: 205, S: 76},
			{X: 191, M: 9, A: 375, S: 86},
		}

		if !reflect.DeepEqual(got.Data, want) {
			t.Errorf("output not equal. got %v, want %v", got.Data, want)
		}
	})
}
