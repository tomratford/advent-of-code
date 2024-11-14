package lexer

import (
	"testing"

	"github.com/tomratford/day-19/token"
)

func TestNextToken(t *testing.T) {
	input := `
px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}

{x=787,m=2655,a=1222,s=2876}
`

	tests := []struct {
		expectedType    token.Type
		expectedLiteral string
		expectedLine    int
	}{
		{token.ID, "px", 1},
		{token.LBRACE, "{", 1},
		{token.APART, "a", 1},
		{token.LT, "<", 1},
		{token.INT, "2006", 1},
		{token.COLON, ":", 1},
		{token.ID, "qkq", 1},
		{token.COMMA, ",", 1},
		{token.MPART, "m", 1},
		{token.GT, ">", 1},
		{token.INT, "2090", 1},
		{token.COLON, ":", 1},
		{token.ACCEPT, "A", 1},
		{token.COMMA, ",", 1},
		{token.ID, "rfg", 1},
		{token.RBRACE, "}", 1},
		{token.ID, "pv", 2},
		{token.LBRACE, "{", 2},
		{token.APART, "a", 2},
		{token.GT, ">", 2},
		{token.INT, "1716", 2},
		{token.COLON, ":", 2},
		{token.REJECT, "R", 2},
		{token.COMMA, ",", 2},
		{token.ACCEPT, "A", 2},
		{token.RBRACE, "}", 2},
		{token.LBRACE, "{", 4},
		{token.XPART, "x", 4},
		{token.EQ, "=", 4},
		{token.INT, "787", 4},
		{token.COMMA, ",", 4},
		{token.MPART, "m", 4},
		{token.EQ, "=", 4},
		{token.INT, "2655", 4},
		{token.COMMA, ",", 4},
		{token.APART, "a", 4},
		{token.EQ, "=", 4},
		{token.INT, "1222", 4},
		{token.COMMA, ",", 4},
		{token.SPART, "s", 4},
		{token.EQ, "=", 4},
		{token.INT, "2876", 4},
		{token.RBRACE, "}", 4},
		{token.EOF, "\x00", 5},
	}

	l := New(input)

	for i, tt := range tests {
		token := l.NextToken()
		if token.Type != tt.expectedType {
			t.Errorf("test[%d] - token type wrong. Expected: %q, Got: %q", i, tt.expectedType, token.Type)
		}
		if token.Literal != tt.expectedLiteral {
			t.Errorf("test[%d] - literal wrong. Expected: %q, Got: %q", i, tt.expectedLiteral, token.Literal)
		}
		if token.Line != tt.expectedLine {
			t.Errorf("test[%d] - line wrong. Expected: %q, Got: %q", i, tt.expectedLine, token.Line)
		}
	}
}
