package lexer

import (
	"github.com/tomratford/day-19/token"
)

type Lexer struct {
	input        []rune
	char         rune // current char under examination
	position     int  // current position in input (points to current char)
	readPosition int  // current reading position in input (after current char)
	line         int  // line number for better error reporting, etc
}

func New(input string) *Lexer {
	l := &Lexer{input: []rune(input)}
	l.readChar()
	return l
}

func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		// End of input (haven't read anything yet or EOF)
		// 0 is ASCII code for "NUL" character
		l.char = 0
	} else {
		l.char = l.input[l.readPosition]
	}

	l.position = l.readPosition
	l.readPosition++
}

func (l *Lexer) readIdentifier() string {
	position := l.position

	for isLower(l.char) {
		l.readChar()
	}

	return string(l.input[position:l.position])
}

func (l *Lexer) readInteger() string {
	position := l.position

	for isInteger(l.char) {
		l.readChar()
	}

	return string(l.input[position:l.position])
}

func newToken(tokenType token.Type, line int, char ...rune) token.Token {
	return token.Token{
		Type:    tokenType,
		Literal: string(char),
		Line:    line,
	}
}

func (l *Lexer) skipWhitespace() {
	for l.char == ' ' || l.char == '\t' || l.char == '\n' || l.char == '\r' {
		if l.char == '\n' {
			l.line++
		}
		l.readChar()
	}
}

func isLower(char rune) bool {
	return 'a' <= char && char <= 'z'
}

func isInteger(char rune) bool {
	return '0' <= char && char <= '9'
}

func (l *Lexer) NextToken() token.Token {
	var t token.Token

	l.skipWhitespace()

	switch l.char {
	case '=':
		t = newToken(token.EQ, l.line, l.char)
	case ',':
		t = newToken(token.COMMA, l.line, l.char)
	case '{':
		t = newToken(token.LBRACE, l.line, l.char)
	case '}':
		t = newToken(token.RBRACE, l.line, l.char)
	case '<':
		t = newToken(token.LT, l.line, l.char)
	case '>':
		t = newToken(token.GT, l.line, l.char)
	case ':':
		t = newToken(token.COLON, l.line, l.char)
	case 'A':
		t = newToken(token.ACCEPT, l.line, l.char)
	case 'R':
		t = newToken(token.REJECT, l.line, l.char)
	default:
		if isLower(l.char) {
			t.Literal = l.readIdentifier()
			t.Line = l.line
			t.Type = token.LookupPart(t.Literal)
			return t
		} else if isInteger(l.char) {
			t.Literal = l.readInteger()
			t.Line = l.line
			t.Type = token.INT
			return t
		} else {
			t = newToken(token.ILLEGAL, l.line, l.char)
		}
	}

	l.readChar()
	return t
}
