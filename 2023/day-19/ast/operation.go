package ast

import (
	"fmt"

	"github.com/tomratford/day-19/token"
)

type OP int

const (
	REDIRECT OP = iota
	LESS_THAN
	GREATER_THAN
)

type Operation struct {
	Op_type  OP
	Redirect string

	Part  token.Type
	Value int
}

func (o Operation) String() string {
	switch o.Op_type {
	case REDIRECT:
		return fmt.Sprintf("⇒%s", o.Redirect)
	case LESS_THAN:
		return fmt.Sprintf("%s<%d:⇒%s", o.Part, o.Value, o.Redirect)
	case GREATER_THAN:
		return fmt.Sprintf("%s>%d:⇒%s", o.Part, o.Value, o.Redirect)
	}
	return "OP_ERROR"
}
