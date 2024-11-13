package ast

import "github.com/tomratford/day-19/token"

type If_Else_Expression struct {
	Part     Part
	PartType token.Type
	LT       bool // If false => greater than
	Else     token.Type
	Else_ID  string
}

func (i If_Else_Expression) Execute() {

}
