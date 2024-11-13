package ast

type Expression interface {
	Execute()
}

type Workflow struct {
	Expressions []Expression
}

type Part struct {
	X int
	M int
	A int
	S int
}
