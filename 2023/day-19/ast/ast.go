package ast

type System struct {
	Workflows map[string][]Operation

	Data []Part
}
