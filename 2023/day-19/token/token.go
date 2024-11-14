package token

type Type string

const (
	ILLEGAL = "ILLEGAL"
	EOF     = "EOF"

	ID  = "ID"
	INT = "INT"

	ACCEPT = "A"
	REJECT = "R"

	COMMA = ","

	EQ = "="

	LT    = "<"
	GT    = ">"
	COLON = ":"

	LBRACE = "{"
	RBRACE = "}"

	XPART = "XPART"
	MPART = "MPART"
	APART = "APART"
	SPART = "SPART"
)

type Token struct {
	Type    Type
	Literal string
	Line    int
}

func LookupPart(identifier string) Type {
	switch identifier {
	case "x":
		return XPART
	case "m":
		return MPART
	case "a":
		return APART
	case "s":
		return SPART
	default:
		return ID
	}
}
