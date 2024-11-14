package parser

import (
	"fmt"
	"strconv"

	"github.com/tomratford/day-19/ast"
	"github.com/tomratford/day-19/lexer"
	"github.com/tomratford/day-19/token"
)

type Parser struct {
	lexer *lexer.Lexer

	currentToken token.Token
	peekToken    token.Token
	prevToken    token.Token
}

func New(l *lexer.Lexer) *Parser {
	p := &Parser{
		lexer: l,
	}

	// Read two tokens, so currentToken and peekToken are both set.
	p.nextToken()
	p.nextToken()

	return p
}

func (p *Parser) nextToken() {
	p.prevToken = p.currentToken
	p.currentToken = p.peekToken
	p.peekToken = p.lexer.NextToken()
}

func (p *Parser) Parse() (ast.System, error) {
	ops := make(map[string][]ast.Operation)
	data := make([]ast.Part, 0)

	for {
		switch p.currentToken.Type {
		case token.ID: // Parse workflow
			id := p.currentToken.Literal
			ops[id] = make([]ast.Operation, 0)
			p.nextToken() // go to lbrace
			if p.currentToken.Type != token.LBRACE {
				return ast.System{}, fmt.Errorf("missing '{' after identifier %q on line %d", p.currentToken.Literal, p.currentToken.Line)
			}
			p.nextToken()                             // skip lbrace
			for p.currentToken.Type != token.RBRACE { // keep going until you stop
				switch p.currentToken.Type {
				case token.ACCEPT, token.REJECT:
					ops[id] = append(ops[id], ast.Operation{
						Op_type:  ast.REDIRECT,
						Redirect: string(p.currentToken.Type),
					})
				case token.XPART, token.MPART, token.APART, token.SPART:
					op := ast.Operation{}

					op.Part = p.currentToken.Type

					p.nextToken() // Go to op ('>' or '<')
					switch p.currentToken.Type {
					case token.GT:
						op.Op_type = ast.GREATER_THAN
					case token.LT:
						op.Op_type = ast.LESS_THAN
					default:
						return ast.System{}, fmt.Errorf("expected > or < , got %q on line %d", p.currentToken.Literal, p.currentToken.Line)
					}

					p.nextToken() // go to value
					i, err := strconv.Atoi(p.currentToken.Literal)
					if err != nil {
						return ast.System{}, err
					}
					op.Value = i

					p.nextToken() // go to colon
					if p.currentToken.Type != token.COLON {
						return ast.System{}, fmt.Errorf("missing ':' on line %d", p.currentToken.Line)
					}

					p.nextToken() // go to redirect id
					switch p.currentToken.Type {
					case token.ACCEPT, token.REJECT:
						op.Redirect = string(p.currentToken.Type)
					case token.ID:
						op.Redirect = p.currentToken.Literal
					default:
						return ast.System{}, fmt.Errorf("expected ID, got %q on line %d", p.currentToken.Literal, p.currentToken.Line)
					}

					ops[id] = append(ops[id], op)
					p.nextToken() // move to comma
				case token.ID:
					ops[id] = append(ops[id], ast.Operation{
						Op_type:  ast.REDIRECT,
						Redirect: p.currentToken.Literal,
					})
				default:
					return ast.System{}, fmt.Errorf("expected workflow on line %d, got %q", p.currentToken.Line, p.currentToken.Literal)
				}
				p.nextToken() // skip comma
			}
		case token.LBRACE: // Parse data
			part := ast.Part{}

			p.nextToken() // skip { (or go to x)
			p.nextToken() // skip x (or go to =)
			p.nextToken() // skip = (or go to value
			if p.currentToken.Type != token.INT {
				return ast.System{}, fmt.Errorf("missing value for x on line %d", p.currentToken.Line)
			}
			i, err := strconv.Atoi(p.currentToken.Literal)
			if err != nil {
				return ast.System{}, err
			}
			part.X = i
			p.nextToken() // move to comma

			p.nextToken() // skip comma (or go to m)
			p.nextToken() // skip m (or go to =)
			p.nextToken() // skip = (or go to value
			if p.currentToken.Type != token.INT {
				return ast.System{}, fmt.Errorf("missing value for m on line %d, got %q", p.currentToken.Line, p.currentToken.Literal)
			}
			i, err = strconv.Atoi(p.currentToken.Literal)
			if err != nil {
				return ast.System{}, err
			}
			part.M = i
			p.nextToken()

			p.nextToken() // skip comma (or go to a)
			p.nextToken() // skip a (or go to =)
			p.nextToken() // skip = (or go to value
			if p.currentToken.Type != token.INT {
				return ast.System{}, fmt.Errorf("missing value for a on line %d", p.currentToken.Line)
			}
			i, err = strconv.Atoi(p.currentToken.Literal)
			if err != nil {
				return ast.System{}, err
			}
			part.A = i
			p.nextToken()

			p.nextToken() // skip comma (or go to s)
			p.nextToken() // skip s (or go to =)
			p.nextToken() // skip = (or go to value
			if p.currentToken.Type != token.INT {
				return ast.System{}, fmt.Errorf("missing value for s on line %d", p.currentToken.Line)
			}
			i, err = strconv.Atoi(p.currentToken.Literal)
			if err != nil {
				return ast.System{}, err
			}
			part.S = i
			p.nextToken()

			p.nextToken() // skip }
			data = append(data, part)
		}
		if p.peekToken.Type == token.EOF {
			break
		} else {
			p.nextToken()
		}
	}

	return ast.System{Workflows: ops, Data: data}, nil
}
