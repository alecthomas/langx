package parser

import (
	"fmt"
	"math/big"

	"github.com/alecthomas/participle"
	"github.com/alecthomas/participle/lexer"
)

// Expr node in the AST.
type Expr struct {
	Pos lexer.Position

	// Either Unary or Op+left+Right will be present.
	Unary *Unary

	Left  *Expr
	Op    Op
	Right *Expr
}

func (e *Expr) accept(visitor VisitorFunc) error {
	return visitor(e, func(err error) error {
		if err != nil {
			return err
		}
		if err = VisitFunc(e.Unary, visitor); err != nil {
			return err
		}
		if err = VisitFunc(e.Left, visitor); err != nil {
			return err
		}
		if err = VisitFunc(e.Right, visitor); err != nil {
			return err
		}
		return nil
	})
}

func (e *Expr) String() string {
	if e.Unary != nil {
		return e.Unary.String()
	}
	return fmt.Sprintf("%s %s %s", e.Left.String(), e.Op.String(), e.Right.String())
}

// Parse expressions with a custom precedence climbing implementation.
func (e *Expr) Parse(lex *lexer.PeekingLexer) error {
	ex, err := parseExpr(lex, 0)
	if err != nil {
		return err
	}
	*e = *ex
	return nil
}

// Precedence climbing implementation based on
// https://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing
func parseExpr(lex *lexer.PeekingLexer, minPrec int) (*Expr, error) {
	lhs, err := parseOperand(lex)
	if err != nil {
		return nil, err
	}
	for {
		token, err := lex.Peek(0)
		if err != nil {
			return nil, err
		}
		op := &Expr{Pos: token.Pos}
		if token.Type != operatorToken {
			break
		}
		err = op.Op.Capture([]string{token.Value})
		if err != nil {
			return nil, err
		}
		if info[op.Op].Priority < minPrec {
			break
		}
		_, _ = lex.Next()
		nextMinPrec := info[op.Op].Priority
		if !info[op.Op].RightAssociative {
			nextMinPrec++
		}
		rhs, err := parseExpr(lex, nextMinPrec)
		if err != nil {
			return nil, err
		}
		op.Left = lhs
		op.Right = rhs
		lhs = op
	}
	return lhs, nil
}

func parseOperand(lex *lexer.PeekingLexer) (*Expr, error) {
	pos := peekPos(lex)
	u := &Unary{Pos: pos}
	err := unaryParser.ParseFromLexer(lex, u, participle.AllowTrailing(true))
	if err != nil {
		return nil, err
	}
	return &Expr{Pos: pos, Unary: u}, nil
}

type Unary struct {
	Pos lexer.Position

	Op        Op         `@( "!" | "-" )?`
	Reference *Reference `@@`
}

func (u *Unary) accept(visitor VisitorFunc) error {
	return visitor(u, func(err error) error {
		if err != nil {
			return err
		}
		return VisitFunc(u.Reference, visitor)
	})
}

func (u *Unary) String() string {
	if u.Op != 0 {
		return fmt.Sprintf("%s%s", u.Op.String(), u.Reference.Describe())
	}
	return u.Reference.Describe()
}

type Terminal struct {
	Pos lexer.Position

	Tuple         []*Expr      `  "(" @@ ( "," @@ )* ")"`
	Literal       *Literal     `| @@`
	Ident         string       `| ( @Ident`
	TypeParameter []*Reference `    ("<" @@ ( "," @@ )* ","? ">" )?`
	Optional      bool         `    @"?"? )`
}

func (t Terminal) accept(visitor VisitorFunc) error {
	return visitor(t, func(err error) error {
		if err != nil {
			return err
		}
		switch {
		case t.Tuple != nil:
			for _, e := range t.Tuple {
				if err = VisitFunc(e, visitor); err != nil {
					return err
				}
			}

		case t.Literal != nil:
			return VisitFunc(t.Literal, visitor)

		case t.Ident != "":
			for _, tp := range t.TypeParameter {
				if err = VisitFunc(tp, visitor); err != nil {
					return err
				}
			}
			return nil

		default:
			panic("??")
		}
		return nil
	})
}

func (t Terminal) Describe() string {
	switch {
	case t.Tuple != nil:
		return "tuple/subexpression"

	case t.Ident != "":
		return fmt.Sprintf("reference to %q", t.Ident)

	case t.Literal != nil:
		return fmt.Sprintf("literal %s", t.Literal.Describe())
	}
	panic("??")
}

type Reference struct {
	Pos lexer.Position

	Terminal *Terminal      `@@`
	Next     *ReferenceNext `@@?`
}

func (t *Reference) accept(visitor VisitorFunc) error {
	return visitor(t, func(err error) error {
		if err != nil {
			return err
		}
		if err = VisitFunc(t.Terminal, visitor); err != nil {
			return err
		}
		if err = VisitFunc(t.Next, visitor); err != nil {
			return err
		}
		return nil
	})
}

func (t *Reference) Describe() string {
	description := t.Terminal.Describe()
	if t.Next != nil {
		description += " " + t.Next.Describe()
	}
	return description
}

type ReferenceNext struct {
	Subscript *Expr     `(   "[" @@ "]"`
	Reference *Terminal `  | "." @@`
	Call      *Call     `  | @@ )`

	Next *ReferenceNext `@@?`
}

func (r *ReferenceNext) accept(visitor VisitorFunc) error {
	return visitor(r, func(err error) error {
		if err != nil {
			return err
		}
		if err = VisitFunc(r.Subscript, visitor); err != nil {
			return err
		}
		if err = VisitFunc(r.Reference, visitor); err != nil {
			return err
		}
		if err = VisitFunc(r.Call, visitor); err != nil {
			return err
		}
		if err = VisitFunc(r.Next, visitor); err != nil {
			return err
		}
		return nil
	})
}

func (r *ReferenceNext) Describe() string {
	description := ""
	switch {
	case r.Subscript != nil:
		description = "subscript"

	case r.Reference != nil:
		description = fmt.Sprintf("reference %s", r.Reference.Describe())

	case r.Call != nil:
		description = "call"
	}

	if r.Next != nil {
		description += " " + r.Next.Describe()
	}
	return description
}

// A Number is an arbitrary precision number.
type Number big.Float

func (n *Number) GoString() string {
	return fmt.Sprintf("parser.Number(%s)", (*big.Float)(n).String())
}

func (n *Number) Capture(values []string) error {
	f, _, err := big.ParseFloat(values[0], 10, 30, big.ToZero)
	if err != nil {
		return err
	}
	*n = Number(*f)
	return nil
}

type Literal struct {
	Pos lexer.Position

	Number    *Number           `  @Number`
	Str       *string           `| @String`
	Bool      *bool             `| @("true" | "false")`
	DictOrSet *DictOrSetLiteral `| @@`
	Array     *ArrayLiteral     `| @@`
}

func (l *Literal) accept(visitor VisitorFunc) error {
	return visitor(l, func(err error) error {
		if err != nil {
			return err
		}
		switch {
		case l.Number != nil:
			return nil

		case l.Str != nil:
			return nil

		case l.Bool != nil:
			return nil

		case l.DictOrSet != nil:
			return VisitFunc(l.DictOrSet, visitor)

		case l.Array != nil:
			return VisitFunc(l.Array, visitor)

		default:
			panic("??")
		}
	})
}

func (l *Literal) Describe() string {
	switch {
	case l.Number != nil:
		return "number"

	case l.Str != nil:
		return "string"

	case l.Bool != nil:
		return "bool"

	case l.DictOrSet != nil:
		if l.DictOrSet.Entries[0].Value != nil {
			return "dict"
		}
		return "set"

	case l.Array != nil:
		return "array"

	default:
		panic("??")
	}
}

type DictOrSetLiteral struct {
	Pos lexer.Position

	Entries []*DictOrSetEntryLiteral `"{" @@ ( "," @@ )* ","? "}"`
}

func (d DictOrSetLiteral) accept(visitor VisitorFunc) error {
	return visitor(d, func(err error) error {
		if err != nil {
			return err
		}
		for _, entry := range d.Entries {
			if err = VisitFunc(entry, visitor); err != nil {
				return err
			}
		}
		return nil
	})
}

// DictOrSetEntryLiteral in the form {"key0": 1, "key1": 2} or {1, 2, 3}
type DictOrSetEntryLiteral struct {
	Pos lexer.Position

	Key *Expr `@@`
	// Dicts and sets both use "{}" as delimiters, so we'll allow intermingling
	// of key:value and value, then resolve during semantic analysis.
	Value *Expr `( ":" @@ )?`
}

func (d DictOrSetEntryLiteral) accept(visitor VisitorFunc) error {
	return visitor(d, func(err error) error {
		if err != nil {
			return err
		}
		if err = VisitFunc(d.Key, visitor); err != nil {
			return err
		}
		if err = VisitFunc(d.Value, visitor); err != nil {
			return err
		}
		return nil
	})
}

// ArrayLiteral in the form [1, 2, 3]
type ArrayLiteral struct {
	Pos lexer.Position

	Values []*Expr `"[" ( @@ ( "," @@ )* )? ","? "]"`
}

func (a ArrayLiteral) accept(visitor VisitorFunc) error {
	return visitor(a, func(err error) error {
		if err != nil {
			return err
		}
		for _, v := range a.Values {
			if err = VisitFunc(v, visitor); err != nil {
				return err
			}
		}
		return nil
	})
}

// ClassLiteral in the form {field:value, field:value, ...)
type ClassLiteral struct {
	Pos lexer.Position

	Fields []*ClassLiteralField `"{" ( @@ ( "," @@ )* )? ","? "}"`
}

type ClassLiteralField struct {
	Pos lexer.Position

	Key string `@Ident ":"`

	Nested *ClassLiteral `(  @@`
	Value  *Expr         ` | @@ )`
}

type Call struct {
	Pos lexer.Position

	Parameters []*Expr `"(" ( @@ ( "," @@ )* )? ","? ")"`
}

func (c Call) accept(visitor VisitorFunc) error {
	return visitor(c, func(err error) error {
		if err != nil {
			return err
		}
		for _, p := range c.Parameters {
			if err = VisitFunc(p, visitor); err != nil {
				return err
			}
		}
		return nil
	})
}

func peekPos(lex *lexer.PeekingLexer) lexer.Position {
	tok, _ := lex.Peek(0)
	return tok.Pos
}
