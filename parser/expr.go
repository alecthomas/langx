package parser

import (
	"fmt"
	"math/big"

	"github.com/alecthomas/participle"
	"github.com/alecthomas/participle/lexer"
)

// Expr represents an expression.
type Expr struct {
	Mixin

	// Either Unary or Op + Left + Right will be present.
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
		if e.Unary != nil {
			return VisitFunc(e.Unary, visitor)
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

type opInfo struct {
	RightAssociative bool
	Priority         int
}

var info = map[Op]opInfo{
	OpAdd:    {Priority: 1},
	OpSub:    {Priority: 1},
	OpMul:    {Priority: 2},
	OpDiv:    {Priority: 2},
	OpMod:    {Priority: 2},
	OpPow:    {RightAssociative: true, Priority: 3},
	OpBitOr:  {Priority: 4},
	OpBitAnd: {Priority: 4},
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
		expr := &Expr{Mixin: Mixin{token.Pos}}
		if token.Type != operatorToken && token.Type != singleOperatorToken {
			break
		}
		err = expr.Op.Capture([]string{token.Value})
		if err != nil {
			return lhs, nil
		}
		if info[expr.Op].Priority < minPrec {
			break
		}
		_, _ = lex.Next()
		nextMinPrec := info[expr.Op].Priority
		if !info[expr.Op].RightAssociative {
			nextMinPrec++
		}
		rhs, err := parseExpr(lex, nextMinPrec)
		if err != nil {
			return nil, err
		}
		expr.Left = lhs
		expr.Right = rhs
		lhs = expr
	}
	return lhs, nil
}

func parseOperand(lex *lexer.PeekingLexer) (*Expr, error) {
	pos := peekPos(lex)
	u := &Unary{Mixin: Mixin{pos}}
	err := unaryParser.ParseFromLexer(lex, u, participle.AllowTrailing(true))
	if err != nil {
		return nil, err
	}
	return &Expr{Mixin: Mixin{pos}, Unary: u}, nil
}

type Unary struct {
	Mixin

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

type InitParameter struct {
	Mixin

	Name  string `@Ident`
	Value *Expr  `"=" @@`
}

func (i *InitParameter) accept(visitor VisitorFunc) error {
	return visitor(i, func(err error) error {
		if err != nil {
			return err
		}
		return VisitFunc(i.Value, visitor)
	})
}

type NewExpr struct {
	Mixin

	Type *Reference       `"new" @@`
	Init []*InitParameter `( "(" ( @@ ( "," @@ )* ","? )? ")" )?`
}

func (n *NewExpr) accept(visitor VisitorFunc) error {
	return visitor(n, func(err error) error {
		if err != nil {
			return err
		}
		if err = VisitFunc(n.Type, visitor); err != nil {
			return err
		}
		for _, param := range n.Init {
			if err = VisitFunc(param, visitor); err != nil {
				return err
			}
		}
		return nil
	})
}

type Terminal struct {
	Mixin

	Tuple   []*Expr  `  "(" @@ ( "," @@ )* ")"`
	New     *NewExpr `| @@`
	Literal *Literal `| @@`
	Ident   string   `| @Ident`
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

		case t.New != nil:
			return VisitFunc(t.New, visitor)

		case t.Ident != "":
			return nil

		default:
			panic("??")
		}
		return nil
	})
}

func (t *Terminal) Describe() string {
	switch {
	case t.New != nil:
		return "new"

	case t.Tuple != nil:
		return "tuple/subexpression"

	case t.Ident != "":
		return fmt.Sprintf("reference to %q", t.Ident)

	case t.Literal != nil:
		return fmt.Sprintf("literal %s", t.Literal.Describe())
	}
	panic("??")
}

// A Reference to a value or a type.
type Reference struct {
	Mixin

	Terminal *Terminal      `@@`
	Next     *ReferenceNext `@@?`
	Optional bool           `@"?"?`
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
	Mixin

	Subscript      *Expr        `(   "[" @@ "]"`
	Reference      *Terminal    `  | "." @@`
	Specialisation []*Reference `  | "<" @@ ( "," @@ )* ","? ">"`
	Call           *Call        `  | @@ )`

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
		for _, ref := range r.Specialisation {
			if err = VisitFunc(ref, visitor); err != nil {
				return err
			}
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

	case r.Specialisation != nil:
		description = "specialisation"

	default:
		panic("??")
	}

	if r.Next != nil {
		description += " " + r.Next.Describe()
	}
	return description
}

// A Number is an arbitrary precision number.
type Number big.Float

func (n *Number) GoString() string {
	return fmt.Sprintf("parser.Number(%s)", n.String())
}

func (n *Number) String() string {
	return (*big.Float)(n).String()
}

func (n *Number) Capture(values []string) error {
	f, _, err := big.ParseFloat(values[0], 10, 30, big.ToZero)
	if err != nil {
		return err
	}
	*n = Number(*f)
	return nil
}

// String with interpolated expressions.
//
// eg.
//
//    "A string with expressions {1 + 2 / 3} {function()} calls and {variables}."
type String struct {
	Mixin

	Fragments []*StringFragment `"\"" @@* "\""`
}

func (s *String) accept(visitor VisitorFunc) error {
	return visitor(s, func(err error) error {
		if err != nil {
			return err
		}
		for _, frag := range s.Fragments {
			if frag.Expr != nil {
				if err = VisitFunc(frag.Expr, visitor); err != nil {
					return err
				}
			}
		}
		return nil
	})
}

type StringFragment struct {
	Escaped string `(  @Escaped`
	Expr    *Expr  ` | "{" @@ "}"`
	String  string ` | @Chars)`
}

type Bool bool

func (b *Bool) Capture(values []string) error {
	*b = values[0] == "true"
	return nil
}

type Literal struct {
	Mixin

	Number    *Number           `  @Number`
	Str       *String           `| @@`
	LitStr    *string           `| @LiteralString`
	Bool      *Bool             `| @("true" | "false")`
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
			return VisitFunc(l.Str, visitor)

		case l.LitStr != nil:
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

	case l.LitStr != nil:
		return "literal string"

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
	Mixin

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
	Mixin

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
	Mixin

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
	Mixin

	Fields []*ClassLiteralField `"{" ( @@ ( "," @@ )* )? ","? "}"`
}

type ClassLiteralField struct {
	Mixin

	Key string `@Ident ":"`

	Nested *ClassLiteral `(  @@`
	Value  *Expr         ` | @@ )`
}

type Call struct {
	Mixin

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
