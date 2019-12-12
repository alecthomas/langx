package parser

import (
	"github.com/alecthomas/participle"
	"github.com/alecthomas/participle/lexer"
)

type opInfo struct {
	RightAssociative bool
	Priority         int
}

var info = map[string]opInfo{
	"+": {Priority: 1},
	"-": {Priority: 1},
	"*": {Priority: 2},
	"/": {Priority: 2},
	"%": {Priority: 2},
	"^": {RightAssociative: true, Priority: 3},
}

// Expr node in the AST.
type Expr struct {
	// Either Terminal or Op+left+Right will be present.
	Terminal *Unary

	Left  *Expr
	Op    string
	Right *Expr
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
		if token.Type != operatorToken || info[token.Value].Priority < minPrec {
			break
		}
		_, _ = lex.Next()
		op := &Expr{Op: token.Value}
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
	u := &Unary{}
	err := unaryParser.ParseFromLexer(lex, u, participle.AllowTrailing(true))
	if err != nil {
		return nil, err
	}
	return &Expr{Terminal: u}, nil
}

type Unary struct {
	Op       string    `  ( @( "!" | "-" )`
	Unary    *Unary    `    @@ )`
	Terminal *Terminal `| @@`
}

type Terminal struct {
	Tuple   []*Expr        `(   "(" @@ ( "," @@ )* ")" `
	Ident   string         `  | ( @Ident`
	Struct  *StructLiteral `      @@? )`
	Literal *Literal       `  | @@ )`

	Subscript *Expr     `( "[" @@ "]" )?`
	Reference *Terminal `( "." @@ )?`
	Call      *Call     `@@?`
}

type Literal struct {
	Int       *int64            `  @Int`
	Float     *float64          `| @Float`
	String    *string           `| @String`
	Bool      *bool             `| ( @"true" | "false" )`
	DictOrSet *DictOrSetLiteral `| @@`
	Array     *ArrayLiteral     `| @@`
	Struct    *StructLiteral    `| @@`
}

type DictOrSetLiteral struct {
	Entries []*DictOrSetEntryLiteral `"{" @@ ( "," @@ )* ","? "}"`
}

// DictOrSetEntryLiteral in the form {"key0": 1, "key1": 2} or {1, 2, 3}
type DictOrSetEntryLiteral struct {
	Key *Expr `@@`
	// Dicts and sets both use "{}" as delimiters, so we'll allow intermingling
	// of key:value and value, then resolve during semantic analysis.
	Value *Expr `( ":" @@ )?`
}

// ArrayLiteral in the form [1, 2, 3]
type ArrayLiteral struct {
	Values []*Expr `"[" ( @@ ( "," @@ )* )? ","? "]"`
}

// StructLiteral in the form {key: 1}
type StructLiteral struct {
	Fields []*StructLiteralField `"{" ( @@ ( "," @@ )* )? ","? "}"`
}

type StructLiteralField struct {
	Key string `@Ident ":"`

	Nested *StructLiteral `(  @@`
	Value  *Expr          ` | @@ )`
}

type Call struct {
	Parameters []*Expr `"(" ( @@ ( "," @@ )* )? ","? ")"`
}
