package parser

import (
	"fmt"
	"math/big"

	"github.com/alecthomas/participle"
	"github.com/alecthomas/participle/lexer"
)

//go:generate stringer -type Op

type Op int

const (
	OpModAsgn Op = iota // %=
	OpGe                // >=
	OpLe                // <=
	OpAnd               // &&
	OpOr                // ||
	OpEq                // ==
	OpNe                // !=
	OpAddAsgn           // +=
	OpSubAsgn           // -=
	OpMulAsgn           // *=
	OpDivAsgn           // /=
	OpPowAsgn           // ^=
	OpSub               // -
	OpAsgn              // =
	OpAdd               // +
	OpMul               // *
	OpDiv               // /
	OpLt                // <
	OpGt                // >
	OpMod               // %
	OpPow               // ^
	OpNot               // !
)

func (o Op) GoString() string {
	switch o {
	case OpModAsgn:
		return "parser.OpModAssgn"
	case OpGe:
		return "parser.OpGe"
	case OpLe:
		return "parser.OpLe"
	case OpAnd:
		return "parser.OpAnd"
	case OpOr:
		return "parser.OpOr"
	case OpEq:
		return "parser.OpEq"
	case OpNe:
		return "parser.OpNe"
	case OpAddAsgn:
		return "parser.OpAddAsgn"
	case OpSubAsgn:
		return "parser.OpSubAsgn"
	case OpMulAsgn:
		return "parser.OpMulAsgn"
	case OpDivAsgn:
		return "parser.OpDivAsgn"
	case OpPowAsgn:
		return "parser.OpPowAsgn"
	case OpSub:
		return "parser.OpSub"
	case OpAsgn:
		return "parser.OpAsgn"
	case OpAdd:
		return "parser.OpAdd"
	case OpMul:
		return "parser.OpMul"
	case OpDiv:
		return "parser.OpDiv"
	case OpLt:
		return "parser.OpLt"
	case OpGt:
		return "parser.OpGt"
	case OpMod:
		return "parser.OpMod"
	case OpPow:
		return "parser.OpPow"
	case OpNot:
		return "parser.OpNot"
	default:
		panic("??")
	}
}

func (o *Op) Capture(values []string) error {
	switch values[0] {
	case "%=":
		*o = OpModAsgn
	case ">=":
		*o = OpGe
	case "<=":
		*o = OpLe
	case "&&":
		*o = OpAnd
	case "||":
		*o = OpOr
	case "==":
		*o = OpEq
	case "!=":
		*o = OpNe
	case "+=":
		*o = OpAddAsgn
	case "-=":
		*o = OpSubAsgn
	case "/=":
		*o = OpDivAsgn
	case "*=":
		*o = OpMulAsgn
	case "^=":
		*o = OpPowAsgn
	case "=":
		*o = OpAsgn
	case "-":
		*o = OpSub
	case "+":
		*o = OpAdd
	case "*":
		*o = OpMul
	case "/":
		*o = OpDiv
	case "%":
		*o = OpMod
	case "<":
		*o = OpLt
	case ">":
		*o = OpGt
	case "^":
		*o = OpPow
	case "!":
		*o = OpNot
	default:
		panic(values[0])
	}
	return nil
}

type opInfo struct {
	RightAssociative bool
	Priority         int
}

var info = map[Op]opInfo{
	OpAdd: {Priority: 1},
	OpSub: {Priority: 1},
	OpMul: {Priority: 2},
	OpDiv: {Priority: 2},
	OpMod: {Priority: 2},
	OpPow: {RightAssociative: true, Priority: 3},
}

// Expr node in the AST.
type Expr struct {
	Pos lexer.Position

	// Either Unary or Op+left+Right will be present.
	Unary *Unary

	Left  *Expr
	Op    Op
	Right *Expr
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

	Op       Op        `@( "!" | "-" )?`
	Terminal *Terminal `@@`
}

func (u *Unary) String() string {
	if u.Op != 0 {
		return fmt.Sprintf("%s%s", u.Op.String(), u.Terminal.String())
	}
	return u.Terminal.String()
}

type Terminal struct {
	Pos lexer.Position

	Tuple   []*Expr       `(   "(" @@ ( "," @@ )* ")" `
	Ident   string        `  | ( @Ident`
	Class   *ClassLiteral `      @@? )`
	Literal *Literal      `  | @@ )`

	Subscript *Expr     `( "[" @@ "]" )?`
	Reference *Terminal `( "." @@ )?`
	Call      *Call     `@@?`
}

func (t *Terminal) Describe() string {
	description := ""
	switch {
	case t.Tuple != nil:
		description = "tuple/subexpression"

	case t.Class != nil:
		description = fmt.Sprintf("struct literal %q", t.Ident)

	case t.Ident != "":
		description = fmt.Sprintf("reference to %q", t.Ident)

	case t.Literal != nil:
		description = fmt.Sprintf("literal %s", t.Literal.Describe())
	}

	if t.Subscript != nil {
		description += " subscript"
	}
	if t.Reference != nil {
		description = fmt.Sprintf("%s reference %s", description, t.Reference.Describe())
	}
	if t.Call != nil {
		description += " call"
	}

	return description
}

func (t *Terminal) String() string {
	panic("??")
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

	Number *Number `  @Number`
	Str    *string `| @String`
	Bool   *bool   `| ( @"true" | "false" )`
	// DictOrSet *DictOrSetLiteral `| @@`
	// Array     *ArrayLiteral     `| @@`
	// Class    *ClassLiteral    `| @@`
}

func (l *Literal) Describe() string {
	switch {
	case l.Number != nil:
		return "number"

	case l.Str != nil:
		return "string"

	case l.Bool != nil:
		return "bool"

	// case l.DictOrSet != nil:
	// 	return "dict or set"
	//
	// case l.Array != nil:
	// 	return "array"
	//
	// case l.Class != nil:
	// 	return "struct"

	default:
		panic("??")
	}
}

type DictOrSetLiteral struct {
	Pos lexer.Position

	Entries []*DictOrSetEntryLiteral `"{" @@ ( "," @@ )* ","? "}"`
}

// DictOrSetEntryLiteral in the form {"key0": 1, "key1": 2} or {1, 2, 3}
type DictOrSetEntryLiteral struct {
	Pos lexer.Position

	Key *Expr `@@`
	// Dicts and sets both use "{}" as delimiters, so we'll allow intermingling
	// of key:value and value, then resolve during semantic analysis.
	Value *Expr `( ":" @@ )?`
}

// ArrayLiteral in the form [1, 2, 3]
type ArrayLiteral struct {
	Pos lexer.Position

	Values []*Expr `"[" ( @@ ( "," @@ )* )? ","? "]"`
}

// ClassLiteral in the form {key: 1}
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

func peekPos(lex *lexer.PeekingLexer) lexer.Position {
	tok, _ := lex.Peek(0)
	return tok.Pos
}
