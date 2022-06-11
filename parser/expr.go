package parser

import (
	"fmt"
	"math/big"

	"github.com/alecthomas/participle/v2"
	"github.com/alecthomas/participle/v2/lexer"
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

func (e *Expr) children() (children []Node) {
	return []Node{e.Unary, e.Left, e.Right}
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
		token := lex.Peek()
		if token.EOF() {
			break
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
		lex.Next()
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

func (u *Unary) children() (children []Node) {
	return []Node{u.Reference}
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

func (i *InitParameter) children() (children []Node) {
	return []Node{i.Value}
}

type NewExpr struct {
	Mixin

	Type *Reference       `"new" @@`
	Init []*InitParameter `( "(" ( @@ ( "," @@ )* ","? )? ")" )?`
}

func (n *NewExpr) children() (children []Node) {
	children = []Node{n.Type}
	for _, param := range n.Init {
		children = append(children, param)
	}
	return
}

type Terminal struct {
	Mixin

	Tuple   []*Expr  `  "(" @@ ( "," @@ )* ")"`
	New     *NewExpr `| @@`
	Literal *Literal `| @@`
	Ident   string   `| @Ident`
}

func (t *Terminal) children() (children []Node) {
	for _, tup := range t.Tuple {
		children = append(children, tup)
	}
	children = append(children, t.New, t.Literal)
	return
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

func (t *Reference) children() (children []Node) {
	return []Node{t.Terminal, t.Next}
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
	Specialisation []*Reference `  | "<" @@ ( "," @@ )* ","? ">" (?= "(" | ";" | "=" | "." )` // https://stackoverflow.com/a/56519680
	Call           *Call        `  | @@ )`

	Next *ReferenceNext `@@?`
}

func (r *ReferenceNext) children() (children []Node) {
	children = []Node{r.Subscript, r.Reference}
	for _, spec := range r.Specialisation {
		children = append(children, spec)
	}
	return append(children, r.Call, r.Next)
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

func (s *String) children() (children []Node) {
	for _, frag := range s.Fragments {
		children = append(children, frag)
	}
	return
}

type StringFragment struct {
	Mixin

	Escaped string `(  @Escaped`
	Expr    *Expr  ` | "{" @@ "}"`
	String  string ` | @Chars)`
}

func (s *StringFragment) children() (children []Node) {
	return []Node{s.Expr}
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

func (l *Literal) children() (children []Node) {
	return []Node{l.Str, l.DictOrSet, l.Array}
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

func (d *DictOrSetLiteral) children() (children []Node) {
	for _, entry := range d.Entries {
		children = append(children, entry)
	}
	return
}

// DictOrSetEntryLiteral in the form {"key0": 1, "key1": 2} or {1, 2, 3}
type DictOrSetEntryLiteral struct {
	Mixin

	Key *Expr `@@`
	// Dicts and sets both use "{}" as delimiters, so we'll allow intermingling
	// of key:value and value, then resolve during semantic analysis.
	Value *Expr `( ":" @@ )?`
}

func (d *DictOrSetEntryLiteral) children() (children []Node) {
	return []Node{d.Key, d.Value}
}

// ArrayLiteral in the form [1, 2, 3]
type ArrayLiteral struct {
	Mixin

	Values []*Expr `"[" ( @@ ( "," @@ )* )? ","? "]"`
}

func (a *ArrayLiteral) children() (children []Node) {
	for _, val := range a.Values {
		children = append(children, val)
	}
	return
}

type Call struct {
	Mixin

	Parameters []*Expr `"(" ( @@ ( "," @@ )* )? ","? ")"`
}

func (c *Call) children() (children []Node) {
	for _, param := range c.Parameters {
		children = append(children, param)
	}
	return
}

func peekPos(lex *lexer.PeekingLexer) lexer.Position {
	return lex.Peek().Pos
}
