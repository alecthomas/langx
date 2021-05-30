package parser

import (
	"fmt"
	"io"
	"strings"

	"github.com/alecthomas/participle/v2"
	"github.com/alecthomas/participle/v2/lexer"
	"github.com/alecthomas/participle/v2/lexer/stateful"
)

var (
	// Note: "lex" is in this file to ensure correct initialisation ordering.
	lex = stateful.Must(stateful.Rules{
		"Root": {
			{"comment", `//.*|(?s:/\*.*?\*/)`, nil},
			{"backslash", `\\`, nil},
			{"whitespace", `[\r\t ]+`, nil},

			{"Modifier", `\b(pub|override|static)\b`, nil},
			{"Keyword", `\b(in|switch|case|default|if|enum|alias|let|fn|break|continue|for|throws|import|new)\b`, nil},
			{"Ident", `\b([[:alpha:]_]\w*)\b`, nil},
			{"Number", `\b(\d+(\.\d+)?)\b`, nil},
			{"String", `"`, stateful.Push("String")},
			{"LiteralString", `` + "`.*?`|'.*?'" + ``, nil},
			{"Newline", `\n`, nil},
			{"Operator", `->|%=|>=|<=|&&|\|\||==|!=`, nil},
			{"Assignment", `(\^=|\+=|-=|\*=|/=|\|=|&=|%=|=)`, nil},
			{"SingleOperator", `[-+*/<>%^!|&]`, nil},
			{"Punct", `[]` + "`" + `~[()@#${}:;?.,]`, nil},
		},
		"String": {
			{"Escaped", `\\.`, nil},
			{"StringEnd", `"`, stateful.Pop()},
			{"Expr", `{`, stateful.Push("StringExpr")},
			{"Chars", `[^{"\\]+`, nil},
		},
		"StringExpr": {
			{"ExprEnd", `}`, stateful.Pop()},
			stateful.Include("Root"),
		},
	})
	parser = participle.MustBuild(&AST{},
		participle.Lexer(&fixupLexerDefinition{}),
		participle.UseLookahead(1),
		unquoteLiteral(),
		// participle.Unquote(),
	)
	unaryParser = participle.MustBuild(&Unary{},
		participle.Lexer(&fixupLexerDefinition{}),
		participle.UseLookahead(1),
		unquoteLiteral(),
		// participle.Unquote(),
	)

	identToken          = lex.Symbols()["Ident"]
	stringEndToken      = lex.Symbols()["StringEnd"]
	numberToken         = lex.Symbols()["Number"]
	operatorToken       = lex.Symbols()["Operator"]
	singleOperatorToken = lex.Symbols()["SingleOperator"]
)

func unquoteLiteral() participle.Option {
	return participle.Map(func(token lexer.Token) (lexer.Token, error) {
		token.Value = token.Value[1 : len(token.Value)-1]
		return token, nil
	}, "LiteralString")
}

// Decls is a group of declarations.
type Decls interface {
	Decls() []Decl
}

type AST struct {
	Mixin

	Declarations []*RootDecl `@@*`
}

func (a *AST) children() (children []Node) {
	for _, child := range a.Declarations {
		children = append(children, child)
	}
	return
}

func (a *AST) Decls() []Decl {
	decls := []Decl{}
	for _, d := range a.Declarations {
		decls = append(decls, d.Decl())
	}
	return decls
}

// RootDecl is a top-level declaration.
type RootDecl struct {
	Mixin

	Modifiers Modifiers `@Modifier*`

	Class  *ClassDecl  `(   @@ ";"?`
	Import *ImportDecl `  | @@ ";"?`
	Enum   *EnumDecl   `  | @@ ";"?`
	Var    *VarDecl    `  | @@ ";"`
	Func   *FuncDecl   `  | @@ ";"? ) `
}

func (r *RootDecl) children() (children []Node) {
	return []Node{r.Class, r.Import, r.Enum, r.Var, r.Func}
}

func (r *RootDecl) Decl() Decl {
	switch {
	case r.Class != nil:
		return r.Class

	case r.Import != nil:
		return r.Import

	case r.Enum != nil:
		return r.Enum

	case r.Var != nil:
		return r.Var

	case r.Func != nil:
		return r.Func

	default:
		panic("?")
	}
}

type ImportDecl struct {
	Mixin

	Alias  string  `"import" @Ident?`
	Import *String `@@`
}

func (i *ImportDecl) children() (children []Node) {
	return []Node{i.Import}
}

func (i *ImportDecl) decl() {}

type EnumDecl struct {
	Mixin

	Type    *NamedTypeDecl `"enum" @@ "{"`
	Members []*EnumMember  `( @@ ( ";" @@ )* ";"? )? "}"`
}

func (e *EnumDecl) children() (children []Node) {
	children = append(children, e.Type)
	for _, member := range e.Members {
		children = append(children, member)
	}
	return
}

func (e *EnumDecl) Decls() []Decl {
	decls := []Decl{}
	for _, member := range e.Members {
		decls = append(decls, member.Decl())
	}
	return decls
}

func (e *EnumDecl) decl() {}

type EnumMember struct {
	Mixin

	Modifiers Modifiers `@Modifier*`

	CaseDecl *CaseDecl `(  @@`
	FuncDecl *FuncDecl ` | @@ )`
}

func (e *EnumMember) children() (children []Node) {
	return
}

// Decl types.
type Decl interface {
	Node
	decl()
}

func (e *EnumMember) Decl() Decl {
	switch {
	case e.CaseDecl != nil:
		return e.CaseDecl

	default:
		panic("??")
	}
}

type CaseDecl struct {
	Mixin

	Name string    `"case" @Ident`
	Type *TypeDecl `( "(" @@ ")" )?`
}

func (c *CaseDecl) children() (children []Node) {
	return
}

func (c *CaseDecl) decl() {}

type ClassDecl struct {
	Mixin

	Type    *NamedTypeDecl `"class" @@ "{"`
	Members []*ClassMember `( @@ ( ";" @@ )* ";"? )? "}"`
}

func (c *ClassDecl) children() (children []Node) {
	return
}

func (c *ClassDecl) Decls() []Decl {
	decls := []Decl{}
	for _, decl := range c.Members {
		decls = append(decls, decl.Decl())
	}
	return decls
}

func (c *ClassDecl) decl() {}

type ClassMember struct {
	Mixin

	Modifiers Modifiers `@Modifier*`

	VarDecl         *VarDecl         `(  @@`
	FuncDecl        *FuncDecl        ` | @@`
	ClassDecl       *ClassDecl       ` | @@`
	EnumDecl        *EnumDecl        ` | @@`
	InitialiserDecl *InitialiserDecl ` | @@ )`
}

func (c *ClassMember) children() (children []Node) {
	return
}

func (c *ClassMember) Decl() Decl {
	switch {
	case c.VarDecl != nil:
		return c.VarDecl

	case c.FuncDecl != nil:
		return c.FuncDecl

	case c.ClassDecl != nil:
		return c.ClassDecl

	case c.EnumDecl != nil:
		return c.EnumDecl

	case c.InitialiserDecl != nil:
		return c.InitialiserDecl

	default:
		panic("??")
	}
}

type InitialiserDecl struct {
	Mixin

	Parameters []*Parameters `"init" "(" ( @@ ( "," @@ )* )? ","? ")"`
	Throws     bool          `@"throws"?`
	Body       *Block        `@@`
}

func (i *InitialiserDecl) children() (children []Node) {
	for _, param := range i.Parameters {
		children = append(children, param)
	}
	return append(children, i.Body)
}

func (i *InitialiserDecl) decl() {}

type TypeDecl struct {
	Mixin

	Named     *NamedTypeDecl     `  @@`
	Array     *ArrayTypeDecl     `| @@`
	DictOrSet *DictOrSetTypeDecl `| @@`
}

func (t *TypeDecl) String() string {
	switch {
	case t.Named != nil:
		return t.Named.String()
	case t.Array != nil:
		return t.Array.String()
	case t.DictOrSet != nil:
		return t.DictOrSet.String()
	default:
		panic("??")
	}
}

func (t TypeDecl) children() (children []Node) {
	return []Node{t.Named, t.Array, t.DictOrSet}
}

// DictOrSeyTypeDecl in the form {<type>: <type>} or {<type>}
type DictOrSetTypeDecl struct {
	Mixin

	Key *TypeDecl `"{" @@`
	// Dicts and sets both use "{}" as delimiters, so we'll allow intermingling
	// of key:value and value, then resolve during semantic analysis.
	Value *TypeDecl `( ":" @@ )? "}"`
}

func (d *DictOrSetTypeDecl) String() string {
	if d.Value == nil {
		return fmt.Sprintf("{%s}", d.Key)
	}
	return fmt.Sprintf("{%s: %s}", d.Key, d.Value)
}

func (d *DictOrSetTypeDecl) children() (children []Node) {
	return []Node{d.Key, d.Value}
}

// ArrayTypeDecl in the form [<type>]
type ArrayTypeDecl struct {
	Mixin

	Element *TypeDecl `"[" @@ "]"`
}

func (a *ArrayTypeDecl) String() string {
	return fmt.Sprintf("[%s]", a.Element)
}

func (a *ArrayTypeDecl) children() (children []Node) {
	return []Node{a.Element}
}

type NamedTypeDecl struct {
	Mixin

	Type          string           `@Ident`
	TypeParameter []*TypeParamDecl `( "<" @@ ( "," @@ )* ","? ">" )?`
}

func (n *NamedTypeDecl) String() string {
	params := []string{}
	for _, p := range n.TypeParameter {
		params = append(params, p.String())
	}
	if len(params) == 0 {
		return n.Type
	}
	return fmt.Sprintf("%s<%s>", n.Type, strings.Join(params, ", "))
}

func (t *NamedTypeDecl) children() (children []Node) {
	for _, param := range t.TypeParameter {
		children = append(children, param)
	}
	return
}

// TypeParamDecl represents a generic type parameter and its optional constraints.
type TypeParamDecl struct {
	Mixin

	Name        string       `@Ident`
	Constraints []*Reference `( ":" @@ ( "," @@ )* )?`
}

func (t *TypeParamDecl) children() (children []Node) {
	for _, constraint := range t.Constraints {
		children = append(children, constraint)
	}
	return
}

func (t *TypeParamDecl) String() string {
	constraints := []string{}
	for _, c := range t.Constraints {
		constraints = append(constraints, c.Describe())
	}
	if len(constraints) == 0 {
		return t.Name
	}
	return fmt.Sprintf("%s: %s", t.Name, strings.Join(constraints, ", "))
}

// Parameters of a function/constructor declaration.
type Parameters struct {
	Mixin

	Names []string   `@Ident ("," @Ident)*`
	Type  *Reference `":" @@`
}

func (p *Parameters) children() (children []Node) {
	return []Node{p.Type}
}

// VarDecl represents the declaration of new variables.
type VarDecl struct {
	Mixin

	// let a, b, c int
	// let a = 1, b = 2
	// let a int = 1, b int = 2
	Vars []*VarDeclAsgn `"let" @@ ( "," @@ )*`
}

func (v *VarDecl) children() (children []Node) {
	for _, v := range v.Vars {
		children = append(children, v)
	}
	return
}

func (v *VarDecl) decl() {}

type VarDeclAsgn struct {
	Mixin

	Name    string `@Ident`
	Type    *Expr  `( ":" @@ )?`
	Default *Expr  `( "=" @@ )?`
}

func (v *VarDeclAsgn) children() (children []Node) {
	return []Node{v.Type, v.Default}
}

// ExprStmt is either an assignment, or a function call.
//
// Other, invalid, expressions will be flagged during semantic analysis.
type ExprStmt struct {
	Mixin

	LHS *Expr `@@`
	Op  Op    `( @Assignment`
	RHS *Expr `  @@ )?`
}

func (a *ExprStmt) children() (children []Node) {
	return []Node{a.LHS, a.RHS}
}

type Stmt struct {
	Mixin

	Return    *ReturnStmt `  @@`
	If        *IfStmt     `| @@`
	For       *ForStmt    `| @@`
	Switch    *SwitchStmt `| @@`
	Block     *Block      `| @@`
	VarDecl   *VarDecl    `| @@`
	FuncDecl  *FuncDecl   `| @@`
	ClassDecl *ClassDecl  `| @@`
	EnumDecl  *EnumDecl   `| @@`
	ExprStmt  *ExprStmt   `| @@`
}

func (s *Stmt) children() (children []Node) {
	return []Node{s.Return, s.If, s.For, s.Switch, s.Block, s.VarDecl, s.FuncDecl, s.ClassDecl, s.EnumDecl, s.ExprStmt}
}

type Block struct {
	Mixin

	Statements []*Stmt `"{" ( @@ ( ";" @@ )* ";"? )? "}"`
}

func (b *Block) children() (children []Node) {
	for _, stmt := range b.Statements {
		children = append(children, stmt)
	}
	return
}

type ForStmt struct {
	Mixin

	Target *Reference `"for" @@`
	Source *Expr      `"in" @@`
	Body   *Block     `@@`
}

func (i *ForStmt) children() (children []Node) {
	return []Node{i.Target, i.Source, i.Body}
}

type IfStmt struct {
	Mixin

	Condition *Expr  `"if" @@`
	Main      *Block `@@`
	Else      *Block `( "else" @@ )?`
}

func (i *IfStmt) children() (children []Node) {
	return []Node{i.Condition, i.Main, i.Else}
}

type SwitchStmt struct {
	Mixin

	Target *Expr       `"switch" @@ "{"`
	Cases  []*CaseStmt `@@* "}"`
}

func (s *SwitchStmt) children() (children []Node) {
	children = []Node{s.Target}
	for _, c := range s.Cases {
		children = append(children, c)
	}
	return
}

type CaseStmt struct {
	Mixin

	Default bool        `( @"default"`
	Case    *CaseSelect `  | "case" @@ ) ":"`
	Body    []*Stmt     `( @@ ( ";" @@ )* ";"? )?`
}

func (c *CaseStmt) children() (children []Node) {
	children = []Node{c.Case}
	for _, stmt := range c.Body {
		children = append(children, stmt)
	}
	return
}

type CaseSelect struct {
	Mixin

	EnumCase *EnumCase `  @@`
	ExprCase *Expr     `| @@`
}

func (c *CaseSelect) children() (children []Node) {
	return []Node{c.EnumCase, c.ExprCase}
}

type EnumCase struct {
	Mixin

	Case string `"." @Ident`
	Var  string `( "(" @Ident ")" )?`
}

func (e *EnumCase) children() (children []Node) { return }

type ReturnStmt struct {
	Mixin

	Value *Expr `"return" @@?`
}

func (r *ReturnStmt) children() (children []Node) {
	return []Node{r.Value}
}

type FuncDecl struct {
	Mixin

	Name       string        `"fn" @Ident "("`
	Parameters []*Parameters `( @@ ( "," @@ )* )? ","? ")"`
	Throws     bool          `@"throws"?`
	Return     *Expr         `( ":" @@ )?`
	Body       *Block        `@@`
}

func (f *FuncDecl) children() (children []Node) {
	for _, param := range f.Parameters {
		children = append(children, param)
	}
	children = append(children, f.Return, f.Body)
	return
}

func (f *FuncDecl) decl() {}

func Parse(r io.Reader) (*AST, error) {
	ast := &AST{}
	return ast, parser.Parse("", r, ast)
}

func ParseString(s string) (*AST, error) {
	ast := &AST{}
	return ast, parser.ParseString("", s, ast)
}
