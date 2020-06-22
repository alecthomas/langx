package parser

import (
	"fmt"
	"io"
	"strings"

	"github.com/alecthomas/participle"
	"github.com/alecthomas/participle/lexer"
	"github.com/alecthomas/participle/lexer/regex"
)

var (
	// Note: "lex" is in this file to ensure correct initialisation ordering.
	lex = lexer.Must(regex.New(`
		comment = //.*|(?s:/\*.*?\*/)
		backslash = \\
		whitespace = [\r\t ]+
	
		Modifier = \b(pub|override|static)\b
		Keyword = \b(in|switch|case|default|if|enum|alias|let|fn|break|continue|for|throws|import|new)\b
		Ident = \b([[:alpha:]_]\w*)\b
		Number = \b(\d+(\.\d+)?)\b
		String = "(\\.|[^"])*"
		LiteralString = ` + "`.*?`|'.*?'" + `
		Newline = \n
		Operator = ->|%=|>=|<=|&&|\|\||==|!=
		Assignment = (\^=|\+=|-=|\*=|/=|\|=|&=|%=|=)
		SingleOperator = [-+*/<>%^!|&]
		Punct = []` + "`" + `~[()@#${}:;?.,]
	`))
	parser = participle.MustBuild(&AST{},
		participle.Lexer(&fixupLexerDefinition{}),
		participle.UseLookahead(1),
		unquoteLiteral(),
		participle.Unquote(),
	)
	unaryParser = participle.MustBuild(&Unary{},
		participle.Lexer(&fixupLexerDefinition{}),
		participle.UseLookahead(1),
		unquoteLiteral(),
		participle.Unquote(),
	)

	identToken          = lex.Symbols()["Ident"]
	stringToken         = lex.Symbols()["String"]
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

func (a *AST) accept(visitor VisitorFunc) error {
	return visitor(a, func(err error) error {
		if err != nil {
			return err
		}
		for _, decl := range a.Declarations {
			err = VisitFunc(decl, visitor)
			if err != nil {
				return err
			}
		}
		return nil
	})
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

func (r *RootDecl) accept(visitor VisitorFunc) error {
	return visitor(r, func(err error) error {
		if err != nil {
			return err
		}
		return VisitFunc(r.Decl(), visitor)
	})
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

	Alias  string `"import" @Ident?`
	Import string `@String`
}

func (i *ImportDecl) accept(visitor VisitorFunc) error {
	return visitor(i, func(err error) error { return err })
}

func (i *ImportDecl) decl() {}

type EnumDecl struct {
	Mixin

	Type    *NamedTypeDecl `"enum" @@ "{"`
	Members []*EnumMember  `( @@ ( ";" @@ )* ";"? )? "}"`
}

func (e *EnumDecl) accept(visitor VisitorFunc) error {
	return visitor(e, func(err error) error {
		if err != nil {
			return err
		}
		if err = VisitFunc(e.Type, visitor); err != nil {
			return err
		}
		for _, m := range e.Members {
			if err = VisitFunc(m, visitor); err != nil {
				return err
			}
		}
		return nil
	})
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

func (e *EnumMember) accept(visitor VisitorFunc) error {
	return visitor(e, func(err error) error {
		if err != nil {
			return err
		}
		return VisitFunc(e.Decl(), visitor)
	})
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

func (c *CaseDecl) accept(visitor VisitorFunc) error {
	return visitor(c, func(err error) error {
		if err != nil {
			return err
		}
		return VisitFunc(c.Type, visitor)
	})
}

func (c *CaseDecl) decl() {}

type ClassDecl struct {
	Mixin

	Type    *NamedTypeDecl `"class" @@ "{"`
	Members []*ClassMember `( @@ ( ";" @@ )* ";"? )? "}"`
}

func (c *ClassDecl) accept(visitor VisitorFunc) error {
	return visitor(c, func(err error) error {
		if err != nil {
			return err
		}
		if err := VisitFunc(c.Type, visitor); err != nil {
			return err
		}
		for _, member := range c.Members {
			if err := VisitFunc(member, visitor); err != nil {
				return err
			}
		}
		return nil
	})
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

func (c *ClassMember) accept(visitor VisitorFunc) error {
	return visitor(c, func(err error) error {
		if err != nil {
			return err
		}
		return VisitFunc(c.Decl(), visitor)
	})
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

func (i *InitialiserDecl) accept(visitor VisitorFunc) error {
	return visitor(i, func(err error) error {
		if err != nil {
			return err
		}
		for _, p := range i.Parameters {
			if err = VisitFunc(p, visitor); err != nil {
				return err
			}
		}
		return VisitFunc(i.Body, visitor)
	})
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

func (t TypeDecl) accept(visitor VisitorFunc) error {
	return visitor(t, func(err error) error {
		if err != nil {
			return err
		}
		switch {
		case t.Named != nil:
			return t.Named.accept(visitor)

		default:
			panic("??")
		}
	})
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

func (d *DictOrSetTypeDecl) accept(visitor VisitorFunc) error {
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

// ArrayTypeDecl in the form [<type>]
type ArrayTypeDecl struct {
	Mixin

	Element *TypeDecl `"[" @@ "]"`
}

func (a *ArrayTypeDecl) String() string {
	return fmt.Sprintf("[%s]", a.Element)
}

func (a *ArrayTypeDecl) accept(visitor VisitorFunc) error {
	return visitor(a, func(err error) error {
		if err != nil {
			return err
		}
		if err = VisitFunc(a.Element, visitor); err != nil {
			return err
		}
		return nil
	})
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

func (t *NamedTypeDecl) accept(visitor VisitorFunc) error {
	return visitor(t, func(err error) error {
		if err != nil {
			return err
		}
		for _, tp := range t.TypeParameter {
			if err = VisitFunc(tp, visitor); err != nil {
				return err
			}
		}
		return nil
	})
}

// TypeParamDecl represents a generic type parameter and its optional constraints.
type TypeParamDecl struct {
	Mixin

	Name        string       `@Ident`
	Constraints []*Reference `( ":" @@ ( "," @@ )* )?`
}

func (t TypeParamDecl) accept(visitor VisitorFunc) error {
	return visitor(t, func(err error) error {
		if err != nil {
			return err
		}
		for _, c := range t.Constraints {
			if err = VisitFunc(c, visitor); err != nil {
				return err
			}
		}
		return nil
	})
}

func (t TypeParamDecl) String() string {
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

func (p Parameters) accept(visitor VisitorFunc) error {
	return visitor(p, func(err error) error {
		if err != nil {
			return err
		}
		return VisitFunc(p.Type, visitor)
	})
}

// VarDecl represents the declaration of new variables.
type VarDecl struct {
	Mixin

	// let a, b, c int
	// let a = 1, b = 2
	// let a int = 1, b int = 2
	Vars []*VarDeclAsgn `"let" @@ ( "," @@ )*`
}

func (v *VarDecl) accept(visitor VisitorFunc) error {
	return visitor(v, func(err error) error {
		if err != nil {
			return err
		}
		for _, v := range v.Vars {
			if err = VisitFunc(v, visitor); err != nil {
				return err
			}
		}
		return nil
	})
}

func (v *VarDecl) decl() {}

type VarDeclAsgn struct {
	Mixin

	Name    string `@Ident`
	Type    *Expr  `( ":" @@ )?`
	Default *Expr  `( "=" @@ )?`
}

func (v VarDeclAsgn) accept(visitor VisitorFunc) error {
	return visitor(v, func(err error) error {
		if err != nil {
			return err
		}
		if err = VisitFunc(v.Type, visitor); err != nil {
			return err
		}
		return VisitFunc(v.Default, visitor)
	})
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

func (a *ExprStmt) accept(visitor VisitorFunc) error {
	return visitor(a, func(err error) error {
		if err != nil {
			return err
		}
		if err = VisitFunc(a.LHS, visitor); err != nil {
			return err
		}
		if err = VisitFunc(a.RHS, visitor); err != nil {
			return err
		}
		return nil
	})
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

func (s Stmt) accept(visitor VisitorFunc) error {
	return visitor(s, func(err error) error {
		if err != nil {
			return err
		}
		switch {
		case s.Return != nil:
			return VisitFunc(s.Return, visitor)
		case s.If != nil:
			return VisitFunc(s.If, visitor)
		case s.For != nil:
			return VisitFunc(s.For, visitor)
		case s.Switch != nil:
			return VisitFunc(s.Switch, visitor)
		case s.Block != nil:
			return VisitFunc(s.Block, visitor)
		case s.VarDecl != nil:
			return VisitFunc(s.VarDecl, visitor)
		case s.FuncDecl != nil:
			return VisitFunc(s.FuncDecl, visitor)
		case s.ClassDecl != nil:
			return VisitFunc(s.ClassDecl, visitor)
		case s.EnumDecl != nil:
			return VisitFunc(s.EnumDecl, visitor)
		case s.ExprStmt != nil:
			return VisitFunc(s.ExprStmt, visitor)
		default:
			panic("??")
		}
	})
}

type Block struct {
	Mixin

	Statements []*Stmt `"{" ( @@ ( ";" @@ )* ";"? )? "}"`
}

func (b Block) accept(visitor VisitorFunc) error {
	return visitor(b, func(err error) error {
		if err != nil {
			return err
		}
		for _, stmt := range b.Statements {
			if err = VisitFunc(stmt, visitor); err != nil {
				return err
			}
		}
		return nil
	})
}

type ForStmt struct {
	Mixin

	Target *Reference `"for" @@`
	Source *Expr      `"in" @@`
	Body   *Block     `@@`
}

func (i ForStmt) accept(visitor VisitorFunc) error {
	return visitor(i, func(err error) error {
		if err != nil {
			return err
		}
		if err = VisitFunc(i.Target, visitor); err != nil {
			return err
		}
		if err = VisitFunc(i.Source, visitor); err != nil {
			return err
		}
		if err = VisitFunc(i.Body, visitor); err != nil {
			return err
		}
		return nil
	})
}

type IfStmt struct {
	Mixin

	Condition *Expr  `"if" @@`
	Main      *Block `@@`
	Else      *Block `( "else" @@ )?`
}

func (i IfStmt) accept(visitor VisitorFunc) error {
	return visitor(i, func(err error) error {
		if err != nil {
			return err
		}
		if err = VisitFunc(i.Condition, visitor); err != nil {
			return err
		}
		if err = VisitFunc(i.Main, visitor); err != nil {
			return err
		}
		if err = VisitFunc(i.Else, visitor); err != nil {
			return err
		}
		return nil
	})
}

type SwitchStmt struct {
	Mixin

	Target *Expr       `"switch" @@ "{"`
	Cases  []*CaseStmt `@@* "}"`
}

func (s SwitchStmt) accept(visitor VisitorFunc) error {
	return visitor(s, func(err error) error {
		if err != nil {
			return err
		}
		if err = VisitFunc(s.Target, visitor); err != nil {
			return err
		}
		for _, c := range s.Cases {
			if err = VisitFunc(c, visitor); err != nil {
				return err
			}
		}
		return nil
	})
}

type CaseStmt struct {
	Mixin

	Default bool        `( @"default"`
	Case    *CaseSelect `  | "case" @@ ) ":"`
	Body    []*Stmt     `( @@ ( ";" @@ )* ";"? )?`
}

func (c CaseStmt) accept(visitor VisitorFunc) error {
	return visitor(c, func(err error) error {
		if err != nil {
			return err
		}
		if err = VisitFunc(c.Case, visitor); err != nil {
			return err
		}
		for _, stmt := range c.Body {
			if err = VisitFunc(stmt, visitor); err != nil {
				return err
			}
		}
		return nil
	})
}

type CaseSelect struct {
	Mixin

	EnumCase *EnumCase `  @@`
	ExprCase *Expr     `| @@`
}

func (c CaseSelect) accept(visitor VisitorFunc) error {
	return visitor(c, func(err error) error {
		if err != nil {
			return err
		}
		if err = VisitFunc(c.EnumCase, visitor); err != nil {
			return err
		}
		return VisitFunc(c.ExprCase, visitor)
	})
}

type EnumCase struct {
	Mixin

	Case string `"." @Ident`
	Var  string `( "(" @Ident ")" )?`
}

func (e EnumCase) accept(visitor VisitorFunc) error {
	return visitor(e, func(err error) error { return err })
}

type ReturnStmt struct {
	Mixin

	Value *Expr `"return" @@?`
}

func (r ReturnStmt) accept(visitor VisitorFunc) error {
	return visitor(r, func(err error) error {
		if err != nil {
			return err
		}
		return VisitFunc(r.Value, visitor)
	})
}

type FuncDecl struct {
	Mixin

	Name       string        `"fn" @Ident "("`
	Parameters []*Parameters `( @@ ( "," @@ )* )? ","? ")"`
	Throws     bool          `@"throws"?`
	Return     *Expr         `( ":" @@ )?`
	Body       *Block        `@@`
}

func (f *FuncDecl) accept(visitor VisitorFunc) error {
	return visitor(f, func(err error) error {
		if err != nil {
			return err
		}
		for _, p := range f.Parameters {
			if err = VisitFunc(p, visitor); err != nil {
				return err
			}
		}
		if err = VisitFunc(f.Return, visitor); err != nil {
			return err
		}
		return VisitFunc(f.Body, visitor)
	})
}

func (f *FuncDecl) decl() {}

func Parse(r io.Reader) (*AST, error) {
	ast := &AST{}
	return ast, parser.Parse(r, ast)
}

func ParseString(s string) (*AST, error) {
	ast := &AST{}
	return ast, parser.ParseString(s, ast)
}
