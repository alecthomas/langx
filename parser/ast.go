package parser

import (
	"io"

	"github.com/alecthomas/participle"
	"github.com/alecthomas/participle/lexer"
	"github.com/alecthomas/participle/lexer/regex"
)

var (
	lex = lexer.Must(regex.New(`
		comment = //.*|(?s:/\*.*?\*/)
		Keyword = \b(switch|case|if|enum|alias|let|fn|break|continue|for|throws|import|new)\b
		Ident = \b([[:alpha:]_]\w*)\b
		Float = \b(\d*\.\d+)\b
		Int = \b(\d+)\b
		String = "(\\.|[^"])*"|'[^']*'
		Newline = \n
		backslash = \\
		Punct = []` + "`" + `~[()!@#$%^&*{}:;?><,./=+-]
		whitespace = [\r\t ]+
	`))
	parser = participle.MustBuild(&AST{},
		participle.Lexer(&fixupLexerDefinition{}),
		participle.Unquote(),
	)

	identToken  = lex.Symbols()["Ident"]
	stringToken = lex.Symbols()["String"]
	intToken    = lex.Symbols()["Int"]
	floatToken  = lex.Symbols()["Float"]
)

type AST struct {
	Declarations []*Decl `@@*`
}

// Decl is a top-level declaration.
type Decl struct {
	Visibility Visibility `@@?`

	Struct *StructDecl `(   @@ ";"?`
	Import *ImportDecl `  | @@ ";"?`
	Enum   *EnumDecl   `  | @@ ";"?`
	Alias  *AliasDecl  `  | @@ ";"?`
	Var    *LetStmt    `  | @@ ";"`
	Func   *FuncDecl   `  | @@ ";"? ) `
}

// Visibility of a field.
type Visibility int

const (
	VisibilityPrivate Visibility = iota
	VisibilityPublic
)

func (v Visibility) GoString() string {
	switch v {
	case VisibilityPrivate:
		return "parser.VisibilityPrivate"
	case VisibilityPublic:
		return "parser.VisibilityPublic"
	default:
		panic(v)
	}
}

func (v *Visibility) Parse(lex lexer.PeekingLexer) error {
	token, err := lex.Peek(0)
	if err != nil {
		return err
	}
	switch token.Value {
	case "public":
		*v = VisibilityPublic

	case "private":
		*v = VisibilityPrivate

	default:
		return participle.NextMatch
	}

	_, _ = lex.Next()
	return nil
}

type ImportDecl struct {
	Alias  string `"import" @Ident?`
	Import string `@String`
}

type AliasDecl struct {
	Name  *Type `"alias" @@`
	Alias *Type `@@`
}

type EnumDecl struct {
	Name    *Type         `"enum" @@ "{"`
	Members []*EnumMember `( @@ ( ";" @@ )* ";"? )? "}"`
}

type EnumMember struct {
	Visibility Visibility `@@?`

	Case   *CaseDecl `(  @@`
	Method *FuncDecl ` | @@ )`
}

type CaseDecl struct {
	Case *Type  `"case" @@`
	Var  string `( "(" @Ident ")" )?`
}

type StructDecl struct {
	Name    *Type           `"struct" @@ "{"`
	Members []*StructMember `( @@ ( ";" @@ )* ";"? )? "}"`
}

type StructMember struct {
	Visibility Visibility `@@?`

	Field  *LetStmt  `(  @@`
	Method *FuncDecl ` | @@ )`
}

type Type struct {
	Type     string          `@Ident`
	Generics []*GenericParam `( "<" @@ ( "," @@ )* ","? ">" )?`
}

type GenericParam struct {
	Name        string  `@Ident`
	Constraints []*Type `( ":" @@ ( "," @@ )* )?`
}

type VarDecl struct {
	Names []string `@Ident ("," @Ident)*`
	Type  *Type    `@@`
}

type LetStmt struct {
	Names   []string `"let" @Ident ("," @Ident)*`
	Type    *Type    `@@?`
	Default *Expr    `( "=" @@ )?`
}

type Stmt struct {
	Return *ReturnStmt `  @@`
	Let    *LetStmt    `| @@`
	Block  *Block      `| @@`
	Func   *FuncDecl   `| @@`
	If     *IfStmt     `| @@`
	Switch *SwitchStmt `| @@`

	// Must be last alternative.
	Expression *Expr `| @@`
}

type Block struct {
	Statements []*Stmt `"{" ( @@ ( ";" @@ )* )? ";"? "}"`
}

type IfStmt struct {
	Expression *Expr  `"if" @@`
	Main       *Block `@@`
	Else       *Block `( "else" @@ )?`
}

type SwitchStmt struct {
	Target *Expr       `"switch" @@ "{"`
	Cases  []*CaseStmt `@@* "}"`
}

type CaseStmt struct {
	Case    *Expr   `( "case" @@`
	Default bool    `  | @"default" ) ":"`
	Body    []*Stmt `( @@ ( ";" @@ )* ";"? )?`
}

type ReturnStmt struct {
	Value *Expr `"return" @@`
}

type FuncDecl struct {
	Name       string     `"fn" @Ident "("`
	Parameters []*VarDecl `( @@ ( "," @@ )* )? ","? ")"`
	Throws     bool       `@"throws"?`
	Return     *Type      `@@?`
	Body       *Block     `@@`
}

type Expr struct {
	Comparison *Comparison `@@`
	Op         string      `[ @( "!" "=" | "=" "=" )`
	Next       *Expr       `  @@ ]`
}

type Comparison struct {
	Addition *Addition   `@@`
	Op       string      `[ @( ">" | ">" "=" | "<" | "<" "=" )`
	Next     *Comparison `  @@ ]`
}

type Addition struct {
	Multiplication *Multiplication `@@`
	Op             string          `[ @( "-" | "+" )`
	Next           *Addition       `  @@ ]`
}

type Multiplication struct {
	Assignment *Assignment     `@@`
	Op         string          `[ @( "/" | "*" )`
	Next       *Multiplication `  @@ ]`
}

type Assignment struct {
	Unary *Unary `@@`
	Op    string `[ @( ( "+" | "-" | "*" | "/" | "%" )? "=" )`
	Next  *Expr  `  @@ ]`
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

func Parse(r io.Reader) (*AST, error) {
	ast := &AST{}
	return ast, parser.Parse(r, ast)
}

func ParseString(s string) (*AST, error) {
	ast := &AST{}
	return ast, parser.ParseString(s, ast)
}
