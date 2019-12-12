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
		backslash = \\
		whitespace = [\r\t ]+
	
		Keyword = \b(switch|case|if|enum|alias|let|fn|break|continue|for|throws|import|new)\b
		Ident = \b([[:alpha:]_]\w*)\b
		Float = \b(\d*\.\d+)\b
		Int = \b(\d+)\b
		String = "(\\.|[^"])*"|'[^']*'
		Newline = \n
		Operator = %=|>=|<=|&&|\|\||==|!=|\+=|-=|\*=|/=|[-=+*/<>%]
		Punct = []` + "`" + `~[()!@#${}:;?,.]
	`))
	parser = participle.MustBuild(&AST{},
		participle.Lexer(&fixupLexerDefinition{}),
		participle.Unquote(),
	)
	unaryParser = participle.MustBuild(&Unary{},
		participle.Lexer(&fixupLexerDefinition{}),
		participle.Unquote(),
	)

	identToken    = lex.Symbols()["Ident"]
	stringToken   = lex.Symbols()["String"]
	intToken      = lex.Symbols()["Int"]
	floatToken    = lex.Symbols()["Float"]
	operatorToken = lex.Symbols()["Operator"]
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

func (v *Visibility) Parse(lex *lexer.PeekingLexer) error {
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

func Parse(r io.Reader) (*AST, error) {
	ast := &AST{}
	return ast, parser.Parse(r, ast)
}

func ParseString(s string) (*AST, error) {
	ast := &AST{}
	return ast, parser.ParseString(s, ast)
}
