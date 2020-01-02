package parser

import (
	"io"
	"strings"

	"github.com/alecthomas/participle"
	"github.com/alecthomas/participle/lexer"
	"github.com/alecthomas/participle/lexer/regex"
)

var (
	lex = lexer.Must(regex.New(`
		comment = //.*|(?s:/\*.*?\*/)
		backslash = \\
		whitespace = [\r\t ]+
	
		Modifier = \b(pub|override)\b
		Keyword = \b(switch|case|if|enum|alias|let|fn|break|continue|for|throws|import|new)\b
		Ident = \b([[:alpha:]_]\w*)\b
		Number = \b(\d+(\.\d+)?)\b
		String = "(\\.|[^"])*"|'[^']*'
		Newline = \n
		Operator = %=|>=|<=|\^=|&&|\|\||==|!=|\+=|-=|\*=|/=|[-=+*/<>%^!]
		Punct = []` + "`" + `~[()@#${}:;?.,]
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
	numberToken   = lex.Symbols()["Number"]
	operatorToken = lex.Symbols()["Operator"]
)

type AST struct {
	Pos lexer.Position

	Declarations []*Decl `@@*`
}

// Decl is a top-level declaration.
type Decl struct {
	Pos lexer.Position

	Modifiers Modifiers `@Modifier*`

	Class  *ClassDecl  `(   @@ ";"?`
	Import *ImportDecl `  | @@ ";"?`
	Enum   *EnumDecl   `  | @@ ";"?`
	Var    *VarDecl    `  | @@ ";"`
	Func   *FuncDecl   `  | @@ ";"? ) `
}

// Modifiers of a field/function.
type Modifiers int

const (
	ModifierPublic Modifiers = 1 << (2 * iota)
	ModifierOverride
)

func (v Modifiers) GoString() string {
	var modifiers []string
	if v&ModifierOverride != 0 {
		modifiers = append(modifiers, "parser.ModifierOverride")
	}
	if v&ModifierPublic != 0 {
		modifiers = append(modifiers, "parser.ModifierPublic")
	}
	return strings.Join(modifiers, "|")
}

func (v *Modifiers) Capture(values []string) error {
	switch values[0] {
	case "pub":
		*v |= ModifierPublic

	case "override":
		*v = ModifierOverride

	default:
		panic("??")
	}
	return nil
}

type ImportDecl struct {
	Pos lexer.Position

	Alias  string `"import" @Ident?`
	Import string `@String`
}

type EnumDecl struct {
	Pos lexer.Position

	Name    *Type         `"enum" @@ "{"`
	Members []*EnumMember `( @@ ( ";" @@ )* ";"? )? "}"`
}

type EnumMember struct {
	Pos lexer.Position

	Modifiers Modifiers `@Modifier*`

	Case   *CaseDecl `(  @@`
	Method *FuncDecl ` | @@ )`
}

type CaseDecl struct {
	Pos lexer.Position

	Case *Type  `"case" @@`
	Var  string `( "(" @Ident ")" )?`
}

type ClassDecl struct {
	Pos lexer.Position

	Name    *Type          `"class" @@ "{"`
	Members []*ClassMember `( @@ ( ";" @@ )* ";"? )? "}"`
}

type ClassMember struct {
	Pos lexer.Position

	Modifiers Modifiers `@Modifier*`

	Field  *VarDecl  `(  @@`
	Method *FuncDecl ` | @@ )`
}

type Type struct {
	Pos lexer.Position

	Type     string          `@Ident`
	Generics []*GenericParam `( "<" @@ ( "," @@ )* ","? ">" )?`
}

type GenericParam struct {
	Pos lexer.Position

	Name        string  `@Ident`
	Constraints []*Type `( ":" @@ ( "," @@ )* )?`
}

type Parameters struct {
	Pos lexer.Position

	Names []string  `@Ident ("," @Ident)*`
	Type  *Terminal `@@`
}

type VarDecl struct {
	Pos lexer.Position

	Names   []string  `"let" @Ident ("," @Ident)*`
	Type    *Terminal `@@?`
	Default *Expr     `( "=" @@ )?`
}

type Stmt struct {
	Pos lexer.Position

	Return    *ReturnStmt `  @@`
	Go        *GoStmt     `| @@`
	If        *IfStmt     `| @@`
	Switch    *SwitchStmt `| @@`
	Block     *Block      `| @@`
	VarDecl   *VarDecl    `| @@`
	FuncDecl  *FuncDecl   `| @@`
	ClassDecl *ClassDecl  `| @@`
	EnumDecl  *EnumDecl   `| @@`

	// Must be last alternative.
	Expression *Expr `| @@`
}

type Block struct {
	Pos lexer.Position

	Statements []*Stmt `"{" ( @@ ( ";" @@ )* )? ";"? "}"`
}

type GoStmt struct {
	Pos lexer.Position

	Call *Terminal `"go" @@`
}

type IfStmt struct {
	Pos lexer.Position

	Expression *Expr  `"if" @@`
	Main       *Block `@@`
	Else       *Block `( "else" @@ )?`
}

type SwitchStmt struct {
	Pos lexer.Position

	Target *Expr       `"switch" @@ "{"`
	Cases  []*CaseStmt `@@* "}"`
}

type CaseStmt struct {
	Pos lexer.Position

	Case    *Expr   `( "case" @@`
	Default bool    `  | @"default" ) ":"`
	Body    []*Stmt `( @@ ( ";" @@ )* ";"? )?`
}

type ReturnStmt struct {
	Pos lexer.Position

	Value *Expr `"return" @@?`
}

type FuncDecl struct {
	Pos lexer.Position

	Name       string        `"fn" @Ident "("`
	Parameters []*Parameters `( @@ ( "," @@ )* )? ","? ")"`
	Throws     bool          `@"throws"?`
	Return     *Terminal     `@@?`
	Body       *Block        `@@`
}

func Parse(r io.Reader) (*AST, error) {
	ast := &AST{}
	return ast, parser.Parse(r, ast)
}

func ParseString(s string) (*AST, error) {
	ast := &AST{}
	return ast, parser.ParseString(s, ast)
}
