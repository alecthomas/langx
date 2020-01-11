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
	
		Modifier = \b(pub|override|static)\b
		Keyword = \b(in|switch|case|default|if|enum|alias|let|fn|break|continue|for|throws|import|new)\b
		Ident = \b([[:alpha:]_]\w*)\b
		Number = \b(\d+(\.\d+)?)\b
		String = "(\\.|[^"])*"|'[^']*'
		Newline = \n
		Operator = ->|%=|>=|<=|\^=|&&|\|\||==|!=|\+=|-=|\*=|/=|[-=+*/<>%^!]
		Punct = []` + "`" + `~[()@#${}:;?.,]
	`))
	parser = participle.MustBuild(&AST{},
		participle.Lexer(&fixupLexerDefinition{}),
		participle.UseLookahead(1),
		participle.Unquote(),
	)
	unaryParser = participle.MustBuild(&Unary{},
		participle.Lexer(&fixupLexerDefinition{}),
		participle.UseLookahead(1),
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

type ImportDecl struct {
	Pos lexer.Position

	Alias  string `"import" @Ident?`
	Import string `@String`
}

type EnumDecl struct {
	Pos lexer.Position

	Type    *TypeDecl     `"enum" @@ "{"`
	Members []*EnumMember `( @@ ( ";" @@ )* ";"? )? "}"`
}

type EnumMember struct {
	Pos lexer.Position

	Modifiers Modifiers `@Modifier*`

	CaseDecl        *CaseDecl        `(  @@`
	VarDecl         *VarDecl         ` | @@`
	FuncDecl        *FuncDecl        ` | @@`
	ClassDecl       *ClassDecl       ` | @@`
	EnumDecl        *EnumDecl        ` | @@`
	InitialiserDecl *InitialiserDecl ` | @@ )`
}

type CaseDecl struct {
	Pos lexer.Position

	Name string    `"case" @Ident`
	Type *TypeDecl `( "(" @@ ")" )?`
}

type ClassDecl struct {
	Pos lexer.Position

	Type    *TypeDecl      `"class" @@ "{"`
	Members []*ClassMember `( @@ ( ";" @@ )* ";"? )? "}"`
}

type ClassMember struct {
	Pos lexer.Position

	Modifiers Modifiers `@Modifier*`

	VarDecl         *VarDecl         `(  @@`
	FuncDecl        *FuncDecl        ` | @@`
	ClassDecl       *ClassDecl       ` | @@`
	EnumDecl        *EnumDecl        ` | @@`
	InitialiserDecl *InitialiserDecl ` | @@ )`
}

type InitialiserDecl struct {
	Pos lexer.Position

	Parameters []*Parameters `"init" "(" ( @@ ( "," @@ )* )? ","? ")"`
	Throws     bool          `@"throws"?`
	Body       *Block        `@@`
}

type TypeDecl struct {
	Pos lexer.Position

	Type          string           `@Ident`
	TypeParameter []*TypeParamDecl `( "<" @@ ( "," @@ )* ","? ">" )?`
}

type TypeParamDecl struct {
	Pos lexer.Position

	Name        string       `@Ident`
	Constraints []*Reference `( ":" @@ ( "," @@ )* )?`
}

type Parameters struct {
	Pos lexer.Position

	Names []string   `@Ident ("," @Ident)*`
	Type  *Reference `":" @@`
}

type VarDecl struct {
	Pos lexer.Position

	// let a, b, c int
	// let a = 1, b = 2
	// let a int = 1, b int = 2
	Vars []*VarDeclAsgn `"let" @@ ( "," @@ )*`
}

type VarDeclAsgn struct {
	Pos lexer.Position

	Name    string     `@Ident`
	Type    *Reference `( ":" @@ )?`
	Default *Expr      `( "=" @@ )?`
}

type Stmt struct {
	Pos lexer.Position

	Return    *ReturnStmt `  @@`
	Go        *GoStmt     `| @@`
	If        *IfStmt     `| @@`
	For       *ForStmt    `| @@`
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

	Statements []*Stmt `"{" ( @@ ( ";" @@ )* ";"? )? "}"`
}

type GoStmt struct {
	Pos lexer.Position

	Call *Reference `"go" @@`
}

type ForStmt struct {
	Pos lexer.Position

	Target *Reference `"for" @@`
	Source *Expr      `"in" @@`
	Body   *Block     `@@`
}

type IfStmt struct {
	Pos lexer.Position

	Condition *Expr  `"if" @@`
	Main      *Block `@@`
	Else      *Block `( "else" @@ )?`
}

type SwitchStmt struct {
	Pos lexer.Position

	Target *Expr       `"switch" @@ "{"`
	Cases  []*CaseStmt `@@* "}"`
}

type CaseStmt struct {
	Pos lexer.Position

	Default bool        `( @"default"`
	Case    *CaseSelect `  | "case" @@ ) ":"`
	Body    []*Stmt     `( @@ ( ";" @@ )* ";"? )?`
}

type CaseSelect struct {
	Pos lexer.Position

	EnumCase *EnumCase `  @@`
	ExprCase *Expr     `| @@`
}

type EnumCase struct {
	Pos lexer.Position

	Case string `"." @Ident`
	Var  string `( "(" @Ident ")" )?`
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
	Return     *Reference    `( ":" @@ )?`
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
