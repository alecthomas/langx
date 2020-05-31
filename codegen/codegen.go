package codegen

import (
	"io"

	"github.com/alecthomas/langx/analyser"
	"github.com/alecthomas/langx/parser"
	"github.com/alecthomas/langx/types"
)

func Generate(w io.Writer, program *analyser.Program) error {
	g := &generator{
		Writer: w,
		p:      program,
		generics: map[string]generic{
			"Option": {
				name: "Option",
				typ:  types.Optional(types.Generic{}),
				body: &parser.EnumDecl{
					Type: &parser.NamedTypeDecl{
						Type: "Option",
						TypeParameter: []*parser.TypeParamDecl{
							{Name: "T"},
						},
					},
					Members: []*parser.EnumMember{
						{CaseDecl: &parser.CaseDecl{Name: "None"}},
						{CaseDecl: &parser.CaseDecl{Name: "Some", Type: &parser.TypeDecl{
							Named: &parser.NamedTypeDecl{Type: "T"}}}},
					},
				},
			},
		},
	}
	return parser.Visit(program.AST, g)
}

type generic struct {
	name string
	typ  types.Type   // types.Enum, types.ClassType
	body parser.Decls // *parser.EnumDecl, *parser.ClassDecl, etc.
}

type generator struct {
	io.Writer
	generics map[string]generic
	p        *analyser.Program
}

func (g *generator) VisitAST(n *parser.AST) error {
	panic("implement me")
}

func (g *generator) VisitExprStmt(n *parser.ExprStmt) error {
	panic("implement me")
}

func (g *generator) VisitArrayLiteral(n parser.ArrayLiteral) error {
	panic("implement me")
}

func (g *generator) VisitBlock(n parser.Block) error {
	panic("implement me")
}

func (g *generator) VisitCall(n parser.Call) error {
	panic("implement me")
}

func (g *generator) VisitCaseDecl(n *parser.CaseDecl) error {
	panic("implement me")
}

func (g *generator) VisitCaseSelect(n parser.CaseSelect) error {
	panic("implement me")
}

func (g *generator) VisitCaseStmt(n parser.CaseStmt) error {
	panic("implement me")
}

func (g *generator) VisitClassDecl(n *parser.ClassDecl) error {
	panic("implement me")
}

func (g *generator) VisitClassMember(n *parser.ClassMember) error {
	panic("implement me")
}

func (g *generator) VisitDictOrSetEntryLiteral(n parser.DictOrSetEntryLiteral) error {
	panic("implement me")
}

func (g *generator) VisitDictOrSetLiteral(n parser.DictOrSetLiteral) error {
	panic("implement me")
}

func (g *generator) VisitEnumCase(n parser.EnumCase) error {
	panic("implement me")
}

func (g *generator) VisitEnumDecl(n *parser.EnumDecl) error {
	panic("implement me")
}

func (g *generator) VisitEnumMember(n *parser.EnumMember) error {
	panic("implement me")
}

func (g *generator) VisitExpr(n *parser.Expr) error {
	panic("implement me")
}

func (g *generator) VisitNew(n *parser.NewExpr) error {
	panic("implement me")
}

func (g *generator) VisitForStmt(n parser.ForStmt) error {
	panic("implement me")
}

func (g *generator) VisitFuncDecl(n *parser.FuncDecl) error {
	panic("implement me")
}

func (g *generator) VisitIfStmt(n parser.IfStmt) error {
	panic("implement me")
}

func (g *generator) VisitImportDecl(n *parser.ImportDecl) error {
	panic("implement me")
}

func (g *generator) VisitInitialiserDecl(n *parser.InitialiserDecl) error {
	panic("implement me")
}

func (g *generator) VisitLiteral(n *parser.Literal) error {
	panic("implement me")
}

func (g *generator) VisitParameters(n parser.Parameters) error {
	panic("implement me")
}

func (g *generator) VisitReference(n *parser.Reference) error {
	panic("implement me")
}

func (g *generator) VisitReferenceNext(n *parser.ReferenceNext) error {
	panic("implement me")
}

func (g *generator) VisitReturnStmt(n parser.ReturnStmt) error {
	panic("implement me")
}

func (g *generator) VisitRootDecl(n *parser.RootDecl) error {
	panic("implement me")
}

func (g *generator) VisitStmt(n parser.Stmt) error {
	panic("implement me")
}

func (g *generator) VisitSwitchStmt(n parser.SwitchStmt) error {
	panic("implement me")
}

func (g *generator) VisitTerminal(n parser.Terminal) error {
	panic("implement me")
}

func (g *generator) VisitTypeDecl(n parser.TypeDecl) error {
	panic("implement me")
}

func (g *generator) VisitArrayTypeDecl(n *parser.ArrayTypeDecl) error {
	panic("implement me")
}

func (g *generator) VisitDictOrSetTypeDecl(n *parser.DictOrSetTypeDecl) error {
	panic("implement me")
}

func (g *generator) VisitNamedTypeDecl(n *parser.NamedTypeDecl) error {
	panic("implement me")
}

func (g *generator) VisitTypeParamDecl(n parser.TypeParamDecl) error {
	panic("implement me")
}

func (g *generator) VisitUnary(n *parser.Unary) error {
	panic("implement me")
}

func (g *generator) VisitVarDecl(n *parser.VarDecl) error {
	panic("implement me")
}

func (g *generator) VisitVarDeclAsgn(n parser.VarDeclAsgn) error {
	panic("implement me")
}
