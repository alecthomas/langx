package codegen

import (
	"fmt"
	"io"
	"strings"

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
	typ  types.Type   // types.Enum, types.Class
	body parser.Decls // *parser.EnumDecl, *parser.ClassDecl, etc.
}

type generator struct {
	io.Writer
	generics map[string]generic
	p        *analyser.Program
}

func (g *generator) VisitArrayTypeDecl(n *parser.ArrayTypeDecl) error {
	panic("implement me")
}

func (g *generator) VisitDictOrSetTypeDecl(n *parser.DictOrSetTypeDecl) error {
	panic("implement me")
}

func (g *generator) VisitNamedTypeDecl(n *parser.NamedTypeDecl) error {
	fmt.Fprintf(g, "%s", n.Type)
	if len(n.TypeParameter) > 0 {
		panic("generics not supported")
	}
	return parser.TerminateRecursion
}

func (g *generator) VisitAST(n *parser.AST) error { return nil }

func (g *generator) VisitArrayLiteral(n parser.ArrayLiteral) error {
	panic("implement me")
}

func (g *generator) VisitBlock(n parser.Block) error {
	fmt.Fprintf(g, "{\n")
	for _, stmt := range n.Statements {
		if err := parser.Visit(*stmt, g); err != nil {
			return err
		}
	}
	fmt.Fprintf(g, "}\n")
	return parser.TerminateRecursion
}

func (g *generator) VisitCall(n parser.Call) error {
	panic("implement me")
}

func (g *generator) VisitCaseDecl(n *parser.CaseDecl) error {
	fmt.Fprintf(g, "  %s ", n.Name)
	defer fmt.Fprintln(g)
	if n.Type != nil {
		fmt.Fprint(g, "*")
		return parser.Visit(*n.Type, g)
	}
	fmt.Fprint(g, "bool")
	return parser.TerminateRecursion
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

func (g *generator) VisitDecl(n parser.Decl) error { return nil }

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
	if n.Type.TypeParameter != nil {
		panic(n.Pos.String() + ": generics not supported")
	}
	fmt.Fprintf(g, "type %s struct {\n", n.Type.Type)
	for _, member := range n.Members {
		if err := parser.Visit(member, g); err != nil {
			return err
		}
	}
	fmt.Fprintln(g, "}")
	return parser.TerminateRecursion
}

func (g *generator) VisitEnumMember(n *parser.EnumMember) error {
	return nil
}

func (g *generator) VisitExpr(n *parser.Expr) error { return nil }

func (g *generator) VisitForStmt(n parser.ForStmt) error {
	panic("implement me")
}

func (g *generator) VisitFuncDecl(n *parser.FuncDecl) error {
	fmt.Fprintf(g, "func %s(", n.Name)
	fmt.Fprintf(g, ") ")
	err := parser.Visit(n.Return, g)
	if err != nil {
		return err
	}
	err = parser.Visit(n.Body, g)
	if err != nil {
		return err
	}
	return parser.TerminateRecursion
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
	switch {
	case n.Number != nil:
		fmt.Fprintf(g, "%s", n.Number)
		return parser.TerminateRecursion

	case n.Str != nil:
		fmt.Fprintf(g, "%q", *n.Str)
		return parser.TerminateRecursion

	case n.Bool != nil:
		fmt.Fprintf(g, "%v", *n.Bool)
		return parser.TerminateRecursion

	default:
		panic("implement me")
	}
}

func (g *generator) VisitParameters(n parser.Parameters) error {
	panic("implement me")
}

func (g *generator) VisitReference(n *parser.Reference) error {
	ref := g.p.Reference(n)
	switch ref.(type) {
	case *types.Value:
		return nil

	case *types.Enum:
		fmt.Fprint(g, "&")
		if err := parser.Visit(*n.Terminal, g); err != nil {
			return err
		}
		fmt.Fprint(g, "{")
		if err := parser.Visit(n.Next, g); err != nil {
			return err
		}
		fmt.Fprint(g, "}")
		return parser.TerminateRecursion

	case types.Builtin:
		return nil

	case nil:
		return nil

	default:
		panic(fmt.Sprintf("%T", ref))
	}
}

func (g *generator) VisitReferenceNext(n *parser.ReferenceNext) error {
	switch {
	case n.Reference != nil:
		fmt.Fprint(g, ".")
		return parser.Visit(*n.Reference, g)

	default:
		panic("??")
	}
}

func (g *generator) VisitReturnStmt(n parser.ReturnStmt) error {
	panic("implement me")
}

func (g *generator) VisitRootDecl(n *parser.RootDecl) error { return nil }

func (g *generator) VisitStmt(n parser.Stmt) error {
	panic("implement me")
}

func (g *generator) VisitSwitchStmt(n parser.SwitchStmt) error {
	panic("implement me")
}

func (g *generator) VisitTerminal(n parser.Terminal) error {
	switch {
	case n.Ident != "":
		fmt.Fprint(g, n.Ident)
		return parser.TerminateRecursion

	default:
		return nil
	}
}

func (g *generator) VisitTypeDecl(n parser.TypeDecl) error {
	switch {
	case n.Named != nil:
		return g.VisitNamedTypeDecl(n.Named)
	default:
		panic("??")
	}
	return parser.TerminateRecursion
}

func (g *generator) VisitTypeParamDecl(n parser.TypeParamDecl) error {
	panic("implement me")
}

func (g *generator) VisitUnary(n *parser.Unary) error {
	fmt.Fprintf(g, "%s", n.Op)
	return parser.Visit(n.Reference, g)
}

func (g *generator) VisitVarDecl(n *parser.VarDecl) error {
	fmt.Fprintln(g, "var (")
	for _, v := range n.Vars {
		fmt.Fprintf(g, "  %s ", v.Name)
		if err := g.generateTypeReference(v); err != nil {
			return err
		}
		if v.Default != nil {
			fmt.Fprint(g, " = ")
			if err := parser.Visit(v.Default, g); err != nil {
				return err
			}
		}
		fmt.Fprintln(g)
	}
	fmt.Fprintln(g, ")")
	return parser.TerminateRecursion
}

func (g *generator) VisitVarDeclAsgn(n parser.VarDeclAsgn) error {
	panic("implement me")
}

func (g *generator) generateCaseValue(next *parser.ReferenceNext) error {
	if err := parser.Visit(*next.Reference, g); err != nil {
		return err
	}
	fmt.Fprint(g, ": ")
	if next.Next != nil {
		ref := g.p.Reference(next.Reference).(*types.Case)
		if ref.Case.Kind().IsScalar() {
			fmt.Fprintf(g, "runtime.%s(", strings.Title(ref.Case.String()))
			defer fmt.Fprint(g, ")")
		}
		return parser.Visit(next.Next.Call.Parameters[0], g)
	}
	fmt.Fprint(g, "true")
	return parser.TerminateRecursion
}

func (g *generator) generateTypeReference(node parser.Node) error {
	ref := g.p.Reference(node).Type()
	if ref == nil {
		return nil
	}
	switch ref := ref.(type) {
	case *types.Case:
		fmt.Fprintf(g, "%s", ref.Name)

	case types.Builtin:
		fmt.Fprintf(g, "%s", ref.Name())

	default:
		panic(fmt.Sprintf("%T", ref))
	}
	return nil
}
