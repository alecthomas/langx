package codegen

import (
	"fmt"
	"io"
	"math/big"

	"github.com/alecthomas/langx/analyser"
	. "github.com/alecthomas/langx/codegen/wat"
	"github.com/alecthomas/langx/parser"
	"github.com/alecthomas/langx/types"
)

func Generate(w io.Writer, program *analyser.Program) error {
	g := &generator{
		program:   program,
		constants: map[*parser.Literal]int{},
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
	sexpr, err := g.genProgram(program.AST)
	if err != nil {
		return err
	}
	return Write(w, sexpr)
}

type generic struct {
	name string
	typ  types.Type   // types.Enum, types.ClassType
	body parser.Decls // *parser.EnumDecl, *parser.ClassDecl, etc.
}

type generator struct {
	generics  map[string]generic
	constants map[*parser.Literal]int
	state     []string
	scope     *analyser.Scope
	program   *analyser.Program
}

func (g *generator) genProgram(p *parser.AST) (Node, error) {
	root := List{
		ID("module"),
	}
	_ = parser.VisitFunc(p, g.buildDataTable(&root))
	for _, decl := range p.Decls() {
		sdecl, err := g.genDecl(nil, decl)
		if err != nil {
			return nil, err
		}
		if sdecl != nil {
			root.Add(sdecl)
		}
	}
	return root, nil
}

// Collect all constant strings into global tables.
func (g *generator) buildDataTable(root *List) parser.VisitorFunc {
	consts := 0
	return func(node parser.Node, next parser.Next) error {
		lit, ok := node.(*parser.Literal)
		if !ok {
			return next(nil)
		}
		var str *string
		if lit.Str != nil {
			str = lit.Str
		} else if lit.LitStr != nil {
			str = lit.LitStr
		} else {
			return next(nil)
		}
		root.Add(List{
			ID("data"),
			List{ID("i32.const"), Int(consts)},
			String(*str),
		})
		g.constants[lit] = consts
		consts += len(*str)
		return next(nil)
	}
}

func (g *generator) genDecl(parent *types.NamedType, decl parser.Decl) (Node, error) {
	prefix := ""
	if parent != nil {
		prefix = parent.Name() + "."
	}
	switch decl := decl.(type) {
	case *parser.FuncDecl:
		fs := List{
			ID("func"),
			Var(prefix + decl.Name),
		}
		for _, p := range decl.Parameters {
			ps, err := g.genParams(p)
			if err != nil {
				return nil, err
			}
			for _, p := range ps {
				fs.Add(p)
			}
		}
		fs.Add(List{
			ID("result"),
			g.typeForExpr(decl.Return),
		})
		fs = append(fs, g.genBlock(decl.Body)...)
		return fs, nil

	case *parser.VarDecl:
		return nil, nil

	case *parser.ClassDecl:
		typ := g.program.Type(decl)
		if typ == nil {
			panic(fmt.Sprintf("%T", decl))
		}
		named := &types.NamedType{
			Nme: decl.Type.Type,
			Typ: typ,
		}
		for _, field := range decl.Decls() {
			_, err := g.genDecl(named, field)
			if err != nil {
				return nil, err
			}
		}
		return nil, nil

	default:
		panic(fmt.Sprintf("%T", decl))
	}
}

func (g *generator) genParams(p *parser.Parameters) (List, error) {
	ps := List{}
	for _, name := range p.Names {
		ps.Add(List{
			ID("param"),
			Var(name),
			g.typeForRef(p.Type),
		})
	}
	return ps, nil
}

func (g *generator) typeForExpr(e *parser.Expr) Node {
	if e.Unary != nil {
		return g.typeForUnary(e.Unary)
	}
	panic(e.String())
}

func (g *generator) typeForUnary(unary *parser.Unary) Node {
	if unary.Op != 0 {
		panic(unary.String())
	}
	return g.typeForRef(unary.Reference)
}

func (g *generator) typeForRef(r *parser.Reference) Node {
	return g.typeForTerm(r.Terminal)
}

func (g *generator) typeForTerm(t *parser.Terminal) Node {
	switch {
	case t.Ident != "":
		switch t.Ident {
		case "int":
			return ID("i64")

		case "float":
			return ID("f64")

		default:
			panic(t.Describe())
		}
	default:
		panic(t.Describe())
	}
}

func (g *generator) genBlock(body *parser.Block) List {
	out := List{}
	for _, stmt := range body.Statements {
		out.Add(g.genStmt(stmt)...)
	}
	return out
}

func (g *generator) genStmt(stmt *parser.Stmt) List {
	switch {
	case stmt.Return != nil:
		return g.genExpr(stmt.Return.Value)
	default:
		panic(fmt.Sprintf("%T", stmt))
	}
}

func (g *generator) genExpr(value *parser.Expr) List {
	ref := g.program.Reference(value)
	if ref == nil {
		panic("??")
	}
	out := List{}
	if value.Left != nil {
		out.Add(g.genExpr(value.Left)...)
	}
	if value.Right != nil {
		out.Add(g.genExpr(value.Right)...)
	}
	if value.Unary != nil {
		out.Add(g.genUnary(value.Unary)...)
	}
	typ := g.typeRef(ref)
	switch value.Op {
	case 0:
	case parser.OpAdd:
		out.Add(ID(typ + ".add"))
	}
	return out
}

func (g *generator) genUnary(unary *parser.Unary) List {
	out := List{}
	out.Add(g.genRef(unary.Reference)...)
	return out
}

func (g *generator) genRef(ref *parser.Reference) List {
	out := List{}
	out.Add(g.genTerminal(ref.Terminal)...)
	return out
}

func (g *generator) genTerminal(terminal *parser.Terminal) List {
	switch {
	case terminal.Ident != "":
		return List{
			ID("local.get"),
			Var(terminal.Ident),
		}

	case terminal.Literal != nil:
		return g.genLiteral(terminal.Literal)

	default:
		panic(terminal.Describe())
	}
}

func (g *generator) genLiteral(literal *parser.Literal) List {
	switch {
	case literal.Number != nil:
		n, _ := (*big.Float)(literal.Number).Int64()
		return List{
			ID("i64.const"),
			Int(n),
		}

	default:
		panic(literal.Describe())
	}
}

func (g *generator) typeRef(ref types.Reference) string {
	switch ref.Kind() {
	case types.KindInt:
		return "i64"
	case types.KindFloat:
		return "f64"
	case types.KindString, types.KindClass:
		return "i32"
	default:
		panic(ref.Kind().String())
	}
}
