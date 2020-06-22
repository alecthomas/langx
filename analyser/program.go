package analyser

import (
	"github.com/alecthomas/langx/parser"
	"github.com/alecthomas/langx/types"
)

// Program represents the type analysis of an associated AST.
type Program struct {
	AST      *parser.AST
	Root     *Scope
	resolved map[parser.Node]types.Reference
	actual   map[parser.Node]types.Reference
}

// Analyse performs semantic analysis on the AST.
func Analyse(ast *parser.AST) (*Program, error) {
	p := &Program{
		AST:      ast,
		Root:     makeScope(builtins, nil),
		resolved: map[parser.Node]types.Reference{},
		actual:   map[parser.Node]types.Reference{},
	}
	a := &analyser{p: p}
	return p, a.checkRoot(p.Root, p.AST)
}

// Associate an AST node with a type reference.
func (p *Program) associate(node parser.Node, ref types.Reference) {
	p.resolved[node] = ref
}

func (p *Program) associateConcrete(node parser.Node, ref types.Reference) {
	p.actual[node] = ref
}

// Associate an AST node with a concrete reference.
func (p Program) resolveConcrete(node parser.Node, scope *Scope, sym string) types.Reference {
	ref := scope.Resolve(sym)
	if ref == nil {
		return nil
	}
	p.actual[node] = ref
	return ref
}

// Associate an AST node with a concrete type reference.
func (p Program) resolveConcreteType(node parser.Node, scope *Scope, sym string) types.Type {
	ref := scope.ResolveType(sym)
	if ref == nil {
		return nil
	}
	p.actual[node] = ref
	return ref
}

// Resolved returns the resolved analysis reference for an AST node (if any).
//
// eg. for a function call this will be the result type.
func (p *Program) Resolved(node parser.Node) types.Reference {
	return p.resolved[node]
}

// ResolvedType returns the resolved type reference for an AST node (if any).
func (p *Program) ResolvedType(node parser.Node) types.Type {
	t, _ := p.resolved[node].(types.Type)
	return t
}

// Actual returns the concrete reference for an AST Node.
//
// A concrete reference is the type defined by the underlying symbol as
// resolved in the scope, rather than the resolved type.
func (p *Program) Actual(node parser.Node) types.Reference {
	return p.actual[node]
}

// ActualType returns the concrete type reference for an AST Node.
func (p *Program) ActualType(node parser.Node) types.Type {
	t, _ := p.actual[node].(types.Type)
	return t
}
