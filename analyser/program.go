package analyser

import (
	"github.com/alecthomas/langx/parser"
	"github.com/alecthomas/langx/types"
)

type Program struct {
	AST         *parser.AST
	Root        *Scope
	annotations map[parser.Node]types.Reference
}

// Analyse performs semantic analysis on the AST.
func Analyse(ast *parser.AST) (*Program, error) {
	p := &Program{
		AST:         ast,
		Root:        makeScope(builtins, nil),
		annotations: map[parser.Node]types.Reference{},
	}
	a := &analyser{p: p}
	return p, a.checkRoot(p.Root, p.AST)
}

// Associate an AST node with a type reference.
func (p *Program) associate(node parser.Node, ref types.Reference) {
	p.annotations[node] = ref
}

// Reference returns the analysis reference for an AST node (if any).
func (p *Program) Reference(node parser.Node) types.Reference {
	return p.annotations[node]
}

// Type returns the type reference for an AST node (if any).
func (p *Program) Type(node parser.Node) types.Type {
	t, ok := p.annotations[node].(types.Type)
	if ok {
		return t
	}
	return nil
}

// Value returns the value reference for an AST node (if any).
func (p *Program) Value(node parser.Node) *types.Value {
	v, ok := p.annotations[node].(*types.Value)
	if ok {
		return v
	}
	return nil
}
