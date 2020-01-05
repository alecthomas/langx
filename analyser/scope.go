package analyser

import (
	"github.com/pkg/errors"

	"github.com/alecthomas/langx/types"
)

// Scope resolves symbols.
type Scope struct {
	parent   *Scope
	owner    types.Type
	children []*Scope
	symbols  map[string]types.Reference
}

func makeScope(parent *Scope, owner types.Type) *Scope {
	return &Scope{
		owner:   owner,
		parent:  parent,
		symbols: map[string]types.Reference{},
	}
}

// Owner returns the owner of this scope (if any).
//
// This is typically used with functions.
func (s *Scope) Owner() types.Type {
	return s.owner
}

func (s *Scope) Parent() *Scope { return s.parent }

// Sub creates a new sub-scope.
func (s *Scope) Sub(owner types.Type) *Scope {
	scope := makeScope(s, owner)
	s.children = append(s.children, scope)
	return scope
}

func (s *Scope) Children() []*Scope { return s.children }
func (s *Scope) Resolve(ident string) types.Reference {
	if ref, ok := s.symbols[ident]; ok {
		return ref
	}
	if s.parent != nil {
		return s.parent.Resolve(ident)
	}
	return nil
}
func (s *Scope) Symbols() map[string]types.Reference { return s.symbols }
func (s *Scope) ResolveType(ident string) types.Type {
	if sym, ok := s.symbols[ident].(types.Type); ok {
		return sym
	}
	if s.parent != nil {
		return s.parent.ResolveType(ident)
	}
	return nil
}
func (s *Scope) ResolveValue(ident string) *types.Value {
	if sym, ok := s.symbols[ident].(*types.Value); ok {
		return sym
	}
	if s.parent != nil {
		return s.parent.ResolveValue(ident)
	}
	return nil
}
func (s *Scope) AddType(name string, symbol types.Type) error {
	_, ok := s.symbols[name]
	if ok {
		return errors.Errorf("%q redeclared", name)
	}
	s.symbols[name] = symbol
	return nil
}
func (s *Scope) AddValue(name string, value *types.Value) error {
	_, ok := s.symbols[name]
	if ok {
		return errors.Errorf("%q redeclared", name)
	}
	s.symbols[name] = value
	return nil
}
