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
	symbols  map[string]types.Type
}

func makeScopeFromType(t types.Type) *Scope {
	s := makeScope(nil, t)
	return s
}

func makeScope(parent *Scope, owner types.Type) *Scope {
	return &Scope{
		owner:   owner,
		parent:  parent,
		symbols: map[string]types.Type{},
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

func (s *Scope) Children() []*Scope             { return s.children }
func (s *Scope) Symbols() map[string]types.Type { return s.symbols }
func (s *Scope) Resolve(ident string) types.Type {
	if sym, ok := s.symbols[ident]; ok {
		return sym
	}
	if s.parent != nil {
		return s.parent.Resolve(ident)
	}
	return nil
}
func (s *Scope) Add(name string, symbol types.Type) error {
	_, ok := s.symbols[name]
	if ok {
		return errors.Errorf("%q redeclared", name)
	}
	s.symbols[name] = symbol
	return nil
}
