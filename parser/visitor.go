package parser

import (
	"reflect"

	"github.com/alecthomas/participle/v2/lexer"
)

// go-sumtype:decl Node

type Mixin struct {
	Pos lexer.Position
}

func (p Mixin) Position() lexer.Position { return p.Pos }

// A Node in the AST.
type Node interface {
	Position() lexer.Position
	children() (children []Node)
}

// VisitorFunc can be used to walk all nodes in the model.
type VisitorFunc func(node Node, next func() error) error

// Visit calls the visitor function on all nodes.
func Visit(node Node, visit VisitorFunc) error {
	if node == nil {
		return nil
	}
	if reflect.ValueOf(node).Kind() == reflect.Ptr && reflect.ValueOf(node).IsNil() {
		return nil
	}
	return visit(node, func() error {
		for _, child := range node.children() {
			if err := Visit(child, visit); err != nil {
				return err
			}
		}
		return nil
	})
}
