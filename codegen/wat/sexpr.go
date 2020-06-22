package wat

import (
	"fmt"
	"hash/fnv"
	"io"
	"strconv"

	"github.com/alecthomas/langx/parser"
)

var oneEntryPerLine = map[string]bool{
	"module": true, "func": true,
}

// Write Node to w.
func Write(w io.Writer, s Node) error {
	s.write("", w)
	return nil
}

// Node in an s-expression.
type Node interface {
	write(indent string, w io.Writer)
}

// Int is a 64-bit integer s-expression node.
type Int int64

func (i Int) write(indent string, w io.Writer) { fmt.Fprint(w, strconv.FormatInt(int64(i), 10)) }

// Float is a 64-bit float s-expression node.
type Float float64

func (f Float) write(indent string, w io.Writer) {
	fmt.Fprint(w, strconv.FormatFloat(float64(f), 'g', -1, 64))
}

// String is a string s-expression node.
type String string

func (s String) write(indent string, w io.Writer) { w.Write([]byte(strconv.Quote(string(s)))) }

// ID is an identifier node.
type ID string

func (i ID) write(indent string, w io.Writer) { w.Write([]byte(i)) }

// Var is a "$var" s-expression node.
type Var string

// UVar returns a unique Var for the given node.
//
// The Var is stable with regard to the position of the node.
func UVar(node parser.Node) Var {
	h := fnv.New32a().Sum([]byte(node.Position().String()))
	return Var(fmt.Sprintf("_%x", h))
}

func (r Var) write(indent string, w io.Writer) { w.Write([]byte("$" + r)) }

// List is a sub-s-expression.
type List []Node

func (l List) write(indent string, w io.Writer) {
	byLine := false
	if id, ok := l[0].(ID); ok && oneEntryPerLine[string(id)] {
		byLine = true
	}
	fmt.Fprint(w, "(")
	for i, e := range l {
		if i > 0 {
			if byLine {
				fmt.Fprintf(w, "\n%s", indent+"  ")
			} else {
				fmt.Fprint(w, " ")
			}
		}
		e.write(indent+"  ", w)
	}
	fmt.Fprint(w, ")")
}

// Add an element to the list.
func (l *List) Add(e ...Node) {
	*l = append(*l, e...)
}
