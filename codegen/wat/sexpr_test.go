package wat

import (
	"strings"
	"testing"

	"github.com/alecthomas/assert/v2"
)

func TestSExpr(t *testing.T) {
	tests := []struct {
		name     string
		expr     Node
		expected string
	}{
		{name: "Int", expr: Int(10), expected: "10"},
		{name: "Float", expr: Float(10.5), expected: "10.5"},
		{name: "String", expr: String("hello world"), expected: `"hello world"`},
		{name: "ID", expr: ID("world"), expected: `world`},
		{name: "Var", expr: Var("world"), expected: `$world`},
		{name: "List", expr: List{
			Int(10),
			Float(10.5),
			String("hello"),
			ID("world"),
			Var("var"),
		}, expected: "(10 10.5 \"hello\" world $var)"},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			w := &strings.Builder{}
			test.expr.write("", w)
			assert.Equal(t, test.expected, w.String())
		})
	}
}
