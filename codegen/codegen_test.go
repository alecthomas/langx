package codegen

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/alecthomas/langx/analyser"
	"github.com/alecthomas/langx/parser"
)

func TestCodeGen(t *testing.T) {
	tests := []struct {
		name   string
		input  string
		output string
	}{
		{name: "FunctionWithReturn",
			input: `
				fn add(a, b: int): int {
					return a + b
				}
			`,
			output: `
(module
  (func
    $add
    (param $a i64)
    (param $b i64)
    (result i64)
    local.get
    $a
    local.get
    $b
    i64.add))
`},
		{name: "StringData",
			input: `
				let a = "Hello"
				let b = "World"
			`,
			output: `
(module
  (data (i32.const 0) "Hello")
  (data (i32.const 5) "World"))
			`},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			ast, err := parser.Parse(strings.NewReader(test.input))
			require.NoError(t, err, test.input)
			program, err := analyser.Analyse(ast)
			require.NoError(t, err, test.input)
			w := &strings.Builder{}
			err = Generate(w, program)
			require.NoError(t, err, test.input)
			require.Equal(t, strings.TrimSpace(test.output), strings.TrimSpace(w.String()))
			t.Log(test.input)
			t.Log("\n" + w.String())
		})
	}
}
