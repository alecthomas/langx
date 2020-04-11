package codegen

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/alecthomas/langx/analyser"
	"github.com/alecthomas/langx/parser"
)

func TestGenerate(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		expected string
		fail     bool
	}{
		{name: "FuncDecl",
			source: `
				pub fn f() {}
			`,
			expected: `
func f() {
}
`,
		},
		{name: "VarDeclNumber",
			source: `
				let a = 10
			`,
			expected: `
var (
  a int = 10
)
`,
		},
		{name: "VarDeclStr",
			source: `
				let a = "hello"
			`,
			expected: `
var (
  a string = "hello"
)
`,
		},
		{name: "VarDeclBool",
			source: `
				let a = true 
			`,
			expected: `
var (
  a bool = true
)
`,
		},
		{name: "VarDeclSet",
			source: `
let a = {1, 2, 3}
`,
			expected: `
var (
  a = map[int]bool{1: true, 2: true, 3: true}
)
`,
		},
		{name: "EnumDecl",
			source: `
				enum OI {
				case None
				case Some(int)
				}
`,
			expected: `
type OI struct {
  None bool
  Some *int
}
`,
		},
		{name: "EnumDeclAndUsage",
			source: `
				enum OI {
				case None
				case Some(int)
				}
	
				let a = OI.Some(1)
				let b = OI.None
			`,
			expected: `
type OI struct {
  None bool
  Some *int
}
var (
  a = &OI{Some: runtime.Int(1)}
)
var (
  b = &OI{None: true}
)
			`,
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			w := &strings.Builder{}
			ast, err := parser.Parse(strings.NewReader(test.source))
			require.NoError(t, err)
			prog, err := analyser.Analyse(ast)
			require.NoError(t, err)
			err = Generate(w, prog)
			if test.fail {
				require.Error(t, err)
			} else {
				require.NoError(t, err)
			}
			require.Equal(t, strings.TrimSpace(test.expected), strings.TrimSpace(w.String()))
		})
	}
}
