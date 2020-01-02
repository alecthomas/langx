package analyser

import (
	"testing"

	"github.com/alecthomas/repr"
	"github.com/stretchr/testify/require"

	"github.com/alecthomas/langx/parser"
)

func TestAnalyser(t *testing.T) {
	s := `
	let a = 1
	
	fn add(a, b int) int {
		return a + b
	}
	
	fn f() {
		let b int = (a + 2) + 3.5 + add(a, 2)
	}
	`
	ast, err := parser.ParseString(s)
	require.NoError(t, err)
	p, err := Analyse(ast)
	require.NoError(t, err)
	repr.Println(p.root)
}
