package parser

import (
	"strings"
	"testing"

	"github.com/alecthomas/participle/lexer"
	"github.com/stretchr/testify/require"
)

func TestLexer(t *testing.T) {
	d := new(fixupLexerDefinition)
	l, err := d.Lex(strings.NewReader(`
	// Comment
	fn foo() { /* Multi-line
		comment */
		if true {
		}
		a = 1 + \
			2
	}
	`))
	require.NoError(t, err)
	tokens, err := lexer.ConsumeAll(l)
	require.NoError(t, err)
	actual := []string{}
	for _, token := range tokens {
		if token.Type == d.Symbols()["Whitespace"] {
			continue
		}
		actual = append(actual, token.Value)
	}
	expected := []string{
		"fn", "foo", "(", ")", "{", "if", "true", "{", "}", ";", "a", "=",
		"1", "+", "2", ";", "}", ";", "",
	}
	require.Equal(t, expected, actual)
}
