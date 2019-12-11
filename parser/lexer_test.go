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
	actual := []lexer.Token{}
	for _, token := range tokens {
		if token.Type == d.Symbols()["Whitespace"] {
			continue
		}
		token.Pos = lexer.Position{}
		token.Type = 0
		actual = append(actual, token)
	}
	expected := []lexer.Token{
		{Value: "fn"},
		{Value: "foo"},
		{Value: "("},
		{Value: ")"},
		{Value: "{"},
		{Value: "if"},
		{Value: "true"},
		{Value: "{"},
		{Value: "}"},
		{Value: ";"},
		{Value: "a"},
		{Value: "="},
		{Value: "1"},
		{Value: "+"},
		{Value: "2"},
		{Value: ";"},
		{Value: "}"},
		{Value: ";"},
		{},
	}
	require.Equal(t, expected, actual)
}
