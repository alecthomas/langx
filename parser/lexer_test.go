package parser

import (
	"strings"
	"testing"

	"github.com/alecthomas/assert/v2"
)

func TestLexer(t *testing.T) {
	tokens, err := parser.Lex("", strings.NewReader(`
	// Comment
	fn foo() { /* Multi-line
		comment */
		if true {
			print("hello")
		}
		a += 1 + \
			 2
		b = `+"`literal string`"+`
	}
	`))
	assert.NoError(t, err)
	actual := []string{}
	for _, token := range tokens {
		if token.Type == parser.Lexer().Symbols()["Whitespace"] {
			continue
		}
		actual = append(actual, token.Value)
	}
	expected := []string{
		"fn", "foo", "(", ")", "{", "if", "true", "{", "print", "(", "\"", "hello", "\"", ")", ";", "}", ";", "a", "+=",
		"1", "+", "2", ";", "b", "=", "literal string", "}", ";", "",
	}
	assert.Equal(t, expected, actual)
}
