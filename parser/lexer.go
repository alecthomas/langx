package parser

import (
	"io"

	"github.com/alecthomas/participle/v2/lexer"
)

// A Lexer that inserts semi-colons and collapses \-separated lines.
type fixupLexerDefinition struct{}

func (l *fixupLexerDefinition) Lex(path string, r io.Reader) (lexer.Lexer, error) { // nolint: golint
	ll, err := lex.Lex(path, r)
	if err != nil {
		return nil, err
	}
	return &fixupLexer{lexer: ll}, nil
}

func (l *fixupLexerDefinition) Symbols() map[string]lexer.TokenType { // nolint: golint
	return lex.Symbols()
}

type fixupLexer struct {
	lexer lexer.Lexer
	last  lexer.Token
}

func (l *fixupLexer) Next() (lexer.Token, error) {
next:
	for {
		token, err := l.lexer.Next()
		if err != nil {
			return token, err
		}
		if token.Value != "\n" {
			l.last = token
			return token, nil
		}

		// Do we need to insert a semi-colon?
		switch l.last.Value {
		case "\\":
			l.last = token
			continue next

		case "break", "continue", "fallthrough", "return", "++", "--", ")", "}", "]", ">":
			token.Value = ";"
			token.Type = ';'

		default:
			switch l.last.Type {
			case numberToken, stringEndToken, identToken:
				token.Value = ";"
				token.Type = ';'

			default:
				l.last = token
				continue next
			}
		}
		l.last = token
		return token, nil
	}
}
