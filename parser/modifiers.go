package parser

import (
	"strings"
)

// Modifiers for a symbol, as a bit-field.
type Modifiers int

//go:generate stringer -linecomment -type Modifiers

const (
	ModifierPublic   Modifiers = 1 << (2 * iota) // pub
	ModifierOverride                             // override
	ModifierStatic                               // static
)

// Has all of the given modifiers set.
func (m Modifiers) Has(modifier Modifiers) bool {
	return m&modifier == modifier
}

func (m Modifiers) GoString() string {
	var modifiers []string
	if m&ModifierStatic != 0 {
		modifiers = append(modifiers, "parser.ModifierStatic")
	}
	if m&ModifierOverride != 0 {
		modifiers = append(modifiers, "parser.ModifierOverride")
	}
	if m&ModifierPublic != 0 {
		modifiers = append(modifiers, "parser.ModifierPublic")
	}
	return strings.Join(modifiers, "|")
}

func (m *Modifiers) Capture(values []string) error {
	switch values[0] {
	case "pub":
		*m |= ModifierPublic

	case "override":
		*m = ModifierOverride

	case "static":
		*m = ModifierStatic

	default:
		panic("??")
	}
	return nil
}
