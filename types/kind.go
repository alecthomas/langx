package types

//go:generate stringer -linecomment -type Kind

type Kind int

const (
	KindNone Kind = iota // none
	// TODO: Get rid of this. It's only used for builtin compound types (map and array)
	//  and should be replaced by classes.
	KindGeneric // generic
	KindFunc    // function
	// Numeric constant (can become any numeric type).
	KindLiteralInt    // literal int
	KindLiteralFloat  // literal float
	KindLiteralString // literal string
	KindString        // string
	KindBool          // bool
	KindInt           // int
	KindFloat         // float
	KindTuple         // tuple
	KindClass         // class
	KindEnum          // enum
	KindCase          // case
	KindAlias         // alias
	KindAny           // any
	KindInterface     // interface
)

// IsScalar returns true if the type is a scalar (string, bool, int, float).
func (i Kind) IsScalar() bool {
	switch i {
	case KindString, KindBool, KindInt, KindFloat:
		return true
	}
	return false
}
