package types

//go:generate stringer -linecomment -type Kind

type Kind int

const (
	KindNone    Kind = iota // none
	KindGeneric             // generic
	KindFunc                // function
	// Numeric constant (can become any numeric type).
	KindNumberInt   // literal-int
	KindNumberFloat // literal-float
	KindString      // string
	KindBool        // bool
	KindInt         // int
	KindFloat       // float
	KindTuple       // tuple
	KindClass       // class
	KindEnum        // enum
	KindCase        // case
	KindAlias       // alias
)

// IsScalar returns true if the type is a scalar (string, bool, int, float).
func (i Kind) IsScalar() bool {
	switch i {
	case KindString, KindBool, KindInt, KindFloat:
		return true
	}
	return false
}
