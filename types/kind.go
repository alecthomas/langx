package types

//go:generate stringer -linecomment -type Kind

type Kind int

const (
	KindNone Kind = iota // none
	KindType             // type
	KindFunc             // function
	// Numeric constant (can become any numeric type).
	KindNumber // number
	KindString // string
	KindBool   // bool
	KindInt    // int
	KindFloat  // float
	KindChan   // chan
	KindTuple  // tuple
	KindClass  // class
	KindEnum   // enum
	KindCase   // case
	KindAlias  // alias
)

func (i Kind) IsScalar() bool {
	switch i {
	case KindString, KindBool, KindInt, KindFloat, KindChan:
		return true
	}
	return false
}
