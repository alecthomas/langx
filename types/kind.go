package types

type Kind int

const (
	KindNone Kind = iota
	KindType
	KindFunc
	KindNumber // Numeric constant (can become any numeric type).
	KindString
	KindBool
	KindInt
	KindFloat
	KindChan
	KindTuple
	KindClass
	KindEnum
	KindAlias
)

func (i Kind) IsScalar() bool {
	switch i {
	case KindString, KindBool, KindInt, KindFloat, KindChan:
		return true
	}
	return false
}

func (i Kind) String() string {
	switch i {
	case KindNone:
		return "none"
	case KindType:
		return "type"
	case KindFunc:
		return "function"
	case KindString:
		return "string"
	case KindBool:
		return "bool"
	case KindNumber:
		return "number"
	case KindInt:
		return "int"
	case KindFloat:
		return "float"
	case KindChan:
		return "chan"
	case KindTuple:
		return "tuple"
	case KindClass:
		return "class"
	case KindEnum:
		return "enum"
	case KindAlias:
		return "alias"
	default:
		panic("??")
	}
}
