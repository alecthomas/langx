package types

import (
	"fmt"
	"strings"

	"github.com/alecthomas/langx/parser"
)

// A Reference to either a Value or a Type.
type Reference interface {
	Type() Type
}
type Op = parser.Op

type Field struct {
	Name string
	Type Type
}

// A Type.
type Type interface {
	// Type of the Type.
	Type() Type
	// Kind of the Type.
	Kind() Kind
	// Returns true if this type can be coerced to the other type.
	CoercibleTo(other Type) bool
	// Returns true if the given operator can be applied to this type
	// and (optionally) the other type. For unary operators rhs will
	// be None.
	CanApply(op Op, rhs Type) bool
	// Fields (if any).
	Fields() []Field
	fmt.Stringer
}

// FieldByName attempts to find a field on a type, by name.
func FieldByName(t Type, name string) *Field {
	for _, f := range t.Fields() {
		if f.Name == name {
			return &f
		}
	}
	return nil
}

type typeType struct{}

var (
	// Meta is a singleton representing the type of types.
	Meta Type = typeType{}
	// NoneType is a singleton representing "none".
	None Type = Builtin(KindNone)
	// Builtin types.
	Number Type = Builtin(KindNumber)
	Int    Type = Builtin(KindInt)
	Float  Type = Builtin(KindFloat)
	String Type = Builtin(KindString)
	Bool   Type = Builtin(KindBool)
)

func (t typeType) Type() Type                      { return t }
func (t typeType) Kind() Kind                      { return KindType }
func (t typeType) CoercibleTo(other Type) bool     { return false }
func (t typeType) CanApply(op Op, other Type) bool { return false }
func (t typeType) Fields() []Field                 { return nil }
func (t typeType) String() string                  { return "meta" }

// Builtin represents a builtin type.
type Builtin Kind

func (b Builtin) String() string { return Kind(b).String() }
func (b Builtin) GoString() string {
	s := Kind(b).String()
	s = "types." + strings.ToUpper(s[0:1]) + s[1:]
	return s
}
func (b Builtin) Type() Type      { return Meta }
func (b Builtin) Name() string    { return b.Kind().String() }
func (b Builtin) Kind() Kind      { return Kind(b) }
func (b Builtin) Fields() []Field { return nil }
func (b Builtin) CoercibleTo(other Type) bool {
	return b == other || coercionMap[coercionKey{b.Kind(), other.Kind()}]
}
func (b Builtin) CanApply(op Op, other Type) bool {
	return opMap[opKey{b.Kind(), op, other.Kind()}]
}

type Parameter struct {
	Name string
	Type
}

type Function struct {
	Parameters []Parameter
	ReturnType Type
}

var _ Type = Function{}

func (f Function) Type() Type                      { return Meta }
func (f Function) Kind() Kind                      { return KindFunc }
func (f Function) CoercibleTo(other Type) bool     { return false }
func (f Function) CanApply(op Op, other Type) bool { return false }
func (f Function) Fields() []Field                 { return nil }
func (f Function) String() string {
	w := &strings.Builder{}
	fmt.Fprint(w, "fn(")
	for i, param := range f.Parameters {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		fmt.Fprintf(w, "%s %s", param.Name, param.Type)
	}
	fmt.Fprint(w, ")")
	if f.ReturnType != None {
		fmt.Fprintf(w, " %s", f.ReturnType)
	}
	return w.String()
}

type Class struct {
	fields []Field
}

var _ Type = &Class{}

func (s *Class) Type() Type                      { return Builtin(KindClass) }
func (s *Class) Kind() Kind                      { return KindClass }
func (s *Class) CoercibleTo(other Type) bool     { return other == s }
func (s *Class) CanApply(op Op, other Type) bool { return false }
func (s *Class) Fields() []Field                 { return s.fields }
func (s *Class) String() string                  { return "class" }

// MakeConcrete maps "t" to a concrete Type if it is a compile-time only type.
func MakeConcrete(t Type) (Type, error) {
	switch t {
	case Number:
		return Int, nil

	case None:
		return nil, fmt.Errorf("none is unusable")
	}
	return t, nil
}
