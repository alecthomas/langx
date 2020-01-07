package types

import (
	"fmt"
	"strings"

	"github.com/alecthomas/langx/parser"
)

type Op = parser.Op

type TypeField struct {
	Name string
	Type Type
}

// A Type.
type Type interface {
	Reference
	// Returns true if this type can be coerced to the other type.
	Coerce(other Type) Type
	// Returns true if the given operator can be applied to this type
	// and (optionally) the other type. For unary operators rhs will
	// be None.
	CanApply(op Op, rhs Type) bool
	// Fields (if any).
	Fields() []TypeField
}

// TypeFieldByName attempts to find a field on a type, by name.
func TypeFieldByName(t Type, name string) *TypeField {
	for _, f := range t.Fields() {
		if f.Name == name {
			return &f
		}
	}
	return nil
}

var (
	None   Type = Builtin(KindNone)
	Number Type = Builtin(KindNumber)
	// Builtin concrete types.
	Int    Type = Builtin(KindInt)
	Float  Type = Builtin(KindFloat)
	String Type = Builtin(KindString)
	Bool   Type = Builtin(KindBool)
)

type Generic struct {
	Constraints []Type
}

func (t Generic) Type() Type                        { return t }
func (t Generic) Kind() Kind                        { return KindType }
func (t Generic) Coerce(other Type) Type            { return nil }
func (t Generic) CanApply(op Op, other Type) bool   { return false }
func (t Generic) Fields() []TypeField               { return nil }
func (t Generic) FieldByName(name string) Reference { return nil }
func (t Generic) String() string                    { return "meta" }

// Builtin represents a builtin type.
type Builtin Kind

func (b Builtin) String() string { return Kind(b).String() }
func (b Builtin) GoString() string {
	s := Kind(b).String()
	s = "types." + strings.ToUpper(s[0:1]) + s[1:]
	return s
}
func (b Builtin) Type() Type                        { return b }
func (b Builtin) Name() string                      { return b.Kind().String() }
func (b Builtin) Kind() Kind                        { return Kind(b) }
func (b Builtin) Fields() []TypeField               { return nil }
func (b Builtin) FieldByName(name string) Reference { return nil }

func (b Builtin) Coerce(other Type) Type {
	if b == other || coercionMap[coercionKey{b.Kind(), other.Kind()}] {
		return other
	}
	return nil
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

var _ Type = &Function{}

func (f *Function) Type() Type                        { return f }
func (f *Function) Kind() Kind                        { return KindFunc }
func (f *Function) Coerce(other Type) Type            { return nil }
func (f *Function) CanApply(op Op, other Type) bool   { return false }
func (f *Function) Fields() []TypeField               { return nil }
func (f *Function) FieldByName(name string) Reference { return nil }
func (f *Function) String() string {
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
	Flds []TypeField
	Init *Function
}

var _ Type = &Class{}

func (s *Class) Type() Type { return s }
func (s *Class) Kind() Kind { return KindClass }
func (s *Class) Coerce(other Type) Type {
	if other == s {
		return other
	}
	return nil
}
func (s *Class) CanApply(op Op, other Type) bool { return false }
func (s *Class) Fields() []TypeField             { return s.Flds }
func (s *Class) FieldByName(name string) Reference {
	for _, fld := range s.Flds {
		if fld.Name == name {
			return fld.Type
		}
	}
	return nil
}
func (s *Class) String() string { return "class" }

type Case struct {
	Name string
	Enum *Enum
	Case Type
}

func (c *Case) Type() Type                        { return c }
func (c *Case) Kind() Kind                        { return KindCase }
func (c *Case) Coerce(other Type) Type            { return nil }
func (c *Case) CanApply(op Op, rhs Type) bool     { return false }
func (c *Case) Fields() []TypeField               { return nil }
func (c *Case) FieldByName(name string) Reference { return nil }
func (c *Case) String() string                    { return "case" }

var _ Type = &Case{}

type Enum struct {
	Flds []TypeField
	Init *Function
}

var _ Type = &Enum{}

func (e *Enum) Type() Type { return e }
func (e *Enum) Kind() Kind { return KindEnum }
func (e *Enum) Coerce(other Type) Type {
	if cse, ok := other.(*Case); ok {
		return cse
	}
	if other == e {
		return other
	}
	return nil
}
func (e *Enum) CanApply(op Op, other Type) bool { return false }
func (e *Enum) Fields() []TypeField             { return e.Flds }
func (e *Enum) FieldByName(name string) Reference {
	for _, fld := range e.Flds {
		if fld.Name == name {
			return fld.Type
		}
	}
	return nil
}
func (e *Enum) String() string { return "enum" }

func (e *Enum) Cases() []*Case {
	cases := []*Case{}
	for _, fld := range e.Flds {
		if cse, ok := fld.Type.(*Case); ok {
			cases = append(cases, cse)
		}
	}
	return cases
}

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
