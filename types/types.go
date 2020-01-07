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

// A Reference is either a Type or a *Value.
type Reference interface {
	// Kind of the Reference.
	Kind() Kind
	// Type of the Reference.
	//
	// For Types this will always return itself.
	Type() Type
	// FieldByName returns the "." referenced field, if any.
	FieldByName(name string) Reference
	String() string
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
	//
	// For generic types this will be the type parameters.
	Fields() []TypeField
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

// A Generic type.
type Generic struct {
	Constraints []TypeField
}

func (t Generic) Type() Type { return t }
func (t Generic) Kind() Kind { return KindGeneric }
func (t Generic) Coerce(other Type) Type {
	otherg, ok := other.Type().(Generic)
	if !ok {
		return nil
	}
	if len(otherg.Constraints) != len(t.Constraints) {
		return nil
	}
	for i, c := range otherg.Constraints {
		if t.Constraints[i].Type.Coerce(c.Type) == nil {
			return nil
		}
	}
	return other
}
func (t Generic) CanApply(op Op, other Type) bool { return false }
func (t Generic) Fields() []TypeField             { return t.Constraints }
func (t Generic) FieldByName(name string) Reference {
	for _, f := range t.Constraints {
		if f.Name == name {
			return f.Type
		}
	}
	return nil
}
func (t Generic) String() string {
	if len(t.Constraints) == 0 {
		return "generic"
	}
	constraints := []string{}
	for _, c := range t.Constraints {
		constraints = append(constraints, c.Type.String())
	}
	return fmt.Sprintf("generic<%s>", strings.Join(constraints, ", "))
}

type MapType struct {
	Generic
}

func Map(key, value Type) Type {
	return MapType{Generic{Constraints: []TypeField{
		{"Key", key},
		{"Value", value},
	}}}
}

func (m MapType) Type() Type { return m }
func (m MapType) String() string {
	return fmt.Sprintf("{%s:%s}", m.Constraints[0].Type, m.Constraints[1].Type)
}

type ArrayType struct {
	Generic
}

func Array(value Type) Type {
	return ArrayType{Generic{Constraints: []TypeField{{"Value", value}}}}
}

func (a ArrayType) String() string { return fmt.Sprintf("[%s]", a.Constraints[0].Type) }

type SetType struct {
	Generic
}

func Set(value Type) Type {
	return SetType{Generic{Constraints: []TypeField{{"Value", value}}}}
}

func (s SetType) Type() Type     { return s }
func (s SetType) String() string { return fmt.Sprintf("{%s}", s.Constraints[0].Type) }

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

// MakeConcrete maps "t" to a concrete Reference if it is a compile-time only type.
func MakeConcrete(r Reference) (Reference, error) {
	switch r.Type() {
	case Number:
		if _, ok := r.(*Value); ok {
			return &Value{Int}, nil
		}
		return Int, nil

	case None:
		return nil, fmt.Errorf("can't reference \"none\"")
	}
	return r, nil
}
