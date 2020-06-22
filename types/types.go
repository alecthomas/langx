package types

import (
	"fmt"
	"strings"

	"github.com/alecthomas/langx/parser"
)

type Op = parser.Op

// NamedType represents the field of a type.
type NamedType struct {
	Nme string
	Typ Type
}

func (t NamedType) Kind() Kind     { return t.Typ.Kind() }
func (t NamedType) Type() Type     { return t.Typ.Type() }
func (t NamedType) String() string { return fmt.Sprintf("%s:%s", t.Nme, t.Typ.String()) }
func (t NamedType) Name() string   { return t.Nme }

// A Reference is either a Type or a *Value.
type Reference interface {
	// Kind of the Reference.
	Kind() Kind
	// Type of the Reference.
	//
	// For Types this will always return itself.
	Type() Type
	// Fields() []NamedReference
	String() string
}

type Direction int

const (
	To Direction = iota
	From
)

type TypeDirection struct {
	Direction Direction
	Type      Type
}

// A Type.
type Type interface {
	Reference
	// Returns true if this type can be coerced to the other type.
	Coerce(direction Direction, other Type) Type
	// Returns true if the given operator can be applied to this type
	// and (optionally) the other type. For unary operators rhs will
	// be None.
	CanApply(op Op, rhs Type) bool
	// Fields (if any).
	Fields() []NamedType
	// Type parameters (if any).
	TypeParameters() []NamedType
}

var (
	None          Type = Builtin(KindNone)
	LiteralInt    Type = Builtin(KindLiteralInt)
	LiteralFloat  Type = Builtin(KindLiteralFloat)
	LiteralString Type = Builtin(KindLiteralString)
	// Builtin concrete types.
	Int    Type = Builtin(KindInt)
	Float  Type = Builtin(KindFloat)
	String Type = Builtin(KindString)
	Bool   Type = Builtin(KindBool)
	Any    Type = Builtin(KindAny)
)

// A Generic type.
type Generic struct {
	Constraints []NamedType
}

var _ Type = Generic{}

func (t Generic) Type() Type { return t }
func (t Generic) Kind() Kind { return KindGeneric }
func (t Generic) Coerce(direction Direction, other Type) Type {
	otherg, ok := other.Type().(Generic)
	if !ok {
		return nil
	}
	if len(otherg.Constraints) != len(t.Constraints) {
		return nil
	}
	for i, c := range otherg.Constraints {
		if t.Constraints[i].Typ.Coerce(To, c.Typ) == nil {
			return nil
		}
	}
	return other
}
func (t Generic) CanApply(op Op, other Type) bool { return false }
func (t Generic) Fields() []NamedType             { return t.Constraints }
func (t Generic) TypeParameters() []NamedType     { return t.Constraints }
func (t Generic) FieldByName(name string) Reference {
	for _, f := range t.Constraints {
		if f.Nme == name {
			return f.Typ
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
		constraints = append(constraints, c.Typ.String())
	}
	return fmt.Sprintf("generic<%s>", strings.Join(constraints, ", "))
}

type MapType struct {
	ClassType
}

func Map(key, value Type) Type {
	return &MapType{ClassType{
		TParams: []NamedType{
			{"Key", key},
			{"ResolvedValue", value},
		}},
	}
}

func (m *MapType) Type() Type { return m }
func (m *MapType) String() string {
	return fmt.Sprintf("{%s:%s}", m.TParams[0].Typ, m.TParams[1].Typ)
}

type ArrayType struct {
	Generic
}

func Array(value Type) Type {
	return ArrayType{Generic{Constraints: []NamedType{{"ResolvedValue", value}}}}
}

func (a ArrayType) String() string { return fmt.Sprintf("[%s]", a.Constraints[0].Typ) }

type SetType struct {
	Generic
}

func Set(value Type) Type {
	return SetType{Generic{Constraints: []NamedType{{"ResolvedValue", value}}}}
}

func (s SetType) TypeParameters() []NamedType { return s.Constraints }
func (s SetType) Type() Type                  { return s }
func (s SetType) String() string              { return fmt.Sprintf("{%s}", s.Constraints[0].Typ) }

type OptionalType struct {
	Enum
}

func Optional(some Type) Type {
	return &OptionalType{Enum{TParams: []NamedType{
		{Nme: "None"},
		{Nme: "Some", Typ: some},
	}}}
}

func (s *OptionalType) Coerce(direction Direction, other Type) Type {
	if other == s.Enum.TParams[1].Typ || other.Kind() == KindNone {
		return s
	}
	return nil
}
func (s *OptionalType) Type() Type     { return s }
func (s *OptionalType) String() string { return fmt.Sprintf("%s?", s.Enum.TParams[1].Typ) }

// Builtin represents a builtin type.
type Builtin Kind

func (b Builtin) String() string { return Kind(b).String() }
func (b Builtin) GoString() string {
	s := b.String()
	s = "types." + strings.Replace(strings.Title(s), " ", "", -1)
	return s
}
func (b Builtin) Type() Type                        { return b }
func (b Builtin) Name() string                      { return b.Kind().String() }
func (b Builtin) Kind() Kind                        { return Kind(b) }
func (b Builtin) Fields() []NamedType               { return nil }
func (b Builtin) TypeParameters() []NamedType       { return nil }
func (b Builtin) FieldByName(name string) Reference { return nil }
func (b Builtin) Coerce(direction Direction, other Type) Type {
	if b == other || coercionMap[coercionKey{b.Kind(), other.Kind()}] {
		return other
	}
	return nil
}
func (b Builtin) CanApply(op Op, other Type) bool {
	return opMap[opKey{b.Kind(), op, other.Kind()}]
}

type Function struct {
	Parameters []NamedType
	ReturnType Type
}

var _ Type = &Function{}

func (f *Function) Type() Type                                  { return f }
func (f *Function) Kind() Kind                                  { return KindFunc }
func (f *Function) Coerce(direction Direction, other Type) Type { return nil }
func (f *Function) CanApply(op Op, other Type) bool             { return false }
func (f *Function) Fields() []NamedType                         { return nil }
func (f *Function) TypeParameters() []NamedType                 { return nil }
func (f *Function) FieldByName(name string) Reference           { return nil }
func (f *Function) String() string {
	w := &strings.Builder{}
	fmt.Fprint(w, "fn(")
	for i, param := range f.Parameters {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		fmt.Fprintf(w, "%s: %s", param.Name(), param.Type())
	}
	fmt.Fprint(w, ")")
	if f.ReturnType != None {
		fmt.Fprintf(w, ": %s", f.ReturnType)
	}
	return w.String()
}

type ClassType struct {
	Name    string
	TParams []NamedType
	Flds    []NamedType
	Init    *Function
}

var _ Type = &ClassType{}

func (s *ClassType) Type() Type { return s }
func (s *ClassType) Kind() Kind { return KindClass }
func (s *ClassType) Coerce(direction Direction, other Type) Type {
	if other == s {
		return other
	}
	return nil
}
func (s *ClassType) CanApply(op Op, other Type) bool { return false }
func (s *ClassType) Fields() []NamedType             { return s.Flds }
func (s *ClassType) TypeParameters() []NamedType     { return s.TParams }
func (s *ClassType) FieldByName(name string) Reference {
	for _, fld := range s.Flds {
		if fld.Nme == name {
			return fld.Typ
		}
	}
	return nil
}
func (s *ClassType) String() string { return "class" }

type Case struct {
	Name string
	Enum *Enum
	Case Type
}

var _ Type = &Case{}

func (c *Case) Type() Type                                  { return c }
func (c *Case) Kind() Kind                                  { return KindCase }
func (c *Case) Coerce(direction Direction, other Type) Type { return nil }
func (c *Case) CanApply(op Op, rhs Type) bool               { return false }
func (c *Case) Fields() []NamedType                         { return nil }
func (c *Case) TypeParameters() []NamedType                 { return nil }
func (c *Case) FieldByName(name string) Reference           { return nil }
func (c *Case) String() string                              { return "case" }

type Specialisation struct {
	Typ        Type
	Parameters []NamedType
}

// Specialise creates a specialisation of a generic type.
func Specialise(base Type, parameters ...Type) Type {
	if len(base.TypeParameters()) != len(parameters) {
		panic("mismatched number of specialised generic parameters")
	}
	fields := make([]NamedType, 0, len(parameters))
	for i, p := range base.Fields() {
		fields = append(fields, NamedType{Nme: p.Nme, Typ: parameters[i]})
	}
	return &Specialisation{
		Typ:        base,
		Parameters: fields,
	}
}

var _ Type = &Specialisation{}

func (s Specialisation) TypeParameters() []NamedType { return nil }
func (s Specialisation) FieldByName(name string) Reference {
	for _, field := range s.Parameters {
		if field.Nme == name {
			return field.Typ
		}
	}
	return nil
}
func (s Specialisation) Kind() Kind                                  { return s.Typ.Kind() }
func (s Specialisation) Type() Type                                  { return s.Typ }
func (s Specialisation) String() string                              { return "specialisation" }
func (s Specialisation) Coerce(direction Direction, other Type) Type { panic("implement me") }
func (s Specialisation) CanApply(op Op, rhs Type) bool               { return false }
func (s Specialisation) Fields() []NamedType                         { return s.Parameters }

type Enum struct {
	Name    string
	TParams []NamedType
	Flds    []NamedType
	Init    *Function
}

var _ Type = &Enum{}

func (e *Enum) Type() Type { return e }
func (e *Enum) Kind() Kind { return KindEnum }
func (e *Enum) Coerce(direction Direction, other Type) Type {
	if cse, ok := other.(*Case); ok {
		return cse
	}
	if other == e {
		return other
	}
	var matched Type
	for _, cse := range e.Cases() {
		// fmt.Println(cse.Name, cse.Case, other)
		if coerced := cse.Case.Coerce(direction, other); coerced != nil {
			// Already have a match, coercion is ambiguous.
			if matched != nil {
				return nil
			}
			matched = coerced
		}
	}
	return matched
}
func (e *Enum) CanApply(op Op, other Type) bool {
	panic(fmt.Sprintf("%s %s", op.String(), other.String()))
}
func (e *Enum) Fields() []NamedType         { return e.Flds }
func (e *Enum) TypeParameters() []NamedType { return e.TParams }
func (e *Enum) FieldByName(name string) Reference {
	for _, fld := range e.Flds {
		if fld.Nme == name {
			return fld.Typ
		}
	}
	return nil
}
func (e *Enum) String() string { return "enum" }

func (e *Enum) Cases() []*Case {
	cases := []*Case{}
	for _, fld := range e.Flds {
		if cse, ok := fld.Typ.(*Case); ok {
			cases = append(cases, cse)
		}
	}
	return cases
}

// Concrete maps "t" to a concrete Reference if it is a compile-time literal.
func Concrete(r Reference) (Reference, error) {
	switch r.Type() {
	case LiteralString:
		if _, ok := r.(*Value); ok {
			return &Value{Typ: String}, nil
		}
		return String, nil

	case LiteralInt:
		if _, ok := r.(*Value); ok {
			return &Value{Typ: Int}, nil
		}
		return Int, nil

	case LiteralFloat:
		if _, ok := r.(*Value); ok {
			return &Value{Typ: Float}, nil
		}
		return Float, nil

	case None:
		return nil, fmt.Errorf("can't reference \"none\"")
	}
	return r, nil
}

// ToOptional makes "r" into an optional (value or type).
func ToOptional(r Reference) Reference {
	switch r := r.(type) {
	case *Value:
		return &Value{Typ: Optional(r.Type())}

	case Type:
		return Optional(r)
	}
	panic("??")
}

// Coerce attempts to coerce a type using either side.
func Coerce(from, to Type) Type {
	if from == nil || to == nil {
		return nil
	}
	switch {
	case from.Coerce(To, to) != nil:
		return to

	case to.Coerce(From, from) != nil:
		return to
	}
	return nil
}

// A NamedReference to a value or type.
type NamedReference interface {
	Name() string
	Reference
}

// FieldByName looks up a value or type's field by name.
func FieldByName(ref Reference, name string) NamedReference {
	switch ref := ref.(type) {
	case *Value:
		for _, fld := range ref.Fields() {
			if fld.Name() == name {
				return fld
			}
		}

	case Type:
		for _, fld := range ref.Fields() {
			if fld.Nme == name {
				return fld
			}
		}
	}
	return nil
}

// TypeName returns the simple name of a type, if any.
func TypeName(t Type) string {
	switch t := t.(type) {
	case *ClassType:
		return t.Name

	case *Enum:
		return t.Name

	case Builtin:
		return t.Name()
	}
	return ""
}
