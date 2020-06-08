package types

import (
	"fmt"
)

//go:generate stringer -type Property

// Property of a value.
type Property int64

// Has returns true if the given property is set.
func (p Property) Has(prop Property) bool { return p&prop != 0 }

// Set proerties.
func (p *Property) Set(prop Property) { *p |= prop }

// Value properties.
const (
	Assignable Property = 1 << iota
)

type Field struct {
	Nme string
	*Value
}

func (f Field) Name() string { return f.Nme }

// Var creates a mutable Value.
func Var(typ Type) *Value {
	return &Value{Typ: typ, Properties: Assignable}
}

// Let creates an immutable Value.
func Let(typ Type) *Value {
	return &Value{Typ: typ}
}

// ToValue attempts to convert a reference to a value.
func ToValue(ref Reference) *Value {
	switch ref := ref.(type) {
	case *Value:
		return ref

	case *Field:
		return ref.Value
	}
	return nil
}

// A Value reference.
type Value struct {
	Typ        Type
	Properties Property
}

var _ Reference = &Value{}

func (v *Value) Kind() Kind { return v.Typ.Kind() }
func (v *Value) Type() Type { return v.Typ }
func (v *Value) Fields() []NamedReference {
	var flds []NamedReference
	for _, ft := range v.Type().Fields() {
		flds = append(flds, Field{Nme: ft.Nme, Value: &Value{
			Typ:        ft.Typ,
			Properties: v.Properties,
		}})
	}
	return flds
}
func (v *Value) String() string { return fmt.Sprintf("%s value", v.Kind()) }
