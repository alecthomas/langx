package types

import (
	"fmt"
)

type Field struct {
	Nme string
	*Value
}

func (f Field) Name() string { return f.Nme }

// A Value reference.
type Value struct {
	Typ Type
}

var _ Reference = &Value{}

func (v *Value) Kind() Kind { return v.Typ.Kind() }
func (v *Value) Type() Type { return v.Typ }
func (v *Value) Fields() []FieldReference {
	var flds []FieldReference
	for _, ft := range v.Type().Fields() {
		flds = append(flds, Field{Nme: ft.Nme, Value: &Value{Typ: ft.Typ}})
	}
	return flds
}
func (v *Value) String() string { return fmt.Sprintf("%s value", v.Kind()) }
