package types

import (
	"fmt"
)

type Field struct {
	Name string
	*Value
}

// A Value reference.
type Value struct {
	Typ Type
}

var _ Reference = &Value{}

func (v *Value) Kind() Kind { return v.Typ.Kind() }
func (v *Value) Type() Type { return v.Typ }
func (v *Value) Fields() []Field {
	flds := []Field{}
	for _, ft := range v.Type().Fields() {
		flds = append(flds, Field{Name: ft.Name, Value: &Value{Typ: ft.Type}})
	}
	return flds
}
func (v *Value) FieldByName(name string) Reference {
	for _, f := range v.Fields() {
		if f.Name == name {
			return f
		}
	}
	return nil
}
func (v *Value) String() string { return fmt.Sprintf("%s value", v.Kind()) }
