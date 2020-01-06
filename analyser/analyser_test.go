package analyser

import (
	"strings"
	"testing"

	"github.com/alecthomas/repr"
	"github.com/stretchr/testify/require"

	"github.com/alecthomas/langx/parser"
	"github.com/alecthomas/langx/types"
)

func TestAnalyser(t *testing.T) {
	type Refs map[string]types.Reference
	tests := []struct {
		name  string
		input string
		fail  string
		refs  Refs
	}{
		{name: "Call",
			input: `
				fn f() int { return 1 }
				let v = f()
				`,
			refs: Refs{
				"f": &types.Function{ReturnType: types.Int},
				"v": &types.Value{Typ: types.Int},
			},
		},
		{name: "CallInvalidArgs",
			input: `
				fn f(a, b int) int { return 1 }
				let v = f()
				`,
			fail: "3:14: 0 parameters provided for function that takes 2 parameters",
		},
		{name: "CallInvalidArgs",
			input: `
				fn f(a, b int) int { return 1 }
				let v = f(1, "moo")
				`,
			fail: "3:18: can't coerce parameter \"b\" from string to int",
		},
		{name: "VarDecl",
			input: "let a, b = 1, c, d string",
			refs: Refs{
				"a": &types.Value{Typ: types.Int},
				"b": &types.Value{Typ: types.Int},
				"c": &types.Value{Typ: types.String},
				"d": &types.Value{Typ: types.String},
			},
		},
		{name: "VarDeclUntyped",
			input: "let a, b = 1, c, d string, e",
			fail:  "1:28: type not specified (and no default value provided)",
		},
		{name: "ConstantTypeMismatch",
			input: "let a string = 1",
			fail:  "1:16: can't assign int to string",
		},
		{name: "Class",
			input: `class Class {}`,
			refs: Refs{
				"Class": &types.Class{},
			},
		},
		{name: "ClassCreationNoInit",
			input: `
				class Class {}
		
				let instance = Class()
				`,
			refs: Refs{
				"instance": &types.Value{Typ: &types.Class{}},
			},
		},
		{name: "ClassCreationCustomInit",
			input: `
				class Class {
					init(a, b int) {
					}
				}
		
				let instance = Class(1, 2)
				`,
			refs: Refs{
				"instance": &types.Value{
					Typ: &types.Class{
						Init: &types.Function{
							Parameters: []types.Parameter{
								{Name: "a", Type: types.Int},
								{Name: "b", Type: types.Int},
							},
							ReturnType: types.None,
						},
					},
				},
			},
		},
		{name: "ClassFields",
			input: `
					class Class {
						let field = 1
						fn method() {
						}
					}
				`,
			refs: Refs{
				"Class.field":  types.Int,
				"Class.method": &types.Function{ReturnType: types.None},
			},
		},
		{name: "EnumFields",
			input: `
					enum Enum {
						case None
						case Int(int)
		
						let field = 1
						fn method() {}
					}
				`,
			refs: Refs{
				"Enum.field":  types.Int,
				"Enum.method": &types.Function{ReturnType: types.None},
				"Enum.None":   &types.Case{Name: "None"},
				"Enum.Int":    &types.Case{Name: "Int", Case: types.Int},
			},
		},
		{name: "EnumCreation",
			input: `
				enum Enum {
					case None
					case Int(int)
				}
		
				let none = Enum.None
				let value = Enum.Int(1)
			`,
			refs: Refs{
				"none":  &types.Value{Typ: &types.Case{Name: "None"}},
				"value": &types.Value{Typ: &types.Case{Name: "Int", Case: types.Int}},
			},
		},
		{name: "SwitchOnValue",
			input: `
				fn f() {
					let a = 1
					switch (a) {
					case 1:
					case 2:
					}
				}
			`,
		},
		{name: "Self",
			input: `
				class Class {
					let a int
	
					init() {
						self.a = 2
					}
	
					fn f() {
						self.a = 2
					}
				}
			`},
		{name: "DeferredBodyAnalysis",
			input: `
			class A {
				init() {
					a = 1
				}

				fn f() {
					a = 2
				}
			}

			fn f() {
				a = 3
			}

			let a = 0
			`,},
		// {name: "SwitchOnEnum",
		// 	input: `
		// 		enum Enum {
		// 			case None
		// 			case Int(int)
		// 		}
		// 		fn f() {
		// 			let a = Enum.Int(1)
		//
		// 			switch (a) {
		// 			case None:
		// 			case Int(n):
		// 			}
		// 		}
		// 	`,
		// },
		{name: "GoCall",
			input: `
			fn f() {}
		
			fn main() {
				go f()
			}
			`,
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			ast, err := parser.ParseString(test.input + "\n")
			require.NoError(t, err)
			program, err := Analyse(ast)
			if test.fail != "" {
				require.EqualError(t, err, test.fail)
			} else {
				require.NoError(t, err)
				if test.refs != nil {
					for key, expected := range test.refs {
						actual := resolve(program.root, key)
						require.Equal(t, expected, actual, "%s", repr.String(actual, repr.Indent("  ")))
					}
				}
			}
		})
	}
}

func resolve(root *Scope, key string) types.Reference {
	parts := strings.Split(key, ".")
	ref := root.Resolve(parts[0])
	for _, part := range parts[1:] {
		ref = ref.FieldByName(part)
		if ref == nil {
			return nil
		}
	}
	return ref
}
