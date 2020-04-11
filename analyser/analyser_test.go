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
	type ref struct {
		ref types.Reference
		fix func(in types.Reference)
	}
	type refs map[string]ref

	tests := []struct {
		name  string
		input string
		fail  string
		refs  refs
	}{
		{name: "Call",
			input: `
				fn f(): int { return 1 }
				let v = f()
				`,
			refs: refs{
				"f": {&types.Function{ReturnType: types.Int}, nil},
				"v": {&types.Value{Typ: types.Int}, nil},
			},
		},
		{name: "CallInvalidNumberOfArguments",
			input: `
				fn f(a, b: int): int { return 1 }
				let v = f()
				`,
			fail: "3:14: 0 parameters provided for function that takes 2 parameters",
		},
		{name: "CallInvalidArgumentType",
			input: `
				fn f(a, b: int): int { return 1 }
				let v = f(1, "moo")
				`,
			fail: "3:18: can't coerce \"b\" from string to int",
		},
		{name: "VarDecl",
			input: "let a, b = 1, c, d: string",
			refs: refs{
				"a": {&types.Value{Typ: types.Int}, nil},
				"b": {&types.Value{Typ: types.Int}, nil},
				"c": {&types.Value{Typ: types.String}, nil},
				"d": {&types.Value{Typ: types.String}, nil},
			},
		},
		{name: "VarDeclUntyped",
			input: "let a, b = 1, c, d: string, e",
			fail:  "1:29: type not specified (and no default value provided)",
		},
		{name: "ConstantTypeMismatch",
			input: "let a: string = 1",
			fail:  "1:17: can't assign int to string",
		},
		{name: "Class",
			input: `class Class {}`,
			refs: refs{
				"Class": {&types.Class{}, nil},
			},
		},
		{name: "ClassCreationNoInit",
			input: `
				class Class {}
		
				let instance = Class()
				`,
			refs: refs{
				"instance": {&types.Value{Typ: &types.Class{}}, nil},
			},
		},
		{name: "ClassCreationCustomInit",
			input: `
				class Class {
					init(a, b: int) {
					}
				}
		
				let instance = Class(1, 2)
				`,
			refs: refs{
				"instance": {
					&types.Value{
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
					nil},
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
			refs: refs{
				"Class.field":  {types.Int, nil},
				"Class.method": {&types.Function{ReturnType: types.None}, nil},
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
			refs: refs{
				"Enum.field":  {types.Int, nil},
				"Enum.method": {&types.Function{ReturnType: types.None}, nil},
				"Enum.None":   {&types.Case{Name: "None"}, normaliseCase},
				"Enum.Int":    {&types.Case{Name: "Int", Case: types.Int}, normaliseCase},
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
			refs: refs{
				"none":  {&types.Value{Typ: &types.Case{Name: "None"}}, normaliseCaseValue},
				"value": {&types.Value{Typ: &types.Case{Name: "Int", Case: types.Int}}, normaliseCaseValue},
			},
		},
		{name: "SwitchOnValue",
			input: `
				fn f() {
					let a = 1
					switch a {
					case 1:
					case 2:
					}
				}
			`,
		},
		{name: "SwitchOnValueInvalidCaseType",
			input: `
				fn f() {
					let a = 1
					switch a {
					case "one":
					case 2:
					}
				}
			`,
			fail: "5:11: can't select case of type string value from int",
		},
		{name: "SwitchOnEnum",
			input: `
				enum Enum {
					case None
					case Int(int)
				}
		
				fn f() {
					let a = Enum.Int(1)
		
					switch a {
					case .None:
					case .Int(n):
						g = n
					}
				}
		
				let g = 1
			`,
		},
		{name: "AssignCaseToEnum",
			input: `
				enum Enum {
				case None
				case Int(int)
				}
		
				fn f() {
					let a: Enum = Enum.None
					let b: Enum = Enum.Int(1)
				}
			`,
		},
		{name: "SwitchOnEnumDefault",
			input: `
				enum Enum {
					case None
					case Int(int)
				}
		
				fn f() {
					let a: Enum = Enum.Int(1)
		
					switch a {
					case .Int(n):
						g = n
					default:
					}
				}
		
				let g = 1
			`,
		},
		{name: "SwitchNotExhaustive",
			input: `
				enum Enum {
					case None
					case Int(int)
				}
		
				fn f() {
					let a = Enum.Int(1)
		
					switch a {
					case .None:
					}
				}
			`,
			fail: `10:6: cases not matched: Int`,
		},
		{name: "SwitchOnEnumUnknownCase",
			input: `
				enum Enum {
					case None
					case Int(int)
				}
		
				fn f() {
					let a = Enum.Int(1)
		
					switch a {
					case .Unknown:
					}
				}
			`,
			fail: `11:11: invalid enum case "Unknown"`,
		},
		{name: "SwitchOnEnumNoTypeProvided",
			input: `
				enum Enum {
					case Int(int)
				}
		
				fn f() {
					let a = Enum.Int(1)
		
					switch a {
					case .Int:
					}
				}
			`,
			fail: `10:11: typed enum case "Int" requires a variable`,
		},
		{name: "SwitchOnEnumNoTypeExpected",
			input: `
				enum Enum {
					case Int
				}
		
				fn f() {
					let a = Enum.Int
		
					switch a {
					case .Int(n):
					}
				}
			`,
			fail: `10:11: case "Int" does not have a type to apply`,
		},
		{name: "SwitchOnEnumExtraParameter",
			input: `
				enum Enum {
					case Int(int)
				}
		
				fn f() {
					let a = Enum.Int(1)
		
					switch a {
					case .Int(n, m):
					}
				}
			`,
			fail: `10:17: unexpected token "," (expected ")")`,
		},
		{name: "GenericClass",
			input: `
				class Pair<A, B> {
					let a: A
					let b: B
				}
			`},
		{name: "GenericConstructor",
			input: `
				class Pair<A, B> {
					let a: A
					let b: B
				}
		
				// let a = Pair<string, string>()
				`,
		},
		{name: "GenericEnum",
			input: `
				enum Maybe<T> {
					case None
					case Some(T)
				}
			`},
		{name: "MultiGenericEnum",
			input: `
				enum OneOf<A, B> {
					case First(A)
					case Second(B)
				}
			`},
		{name: "Self",
			input: `
				class Class {
					let a: int
	
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
			`},
		{name: "GoCall",
			input: `
			fn f() {}
		
			fn main() {
			}
			`,
		},
		{name: "ArrayLiteralInferred",
			input: `
				let a = [1, 2, 3]
			`,
			refs: refs{
				"a": {&types.Value{types.Array(types.Int)}, nil},
			}},
		{name: "ArrayLiteralExplicit",
			input: `
				let a: [int] = [1, 2, 3]
			`,
			refs: refs{
				"a": {&types.Value{types.Array(types.Int)}, nil},
			}},
		{name: "ArrayLiteralInvalidExplicit",
			input: `
				let a: [string] = [1, 2, 3]
			`,
			fail: `2:23: can't assign [int] to [string]`,
		},
		{name: "ArrayLiteralAssignToMap",
			input: `
				let a: {string: int} = [1, 2, 3]
			`,
			fail: `2:28: can't assign [int] to {string:int}`,
		},
		{name: "ArrayLiteralHeterogeneous",
			input: `
				let a = [1, 2, "3"]
			`,
			fail: `2:20: inconsistent element types int and string`,
		},
		{name: "ArrayLiteralHeterogenousLiteralNumbersInt",
			input: `
				let a = [1, 1.2]
			`,
			refs: refs{
				"a": {&types.Value{types.Array(types.Int)}, nil},
			},
		},
		{name: "ArrayLiteralHeterogenousLiteralNumbersFloat",
			input: `
				let a = [1.2, 1]
			`,
			refs: refs{
				"a": {&types.Value{types.Array(types.Float)}, nil},
			},
		},
		{name: "SetLiteral",
			input: `
				let a = {1, 2, 3}
			`,
			refs: refs{
				"a": {&types.Value{types.Set(types.Int)}, nil},
			}},
		{name: "SetLiteralHeterogeneous",
			input: `
				let a = {1, 2, "3"}
			`,
			fail: `2:20: inconsistent element types int and string`,
		},
		{name: "SetAndDictValuesIntermingled",
			input: `
				let a = {1, 2:3}
			`,
			fail: `2:13: dict value in set at index 1`,
		},
		{name: "DictLiteral",
			input: `
				let a = {1:"2", 2:"3", 3:"4"}
			`,
			refs: refs{
				"a": {&types.Value{types.Map(types.Int, types.String)}, nil},
			}},
		{name: "DictLiteralHeterogeneousKey",
			input: `
				let a = {1:2, 2:3, "3":4}
			`,
			fail: `2:24: inconsistent element types int and string`,
		},
		{name: "DictLiteralHeterogeneousValue",
			input: `
				let a = {1:2, 2:3, 3:"4"}
			`,
			fail: `2:26: inconsistent element types int and string`,
		},
		{name: "ArrayType",
			input: `
				let a: [int]
			`,
			refs: refs{
				"a": ref{&types.Value{types.Array(types.Int)}, nil},
			},
		},
		{name: "OptionalDecl",
			input: `
				let a:int? = 1
			`,
			refs: refs{
				"a": ref{&types.Value{types.Optional(types.Int)}, nil},
			},
		},
		{name: "NestedEnum",
			input: `
				enum Scalar {
					case Number(float)
					case String(string)
				}

				enum Value {
					case Scalar(Scalar)
					case List([Scalar])
					case Hash({string: Scalar})
				}
			`,
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			ast, err := parser.ParseString(test.input + "\n")
			if err != nil {
				if test.fail == "" {
					require.NoError(t, err)
				} else {
					require.EqualError(t, err, test.fail)
				}
				return
			}
			program, err := Analyse(ast)
			if test.fail != "" {
				require.EqualError(t, err, test.fail)
			} else {
				require.NoError(t, err)
				if test.refs != nil {
					for key, ref := range test.refs {
						expected := ref.ref
						actual := resolve(program.Root, key)
						if ref.fix != nil {
							ref.fix(actual)
						}
						require.Equal(t, expected, actual, "%s", repr.String(actual, repr.Indent("  ")))
					}
				}
			}
		})
	}
}

func normaliseCase(in types.Reference) {
	in.(*types.Case).Enum = nil
}

func normaliseCaseValue(in types.Reference) {
	in.(*types.Value).Typ.(*types.Case).Enum = nil
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
