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
		{name: "ExpressionStatementWithNoEffect",
			input: `
			fn f() {
				let a: int
				a == 10
			}
			`,
			fail: `4:5: statement with no effect`,
		},
		{name: "ReferenceStatementWithNoEffect",
			input: `
			fn f() {
				let a: int
				a
			}
			`,
			fail: `4:5: statement with no effect`,
		},
		{name: "FunctionCallStatement",
			input: `
			fn f() {
				f()
			}
			`,
		},
		{name: "FunctionCallLHSAssignment",
			input: `
			fn f() {
				f() = 10
			}
			`,
			fail: `3:5: left hand side of assignment must be assignable`,
		},
		{name: "ExpressionLHSAssignment",
			input: `
			fn f() {
				let a: int
				(a == 10) = 10
			}
			`,
			fail: `4:5: left hand side of assignment must be assignable`,
		},
		{name: "Assignment",
			input: `
			fn f() {
				let a:int
				a = 10
			}
			`,
		},
		// TODO: Make this work.
		// {name: "IncrementStatement",
		// 	input: `
		// 	fn f() {
		// 		let a:int
		// 		a++
		// 	}
		// 	`},
		{name: "FieldAssignment",
			input: `
			class A {
				let a: int
			}
			
			fn f() {
				let a = new A
				a.a = 10
			}
			`},
		{name: "Call",
			input: `
				fn f(): int { return 1 }
				let v = f()
				`,
			refs: refs{
				"f": {&types.Function{ReturnType: types.Int}, nil},
				"v": {types.Var(types.Int), nil},
			},
		},
		{name: "CallInvalidNumberOfArguments",
			input: `
				fn f(a, b: int): int { return 1 }
				let v = f()
				`,
			fail: "3:14: invalid initial value for \"v\": 0 parameters provided for function that takes 2 parameters",
		},
		{name: "CallInvalidArgumentType",
			input: `
				fn f(a, b: int): int { return 1 }
				let v = f(1, "moo")
				`,
			fail: "3:18: invalid initial value for \"v\": can't coerce \"b\" from literal string to int",
		},
		{name: "VarDecl",
			input: "let a, b = 1, c, d: string",
			refs: refs{
				"a": {types.Var(types.Int), nil},
				"b": {types.Var(types.Int), nil},
				"c": {types.Var(types.String), nil},
				"d": {types.Var(types.String), nil},
			},
		},
		{name: "VarDeclUntyped",
			input: "let a, b = 1, c, d: string, e",
			fail:  "1:29: type not specified for \"e\" (and no default value provided)",
		},
		{name: "ConstantTypeMismatch",
			input: "let a: string = 1",
			fail:  "1:17: can't assign int to string",
		},
		{name: "ClassType",
			input: `class ClassType {}`,
			refs: refs{
				"ClassType": {&types.ClassType{Name: "ClassType"}, nil},
			},
		},
		{name: "ClassCreationNoInit",
			input: `
				class ClassType {}
		
				let instance = ClassType()
				`,
			refs: refs{
				"instance": {types.Var(&types.ClassType{Name: "ClassType"}), nil},
			},
		},
		{name: "ClassCreationCustomInit",
			input: `
				class ClassType {
					init(a, b: int) {
					}
				}
		
				let instance = ClassType(1, 2)
				`,
			refs: refs{
				"instance": {
					types.Var(
						&types.ClassType{
							Name: "ClassType",
							Init: &types.Function{
								Parameters: []types.NamedType{
									{Nme: "a", Typ: types.Int},
									{Nme: "b", Typ: types.Int},
								},
								ReturnType: types.None,
							},
						},
					),
					nil},
			},
		},
		{name: "ClassFields",
			input: `
					class ClassType {
						let field = 1
						fn method() {
						}
					}
				`,
			refs: refs{
				"ClassType.field":  {types.NamedType{Nme: "field", Typ: types.Int}, nil},
				"ClassType.method": {types.NamedType{Nme: "method", Typ: &types.Function{ReturnType: types.None}}, nil},
			},
		},
		{name: "EnumFields",
			input: `
					enum Enum {
						case None
						case Int(int)
		
						fn method() {}
					}
				`,
			refs: refs{
				"Enum.method": {types.NamedType{Nme: "method", Typ: &types.Function{ReturnType: types.None}}, nil},
				"Enum.None":   {types.NamedType{Nme: "None", Typ: &types.Case{Name: "None"}}, normaliseCase},
				"Enum.Int":    {types.NamedType{Nme: "Int", Typ: &types.Case{Name: "Int", Case: types.Int}}, normaliseCase},
			},
		},
		{name: "InvalidNamedReference",
			input: `
				class A {
					let b : string
				}

				fn main() {
					let a = new A
					a."foo"
				}
			`,
			fail: `8:8: invalid field reference via literal string`,
		},
		{name: "EnumCreation",
			input: `
				enum Enum {
					case None
					case Int(int)
				}
		
				let value = Enum.Int(1)
				let none = Enum.None
			`,
			refs: refs{
				"none":  {types.Var(&types.Case{Name: "None"}), normaliseCaseValue},
				"value": {types.Var(&types.Case{Name: "Int", Case: types.Int}), normaliseCaseValue},
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
			fail: "5:11: can't select case of type literal string value from int",
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
		{name: "EnumSwitchNotExhaustive",
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
		
				// let a = new Pair<string, string>()
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
				class ClassType {
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
				"a": {types.Var(types.Array(types.Int)), nil},
			}},
		{name: "ArrayLiteralExplicit",
			input: `
				let a: [int] = [1, 2, 3]
			`,
			refs: refs{
				"a": {types.Var(types.Array(types.Int)), nil},
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
			fail: `2:20: invalid initial value for "a": inconsistent element types int and literal string`,
		},
		{name: "ArrayLiteralHeterogenousLiteralNumbersInt",
			input: `
				let a = [1, 1.2]
			`,
			refs: refs{
				"a": {types.Var(types.Array(types.Int)), nil},
			},
		},
		{name: "ArrayLiteralHeterogenousLiteralNumbersFloat",
			input: `
				let a = [1.2, 1]
			`,
			refs: refs{
				"a": {types.Var(types.Array(types.Float)), nil},
			},
		},
		{name: "SetLiteral",
			input: `
				let a = {1, 2, 3}
			`,
			refs: refs{
				"a": {types.Var(types.Set(types.Int)), nil},
			}},
		{name: "SetLiteralHeterogeneous",
			input: `
				let a = {1, 2, "3"}
			`,
			fail: `2:20: invalid initial value for "a": inconsistent element types int and literal string`,
		},
		{name: "SetAndDictValuesIntermingled",
			input: `
				let a = {1, 2:3}
			`,
			fail: `2:13: invalid initial value for "a": dict value in set at index 1`,
		},
		{name: "DictLiteral",
			input: `
				let a = {1:"2", 2:"3", 3:"4"}
			`,
			refs: refs{
				"a": {types.Var(types.Map(types.Int, types.String)), nil},
			}},
		{name: "DictLiteralHeterogeneousKey",
			input: `
				let a = {1:2, 2:3, "3":4}
			`,
			fail: `2:24: invalid initial value for "a": inconsistent element types int and literal string`,
		},
		{name: "DictLiteralHeterogeneousValue",
			input: `
				let a = {1:2, 2:3, 3:"4"}
			`,
			fail: `2:26: invalid initial value for "a": inconsistent element types int and literal string`,
		},
		{name: "InvalidType",
			input: `
				let a: foo
			`,
			fail: `2:12: invalid type for "a": unknown symbol "foo"`},
		{name: "DuplicateVariable",
			input: `
				let a, a: int
			`,
			fail: `2:12: invalid variable "a": "a" redeclared`},
		{name: "ArrayType",
			input: `
				let a: [int]
			`,
			refs: refs{
				"a": ref{types.Var(types.Array(types.Int)), nil},
			},
		},
		{name: "OptionalDecl",
			input: `
				let a:int? = 1
			`,
			refs: refs{
				"a": ref{types.Var(types.Optional(types.Int)), nil},
			},
		},
		{name: "NestedEnum",
			input: `
				enum Scalar {
					case Number(float)
					case String(string)
				}

				enum ResolvedValue {
					case Scalar(Scalar)
					case List([Scalar])
					case Hash({string: Scalar})
				}
			`,
		},
		{name: "New",
			input: `
				let a = new {string:int}
				enum Enum {}
				let b = new Enum
				class ClassType {}
				let c = new ClassType
				// class Generic<T> {
				// 	let name : string
				// }
				// let d = new Generic<int>()
			`,
			refs: refs{
				"a": ref{&types.Value{Typ: types.Map(types.String, types.Int), Properties: types.Assignable}, nil},
				"b": ref{&types.Value{Typ: &types.Enum{Name: "Enum"}, Properties: types.Assignable}, nil},
				"c": ref{&types.Value{Typ: &types.ClassType{Name: "ClassType"}, Properties: types.Assignable}, nil},
				// "d": ref{&types.ResolvedValue{Typ: &types.ClassType{
				// 	TParams: []types.NamedType{
				// 		{Nme: "T"},
				// 	},
				// }}, nil},
			},
		},
		{name: "EnumUnambiguousInference",
			input: `
			enum A {
				case int(int)
				case string(string)
			}

			let a: A = "hello"
			let b: A = 1
			`},
		{name: "EnumAmbiguousInference",
			input: `
			enum A {
				case int(int)
				case float(float)
			}

			let a: A = 1
			`,
			fail: `7:15: can't assign int to enum`},
		{name: "AnonymousEnum",
			input: `
				fn func(): int|string {
					return 1
				}

				let a = func()
			`,
			refs: refs{
				"func": {&types.Function{
					ReturnType: &types.Enum{
						Flds: []types.NamedType{{
							Nme: "int",
							Typ: &types.Case{
								Name: "int",
								Case: types.Int,
							}}, {
							Nme: "string",
							Typ: &types.Case{
								Name: "string",
								Case: types.String,
							}},
						},
					}}, nil},
			},
		},
		{name: `GenericAmbiguity`,
			input: `
				class Pair<A, B> {
					let a: A
					let b: B
				}

				fn add(a, b: int): int {
					return a + b
				}

				fn main() {
					// This can either be either:
					//
					// 1. A tuple of ((Pair<string), (int>1))
					// 2. Generic instantiation of Pair<string, int>(1).
					//
					// Currently only the latter is supported.
					let a = add(1, Pair<string, int>(1))
				}
			`},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			ast, err := parser.ParseString(test.input + "\n")
			if err != nil {
				if test.fail == "" {
					require.NoError(t, err, test.input)
				} else {
					require.EqualError(t, err, test.fail, test.input)
				}
				return
			}
			program, err := Analyse(ast)
			if test.fail != "" {
				require.EqualError(t, err, test.fail, test.input)
			} else {
				require.NoError(t, err, test.input)
				if test.refs != nil {
					for key, ref := range test.refs {
						expected := ref.ref
						actual := resolve(program.Root, key)
						if ref.fix != nil {
							ref.fix(actual)
						}
						require.Equal(t, repr.String(expected, repr.Indent("  ")), repr.String(actual, repr.Indent("  ")), test.input)
					}
				}
			}
		})
	}
}

func normaliseCase(in types.Reference) {
	in.(types.NamedType).Typ.(*types.Case).Enum = nil
}

func normaliseCaseValue(in types.Reference) {
	in.(*types.Value).Typ.(*types.Case).Enum = nil
}

func resolve(root *Scope, key string) types.Reference {
	parts := strings.Split(key, ".")
	ref := root.Resolve(parts[0])
	for _, part := range parts[1:] {
		ref = types.FieldByName(ref, part)
		if ref == nil {
			return nil
		}
	}
	return ref
}
