package codegen

import (
	"strings"
	"testing"

	"github.com/alecthomas/assert/v2"
	"github.com/alecthomas/langx/analyser"
	"github.com/alecthomas/langx/parser"
)

func TestCodeGen(t *testing.T) {
	tests := []struct {
		name   string
		input  string
		output string
	}{
		{name: "FunctionWithReturn",
			input: `
				fn add(a, b: int): int {
					return a + b
				}
			`,
			output: `
(module
  (memory (export "memory") 1)
  (func
    $add
    (export "add")
    (param $a i64)
    (param $b i64)
    (result i64)
    (i64.add (local.get $a) (local.get $b))
    return))
		`},
		{name: "StringData",
			input: `
				let a = "Hello {user.name} how are you?"
				let b = "World"
			`,
			output: `
(module
  (memory (export "memory") 1)
  (data (i32.const 0) "Hello ")
  (data (i32.const 6) " how are you?")
  (data (i32.const 19) "World"))
		`},
		{name: `If`,
			input: `
				fn f(n: int): bool {
					if 1 + 2 > n {
						return true
					}
					return false
				}`,
			output: `
(module
  (memory (export "memory") 1)
  (func
    $f
    (export "f")
    (param $n i64)
    (result i32)
    block
    $_333a36811c9dc5
    (i64.gt_s (i64.add (i64.const 1) (i64.const 2)) (local.get $n))
    br_if
    $_333a36811c9dc5
    (i32.const 1)
    return
    end
    $_333a36811c9dc5
    (i32.const 0)
    return))
`},
		// {name: "Class",
		// 	input: `
		// 		class A {
		// 			let a: int
		// 			let b: int
		// 		}
		// 		class C {
		// 			let a: int
		// 			let b: string
		// 			let C: A
		// 		}
		//
		// 		let c = C()
		// 	`},
		{name: "Fibonacci",
			input: `
				fn fib(n: int): int {
					if n <= 1 {
						return n
					}
					return fib(n-1) + fib(n-2);
				}
			`},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			ast, err := parser.Parse(strings.NewReader(test.input))
			assert.NoError(t, err, test.input)
			program, err := analyser.Analyse(ast)
			assert.NoError(t, err, test.input)
			w := &strings.Builder{}
			err = Generate(w, program)
			assert.NoError(t, err, test.input)
			assert.Equal(t, strings.TrimSpace(test.output), strings.TrimSpace(w.String()))
			t.Log(test.input)
			t.Log("\n" + w.String())
		})
	}
}
