// Package analyser
package analyser

import (
	"github.com/alecthomas/participle"
	"github.com/alecthomas/participle/lexer"

	"github.com/alecthomas/langx/parser"
	"github.com/alecthomas/langx/types"
)

var (
	builtins = &Scope{
		symbols: map[string]types.Type{
			"none":   types.Builtin(types.KindNone),
			"string": types.Builtin(types.KindString),
			"bool":   types.Builtin(types.KindBool),
			"number": types.Builtin(types.KindNumber),
			"int":    types.Builtin(types.KindInt),
			"float":  types.Builtin(types.KindFloat),
			"class":  types.Builtin(types.KindClass),
		},
	}
	noneType   = builtins.symbols["none"]
	numberType = builtins.symbols["number"]
	intType    = builtins.symbols["int"]
	floatType  = builtins.symbols["float"]
	stringType = builtins.symbols["string"]
	boolType   = builtins.symbols["bool"]
)

type Program struct {
	ast  *parser.AST
	root *Scope
}

// Analyse performs semantic analysis on the AST.
func Analyse(ast *parser.AST) (*Program, error) {
	p := &Program{
		ast:  ast,
		root: makeScope(builtins, nil),
	}
	err := checkRoot(p.root, p.ast)
	return p, err
}

func checkRoot(scope *Scope, ast *parser.AST) error {
	for _, decl := range ast.Declarations {
		switch {
		case decl.Var != nil:
			varDecl := decl.Var
			err := checkVarDecl(scope, varDecl)
			if err != nil {
				return err
			}

		case decl.Func != nil:
			fn := decl.Func
			err := checkFuncDecl(scope, fn)
			if err != nil {
				return err
			}

		default:
			panic("not implemented")
		}
	}
	return nil
}

func checkFuncDecl(scope *Scope, fn *parser.FuncDecl) error {
	var (
		returnType types.Type
		err        error
	)
	if fn.Return != nil {
		returnType, err = resolveType(scope, fn.Return)
		if err != nil {
			return err
		}
	} else {
		returnType = types.None
	}
	// Construct function type.
	fnt := &types.Function{ReturnType: returnType}
	for _, param := range fn.Parameters {
		typ, err := resolveType(scope, param.Type)
		if err != nil {
			return err
		}
		for _, name := range param.Names {
			fnt.Parameters = append(fnt.Parameters, types.Parameter{Name: name, Type: typ})
		}
	}

	err = scope.Add(fn.Name, fnt)
	if err != nil {
		return err
	}

	// Create scope and
	funcScope := scope.Sub(fnt)

	// Add parameters to scope.
	for _, param := range fn.Parameters {
		typ, err := resolveType(scope, param.Type)
		if err != nil {
			return err
		}
		err = declVars(param.Pos, funcScope, typ, param.Names...)
		if err != nil {
			return err
		}
	}

	// Check statements.
	return checkBody(funcScope, fn.Body)
}

func findEnclosingFunction(scope *Scope) *types.Function {
	if scope == nil {
		return nil
	}
	if f, ok := scope.Owner().(*types.Function); ok {
		return f
	}
	return findEnclosingFunction(scope.Parent())
}

func checkBody(scope *Scope, body *parser.Block) error {
	for _, stmt := range body.Statements {
		if err := checkStatement(scope, stmt); err != nil {
			return err
		}
	}
	return nil
}

func checkStatement(scope *Scope, stmt *parser.Stmt) error {
	switch {
	case stmt.Return != nil:
		f := findEnclosingFunction(scope)
		if f == nil {
			return participle.Errorf(stmt.Return.Pos, "can't return from outside a function")
		}
		sym, err := resolveExpr(scope, stmt.Return.Value)
		if err != nil {
			return err
		}
		if !sym.CanApply(parser.OpAsgn, f.ReturnType) {
			return participle.Errorf(stmt.Return.Pos, "cannot return %s as %s", sym.Kind(), f.ReturnType.Kind())
		}
		return nil

	case stmt.VarDecl != nil:
		return checkVarDecl(scope, stmt.VarDecl)

	case stmt.FuncDecl != nil:
		return checkFuncDecl(scope, stmt.FuncDecl)
	}
	panic("??")
}

func checkVarDecl(scope *Scope, varDecl *parser.VarDecl) error {
	var (
		typ     types.Type
		dfltTyp types.Type
		err     error
	)
	if varDecl.Default != nil {
		dfltTyp, err = resolveExpr(scope, varDecl.Default)
		if err != nil {
			return err
		}
		dfltTyp, err = types.MakeConcrete(dfltTyp)
		if err != nil {
			return participle.Errorf(varDecl.Pos, "invalid default value: %s", err)
		}
	}
	if varDecl.Type == nil {
		if dfltTyp == nil {
			return participle.Errorf(varDecl.Pos, "type not specified (and no default value provided)")
		}
		typ = dfltTyp
	} else {
		typ, err = resolveType(scope, varDecl.Type)
		if err != nil {
			return err
		}
		if dfltTyp != nil && !typ.CanApply(parser.OpAsgn, dfltTyp) {
			return participle.Errorf(varDecl.Pos, "can't assign %s to %s", dfltTyp, typ)
		}
	}
	err = declVars(varDecl.Pos, scope, typ, varDecl.Names...)
	if err != nil {
		return err
	}
	return nil
}

// Check and resolve an expression to its type.
func resolveExpr(scope *Scope, expr *parser.Expr) (types.Type, error) {
	if expr.Unary != nil {
		return resolveUnary(scope, expr.Unary)
	}
	lhs, err := resolveExpr(scope, expr.Left)
	if err != nil {
		return nil, err
	}
	rhs, err := resolveExpr(scope, expr.Right)
	if err != nil {
		return nil, err
	}
	if !lhs.CanApply(expr.Op, rhs) {
		return nil, participle.Errorf(expr.Pos, "cannot apply %s %s %s", lhs, expr.Op, rhs)
	}
	switch expr.Op {
	case parser.OpSub, parser.OpAdd, parser.OpMul, parser.OpDiv, parser.OpMod:
		return lhs, nil

	case parser.OpLe, parser.OpLt, parser.OpGe, parser.OpGt, parser.OpEq, parser.OpNe:
		return types.Bool, nil
	}
	panic(expr.Pos.String())
}

func resolveUnary(s *Scope, unary *parser.Unary) (types.Type, error) {
	sym, err := resolveTerminal(s, unary.Terminal)
	if err != nil {
		return nil, err
	}
	if sym.CanApply(parser.OpNot, types.None) {
		return nil, participle.Errorf(unary.Pos, "! requires a boolean but got %s", sym.Kind())
	}
	// if unary.Op == parser.OpSub && !sym.Kind().IsArithmetic() {
	// 	return nil, participle.Errorf(unary.Pos, "- cannot be used with %s", sym.Kind())
	// }
	return resolveType(s, unary.Terminal)
}

func resolveType(scope *Scope, terminal *parser.Terminal) (types.Type, error) {
	ref, err := resolveTerminal(scope, terminal)
	if err != nil {
		return nil, err
	}
	typ, ok := ref.(types.Type)
	if !ok {
		return nil, participle.Errorf(terminal.Pos, "not a type")
	}
	return typ, nil
}

func resolveTerminal(scope *Scope, terminal *parser.Terminal) (types.Type, error) {
	switch {
	case terminal.Literal != nil:
		return resolveLiteral(terminal.Literal)

	case terminal.Ident != "":
		sym := scope.Resolve(terminal.Ident)
		if sym == nil {
			return nil, participle.Errorf(terminal.Pos, "unknown symbol %q", terminal.Ident)
		}
		switch {
		case terminal.Reference != nil:
			return resolveField(scope, sym, terminal.Reference)

		case terminal.Call != nil:
			if sym.Kind() != types.KindFunc {
				return nil, participle.Errorf(terminal.Call.Pos, "can't call non-function %q of type %q", terminal.Ident, sym.Kind())
			}
			return resolveCall(scope, sym.(*types.Function), terminal.Call)
		}
		return sym, nil

	case terminal.Tuple != nil:
		if len(terminal.Tuple) == 1 {
			return resolveExpr(scope, terminal.Tuple[0])
		}
		fallthrough

	default:
		return nil, participle.Errorf(terminal.Pos, "can't resolve %s", terminal.Describe())
	}
}

func resolveCall(scope *Scope, fnt *types.Function, call *parser.Call) (types.Type, error) {
	if len(fnt.Parameters) != len(call.Parameters) {
		return nil, participle.Errorf(call.Pos,
			"%d parameters provided for function that takes %d parameters",
			len(call.Parameters), len(fnt.Parameters))
	}
	for i, param := range call.Parameters {
		ptype, err := resolveExpr(scope, param)
		if err != nil {
			return nil, err
		}
		parameter := fnt.Parameters[i]
		if !ptype.CoercibleTo(parameter.Type) {
			return nil, participle.Errorf(param.Pos, "%s: can't coerce parameter %q from %s to %s",
				fnt, parameter.Name, ptype.Kind(), parameter.Type)
		}
	}
	return fnt.ReturnType, nil
}

func resolveField(scope *Scope, parent types.Type, terminal *parser.Terminal) (types.Type, error) {
	switch {
	case terminal.Ident != "":
		field := types.FieldByName(parent, terminal.Ident)
		if field == nil {
			return nil, participle.Errorf(terminal.Pos, "unknown field %s.%s", parent, terminal.Ident)
		}
		// Follow field references.
		if terminal.Reference != nil {
			return resolveField(scope, field.Type, terminal.Reference)
		}
		return field.Type, nil

	default:
		return nil, participle.Errorf(terminal.Pos, "invalid field reference %s", terminal.Describe())
	}
}

func resolveLiteral(literal *parser.Literal) (types.Type, error) {
	switch {
	case literal.Number != nil:
		return types.Number, nil

	case literal.Str != nil:
		return types.String, nil

	case literal.Bool != nil:
		return types.Bool, nil

	default:
		panic("??")
	}
}

// Declare vars of typ in scope.
func declVars(pos lexer.Position, scope *Scope, typ types.Type, names ...string) error {
	if typ == nil {
		return participle.Errorf(pos, "no type provided (type inference not implemented yet)")
	}
	for _, name := range names {
		err := scope.Add(name, typ)
		if err != nil {
			return participle.Errorf(pos, "%s", err)
		}
	}
	return nil
}
