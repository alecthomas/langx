// Package analyser
package analyser

import (
	"github.com/alecthomas/participle"
	"github.com/alecthomas/participle/lexer"
	"github.com/alecthomas/repr"

	"github.com/alecthomas/langx/parser"
	"github.com/alecthomas/langx/types"
)

var (
	builtins = &Scope{
		symbols: map[string]types.Reference{
			"string": types.String,
			"bool":   types.Bool,
			"int":    types.Int,
			"float":  types.Float,
		},
	}
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

type funcAndScope struct {
	fn    *parser.Block
	scope *Scope
}

func checkRoot(scope *Scope, ast *parser.AST) error {
	// TODO: Accumulate function bodies across all classes/enums and globals,
	//  and do them all in one pass.
	funcs := []funcAndScope{}
	for _, decl := range ast.Declarations {
		switch {
		case decl.Var != nil:
			if err := checkVarDecl(scope, decl.Var); err != nil {
				return err
			}

		case decl.Func != nil:
			funcScope, err := checkFuncDecl(scope, decl.Func)
			if err != nil {
				return err
			}
			funcs = append(funcs, funcAndScope{decl.Func.Body, funcScope})

		case decl.Class != nil:
			if err := checkClassDecl(scope, decl.Class); err != nil {
				return err
			}

		case decl.Enum != nil:
			if err := checkEnumDecl(scope, decl.Enum); err != nil {
				return err
			}

		default:
			panic("not implemented")
		}
	}
	return checkFuncScopes(funcs)
}

func checkFuncScopes(funcs []funcAndScope) error {
	for _, fn := range funcs {
		if err := checkBlock(fn.scope, fn.fn); err != nil {
			return err
		}
	}
	return nil
}

func checkEnumDecl(scope *Scope, enum *parser.EnumDecl) error {
	// Add the enum to the parent scope.
	enumt := &types.Enum{}
	err := scope.AddType(enum.Name.Type, enumt)
	if err != nil {
		return participle.Errorf(enum.Pos, "%s", err)
	}
	// Intermediate scope for "self" (so we don't add it to the set of fields).
	enumScope := scope.Sub(enumt)
	err = enumScope.AddValue("self", &types.Value{Typ: enumt})
	if err != nil {
		return participle.Errorf(enum.Pos, "%s", err)
	}
	// Create a sub-scope for all the fields.
	enumScope = enumScope.Sub(nil)
	funcScopes := []funcAndScope{}
	for _, member := range enum.Members {
		switch {
		case member.CaseDecl != nil:
			if err := resolveCaseDecl(enumScope, enumt, member.CaseDecl); err != nil {
				return err
			}

		case member.VarDecl != nil:
			if err := checkVarDecl(enumScope, member.VarDecl); err != nil {
				return err
			}

		case member.FuncDecl != nil:
			funcScope, err := checkFuncDecl(enumScope, member.FuncDecl)
			if err != nil {
				return err
			}
			funcScopes = append(funcScopes, funcAndScope{member.FuncDecl.Body, funcScope})

		case member.EnumDecl != nil:
			if err := checkEnumDecl(enumScope, member.EnumDecl); err != nil {
				return err
			}

		case member.ClassDecl != nil:
			if err := checkClassDecl(enumScope, member.ClassDecl); err != nil {
				return err
			}

		case member.InitialiserDecl != nil:
			initScope, init, err := resolveInitialiserDecl(enumScope, member.InitialiserDecl)
			if err != nil {
				return err
			}
			enumt.Init = init
			funcScopes = append(funcScopes, funcAndScope{member.InitialiserDecl.Body, initScope})
		}
	}
	enumt.Flds = scopeToTypeFields(enumScope)
	return checkFuncScopes(funcScopes)
}

func scopeToTypeFields(scope *Scope) []types.TypeField {
	symbols := scope.Symbols()
	var out []types.TypeField
	for name, sym := range symbols {
		out = append(out, types.TypeField{
			Name: name,
			Type: sym.Type(),
		})
	}
	return out
}

func resolveCaseDecl(scope *Scope, enum *types.Enum, decl *parser.CaseDecl) error {
	ctype := &types.Case{
		Enum: enum,
		Name: decl.Name,
	}
	if decl.Type != nil {
		typ, err := resolveType(scope, decl.Type)
		if err != nil {
			return err
		}
		ctype.Case = typ
	}
	err := scope.AddType(decl.Name, ctype)
	if err != nil {
		return participle.Errorf(decl.Pos, "%s", err)
	}
	return nil
}

func resolveType(scope *Scope, cse *parser.Type) (types.Type, error) {
	typ := scope.ResolveType(cse.Type)
	if typ == nil {
		return nil, participle.Errorf(cse.Pos, "unknown type %q", cse.Type)
	}
	return typ, nil
}

func checkClassDecl(scope *Scope, class *parser.ClassDecl) error {
	clst := &types.Class{}
	err := scope.AddType(class.Name.Type, clst)
	if err != nil {
		return participle.Errorf(class.Pos, "%s", err)
	}
	// Intermediate scope for "self" (so we don't add it to the set of fields).
	classScope := scope.Sub(clst)
	err = classScope.AddValue("self", &types.Value{Typ: clst})
	if err != nil {
		return participle.Errorf(class.Pos, "%s", err)
	}
	// Create a sub-scope for all the fields.
	classScope = classScope.Sub(nil)
	funcScopes := []funcAndScope{}
	for _, member := range class.Members {
		switch {
		case member.VarDecl != nil:
			if err := checkVarDecl(classScope, member.VarDecl); err != nil {
				return err
			}

		case member.FuncDecl != nil:
			funcScope, err := checkFuncDecl(classScope, member.FuncDecl)
			if err != nil {
				return err
			}
			funcScopes = append(funcScopes, funcAndScope{member.FuncDecl.Body, funcScope})

		case member.EnumDecl != nil:
			if err := checkEnumDecl(classScope, member.EnumDecl); err != nil {
				return err
			}

		case member.ClassDecl != nil:
			if err := checkClassDecl(classScope, member.ClassDecl); err != nil {
				return err
			}

		case member.InitialiserDecl != nil:
			initScope, init, err := resolveInitialiserDecl(classScope, member.InitialiserDecl)
			if err != nil {
				return err
			}
			clst.Init = init
			funcScopes = append(funcScopes, funcAndScope{member.InitialiserDecl.Body, initScope})
		}
	}
	clst.Flds = scopeToTypeFields(classScope)
	return checkFuncScopes(funcScopes)
}

func resolveInitialiserDecl(scope *Scope, decl *parser.InitialiserDecl) (*Scope, *types.Function, error) {
	fnt, err := makeFunction(scope, types.None, decl.Parameters)
	if err != nil {
		return nil, nil, err
	}
	funcScope := scope.Sub(fnt)
	err = addParametersToScope(funcScope, decl.Parameters)
	if err != nil {
		return nil, nil, err
	}
	return funcScope, fnt, nil
}

// Check function declaration (but not body).
//
// We pre-declare all symbols in a first pass before checking the function bodies in a
// second pass.
func checkFuncDecl(scope *Scope, fn *parser.FuncDecl) (*Scope, error) {
	var (
		returnType types.Type
		err        error
	)
	if fn.Return != nil {
		returnType, err = resolveTerminalType(scope, fn.Return)
		if err != nil {
			return nil, err
		}
	} else {
		returnType = types.None
	}
	// Construct function type.
	fnt, err := makeFunction(scope, returnType, fn.Parameters)
	if err != nil {
		return nil, err
	}

	err = scope.AddType(fn.Name, fnt)
	if err != nil {
		return nil, err
	}

	// Create scope and add parameters to it.
	funcScope := scope.Sub(fnt)

	err = addParametersToScope(funcScope, fn.Parameters)
	if err != nil {
		return nil, err
	}

	return funcScope, nil
}

func addParametersToScope(scope *Scope, parameters []*parser.Parameters) error {
	// Add parameters to scope.
	for _, param := range parameters {
		typ, err := resolveTerminalType(scope.Parent(), param.Type)
		if err != nil {
			return err
		}
		err = declVars(param.Pos, scope, &types.Value{Typ: typ}, param.Names...)
		if err != nil {
			return err
		}
	}
	return nil
}

func makeFunction(scope *Scope, returnType types.Type, parameters []*parser.Parameters) (*types.Function, error) {
	fnt := &types.Function{ReturnType: returnType}
	for _, param := range parameters {
		typ, err := resolveTerminalType(scope, param.Type)
		if err != nil {
			return nil, err
		}
		for _, name := range param.Names {
			fnt.Parameters = append(fnt.Parameters, types.Parameter{Name: name, Type: typ})
		}
	}
	return fnt, nil
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

func checkStatement(scope *Scope, stmt *parser.Stmt) error {
	if stmt == nil {
		return nil
	}
	switch {
	case stmt.Return != nil:
		f := findEnclosingFunction(scope)
		if f == nil {
			return participle.Errorf(stmt.Return.Pos, "can't return from outside a function")
		}
		val, err := resolveExprValue(scope, stmt.Return.Value)
		if err != nil {
			return err
		}
		if !val.Type().CoercibleTo(f.ReturnType) {
			return participle.Errorf(stmt.Return.Pos, "cannot return %s as %s", val.Kind(), f.ReturnType.Kind())
		}
		return nil

	case stmt.VarDecl != nil:
		return checkVarDecl(scope, stmt.VarDecl)

	case stmt.FuncDecl != nil:
		funcScope, err := checkFuncDecl(scope, stmt.FuncDecl)
		if err != nil {
			return err
		}
		return checkBlock(funcScope, stmt.FuncDecl.Body)

	case stmt.If != nil:
		stmt := stmt.If
		if err := checkBoolExpr(scope, stmt.Condition); err != nil {
			return err
		}
		if err := checkStatement(scope.Sub(nil), stmt.Main); err != nil {
			return err
		}
		return checkStatement(scope.Sub(nil), stmt.Else)

	case stmt.Go != nil:
		if stmt.Go.Call == nil {
			return participle.Errorf(stmt.Go.Pos, "go requires a function call")
		}
		_, err := resolveTerminal(scope, stmt.Go.Call)
		return err

	case stmt.Switch != nil:
		return checkSwitch(scope, stmt.Switch)

	case stmt.Expression != nil:
		_, err := resolveExpr(scope, stmt.Expression)
		return err

	case stmt.Block != nil:
		return checkBlock(scope.Sub(nil), stmt.Block)
	}
	panic(stmt.Pos.String())
}

func checkSwitch(scope *Scope, stmt *parser.SwitchStmt) error {
	target, err := resolveExprValue(scope, stmt.Target)
	if err != nil {
		return err
	}
	switch target := target.Type().(type) {
	case *types.Case:
		panic("??")
		// defaulted := false
		// seen := map[string]*types.Enum{}
		// cases := casesForEnum(target.Enum)
		// for _, cse := range stmt.Cases {
		// 	if cse.Default {
		// 		if defaulted {
		// 			return participle.Errorf(cse.Pos, "duplicate default case")
		// 		}
		// 		defaulted = true
		// 	} else {
		// 	}
		// }

	default:
		return checkSwitchOnValue(scope, target, stmt)
	}
	return nil
}

func checkSwitchOnValue(scope *Scope, target types.Type, stmt *parser.SwitchStmt) error {
	for _, cse := range stmt.Cases {
		if cse.Case != nil {
			resolvedCase, err := resolveExpr(scope, cse.Case)
			if err != nil {
				return err
			}
			if !resolvedCase.Type().CoercibleTo(target.Type()) {
				return participle.Errorf(cse.Case.Pos, "can't select case of type %s from %s", resolvedCase, target)
			}
		}
		for _, stmt := range cse.Body {
			if err := checkStatement(scope, stmt); err != nil {
				return err
			}
		}
	}
	return nil
}

func casesForEnum(t types.Type) map[string]*types.Case {
	out := map[string]*types.Case{}
	for _, f := range t.Fields() {
		c, ok := f.Type.(*types.Case)
		if !ok {
			continue
		}
		out[f.Name] = c
	}
	return out
}

func checkBoolExpr(scope *Scope, cond *parser.Expr) error {
	condition, err := resolveExprValue(scope, cond)
	if err != nil {
		return err
	}
	if condition.Kind() != types.KindBool {
		return participle.Errorf(cond.Pos, "condition must be bool but is %s", condition)
	}
	return nil
}

func checkBlock(scope *Scope, block *parser.Block) error {
	if block == nil {
		return nil
	}
	for _, stmt := range block.Statements {
		if err := checkStatement(scope, stmt); err != nil {
			return err
		}
	}
	return nil
}

func checkVarDecl(scope *Scope, varDecl *parser.VarDecl) error {
	var untyped []string
	for _, decl := range varDecl.Vars {
		untyped = append(untyped, decl.Name)
		if decl.Type == nil && decl.Default == nil {
			continue
		}

		var (
			typ     types.Type
			dfltTyp types.Type
			err     error
		)

		if decl.Default != nil {
			dfltValue, err := resolveExprValue(scope, decl.Default)
			if err != nil {
				return err
			}
			dfltTyp, err = types.MakeConcrete(dfltValue.Type())
			if err != nil {
				return participle.Errorf(decl.Default.Pos, "invalid default value: %s", err)
			}
		}
		if decl.Type == nil {
			if dfltTyp == nil {
				return participle.Errorf(decl.Pos, "type not specified (and no default value provided)")
			}
			// Infer type from the default value.
			typ = dfltTyp
		} else {
			typ, err = resolveTerminalType(scope, decl.Type)
			if err != nil {
				return err
			}
			if dfltTyp != nil && !typ.CanApply(parser.OpAsgn, dfltTyp) {
				return participle.Errorf(decl.Default.Pos, "can't assign %s to %s", dfltTyp, typ)
			}
		}
		err = declVars(varDecl.Pos, scope, &types.Value{Typ: typ}, untyped...)
		if err != nil {
			return err
		}
		untyped = nil
	}
	if len(untyped) > 0 {
		return participle.Errorf(varDecl.Vars[len(varDecl.Vars)-1].Pos, "type not specified (and no default value provided)")
	}
	return nil
}

func resolveExprValue(scope *Scope, expr *parser.Expr) (*types.Value, error) {
	ref, err := resolveExpr(scope, expr)
	if err != nil {
		return nil, err
	}
	switch ref := ref.(type) {
	case *types.Value:
		return ref, nil

	case *types.Case:
		return &types.Value{Typ: ref}, nil

	case types.Field:
		return ref.Value, nil

	default:
		return nil, participle.Errorf(expr.Pos, "expected a value but got %s", ref)
	}
}

func resolveExprType(scope *Scope, expr *parser.Expr) (types.Type, error) {
	ref, err := resolveExpr(scope, expr)
	if err != nil {
		return nil, err
	}
	typ, ok := ref.(types.Type)
	if !ok {
		return nil, participle.Errorf(expr.Pos, "expected a type but got a %s value", ref.Kind())
	}
	return typ, nil
}

// Check and resolve an expression to its type.
func resolveExpr(scope *Scope, expr *parser.Expr) (types.Reference, error) {
	if expr.Unary != nil {
		return resolveUnary(scope, expr.Unary)
	}
	lhs, err := resolveExprValue(scope, expr.Left)
	if err != nil {
		return nil, err
	}
	rhs, err := resolveExprValue(scope, expr.Right)
	if err != nil {
		return nil, err
	}
	if !lhs.Type().CanApply(expr.Op, rhs.Type()) {
		return nil, participle.Errorf(expr.Pos, "cannot apply %s %s %s", lhs, expr.Op, rhs)
	}
	switch expr.Op {
	case parser.OpSub, parser.OpAdd, parser.OpMul, parser.OpDiv, parser.OpMod,
		parser.OpAsgn, parser.OpMulAsgn, parser.OpSubAsgn, parser.OpAddAsgn,
		parser.OpDivAsgn, parser.OpModAsgn:
		return lhs, nil

	case parser.OpLe, parser.OpLt, parser.OpGe, parser.OpGt, parser.OpEq, parser.OpNe:
		return &types.Value{Typ: types.Bool}, nil
	}
	panic(expr.Pos.String())
}

func resolveUnary(s *Scope, unary *parser.Unary) (types.Reference, error) {
	sym, err := resolveTerminal(s, unary.Terminal)
	if err != nil {
		return nil, err
	}
	if sym.Type().CanApply(unary.Op, types.None) {
		return nil, participle.Errorf(unary.Pos, "! requires a boolean but got %s", sym.Kind())
	}
	return resolveTerminal(s, unary.Terminal)
}

func resolveTerminalType(scope *Scope, terminal *parser.Terminal) (types.Type, error) {
	ref, err := resolveTerminal(scope, terminal)
	if err != nil {
		return nil, err
	}
	val, ok := ref.(types.Type)
	if !ok {
		return nil, participle.Errorf(terminal.Pos, "expected a type but got %s value", ref.Kind())
	}
	return val, nil
}

func resolveTerminalValue(scope *Scope, terminal *parser.Terminal) (*types.Value, error) {
	ref, err := resolveTerminal(scope, terminal)
	if err != nil {
		return nil, err
	}
	val, ok := ref.(*types.Value)
	if !ok {
		return nil, participle.Errorf(terminal.Pos, "expected a value but got %s", ref.Kind())
	}
	return val, nil
}

func resolveTerminal(scope *Scope, terminal *parser.Terminal) (types.Reference, error) {
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
			return resolveCallLike(scope, sym, terminal)

		case terminal.Subscript != nil:
			panic(terminal.Subscript.Pos.String())
		}
		return sym, nil

	case terminal.Tuple != nil:
		// Sub-expression.
		if len(terminal.Tuple) == 1 {
			return resolveExpr(scope, terminal.Tuple[0])
		}
		return nil, participle.Errorf(terminal.Pos, "tuples are not supported yet")

	default:
		return nil, participle.Errorf(terminal.Pos, "can't resolve %s", terminal.Describe())
	}
}

// Resolve something that looks like a function call (function, case, class initialiser).
func resolveCallLike(scope *Scope, ref types.Reference, terminal *parser.Terminal) (*types.Value, error) {
	switch ref := ref.(type) {
	case *types.Case: // Case(Type)
		if ref.Case == nil {
			return nil, participle.Errorf(terminal.Call.Pos, "untyped case %q should not be called", terminal.Ident)
		}
		// Synthesise case parameters.
		// TODO: Add "Cases" to Enum?
		parameters := []types.Parameter{{Type: ref.Case}}
		_, err := resolveCallActual(scope, ref.Case, parameters, terminal.Call)
		if err != nil {
			return nil, err
		}
		return &types.Value{Typ: ref}, nil

	case *types.Class:
		var parameters []types.Parameter
		if ref.Init != nil {
			parameters = ref.Init.Parameters
		}
		return resolveCallActual(scope, ref, parameters, terminal.Call)

	case *types.Enum:
		var parameters []types.Parameter
		if ref.Init != nil {
			parameters = ref.Init.Parameters
		}
		return resolveCallActual(scope, ref, parameters, terminal.Call)

	case *types.Function:
		return resolveCallActual(scope, ref.ReturnType, ref.Parameters, terminal.Call)

	default:
		return nil, participle.Errorf(terminal.Call.Pos, "can't call %s", ref)
	}
}

func resolveCallActual(scope *Scope, returnType types.Type, parameters []types.Parameter, call *parser.Call) (*types.Value, error) {
	if len(parameters) != len(call.Parameters) {
		return nil, participle.Errorf(call.Pos,
			"%d parameters provided for function that takes %d parameters",
			len(call.Parameters), len(parameters))
	}
	for i, param := range call.Parameters {
		value, err := resolveExprValue(scope, param)
		if err != nil {
			return nil, err
		}
		parameter := parameters[i]
		if !value.Type().CoercibleTo(parameter.Type) {
			return nil, participle.Errorf(param.Pos, "can't coerce parameter %q from %s to %s",
				parameter.Name, value.Kind(), parameter.Type)
		}
	}
	return &types.Value{Typ: returnType}, nil
}

func resolveField(scope *Scope, parent types.Reference, terminal *parser.Terminal) (types.Reference, error) {
	switch {
	case terminal.Ident != "":
		field := parent.FieldByName(terminal.Ident)
		if field == nil {
			return nil, participle.Errorf(terminal.Pos, "unknown field %s.%s", parent, terminal.Ident)
		}
		switch {
		case terminal.Reference != nil:
			return resolveField(scope, field, terminal.Reference)

		case terminal.Call != nil:
			return resolveCallLike(scope, field, terminal)

		case terminal.Subscript != nil:
			panic(terminal.Subscript.Pos.String())
		}
		return field, nil

	default:
		return nil, participle.Errorf(terminal.Pos, "invalid field reference %s", terminal.Describe())
	}
}

func resolveLiteral(literal *parser.Literal) (types.Reference, error) {
	switch {
	case literal.Number != nil:
		return &types.Value{Typ: types.Number}, nil

	case literal.Str != nil:
		// TODO: Resolve interpolation vars eg. "\(x), \(y), \(z)".
		return &types.Value{Typ: types.String}, nil

	case literal.Bool != nil:
		return &types.Value{Typ: types.Bool}, nil

	default:
		panic(repr.String(literal))
	}
}

// Declare vars of typ in scope.
func declVars(pos lexer.Position, scope *Scope, value *types.Value, names ...string) error {
	if value == nil {
		return participle.Errorf(pos, "no type provided (type inference not implemented yet)")
	}
	for _, name := range names {
		err := scope.AddValue(name, value)
		if err != nil {
			return participle.Errorf(pos, "%s", err)
		}
	}
	return nil
}
