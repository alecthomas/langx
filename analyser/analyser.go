// Package analyser
package analyser

import (
	"strings"

	"github.com/alecthomas/participle"
	"github.com/alecthomas/participle/lexer"

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
	return p, new(analyser).checkRoot(p.root, p.ast)
}

type funcAndScope struct {
	fn    *parser.Block
	scope *Scope
}

type analyser struct {
	funcs []funcAndScope
}

func (a *analyser) deferFunc(fn *parser.Block, scope *Scope) {
	a.funcs = append(a.funcs, funcAndScope{fn, scope})
}

func (a *analyser) checkRoot(scope *Scope, ast *parser.AST) error {
	// TODO: Accumulate function bodies across all classes/enums and globals,
	//  and do them all in one pass.
	for _, decl := range ast.Declarations {
		switch {
		case decl.Var != nil:
			if err := a.checkVarDecl(scope, decl.Var); err != nil {
				return err
			}

		case decl.Func != nil:
			funcScope, err := a.checkFuncDecl(scope, decl.Func)
			if err != nil {
				return err
			}
			a.deferFunc(decl.Func.Body, funcScope)

		case decl.Class != nil:
			if err := a.checkClassDecl(scope, decl.Class); err != nil {
				return err
			}

		case decl.Enum != nil:
			if err := a.checkEnumDecl(scope, decl.Enum); err != nil {
				return err
			}

		default:
			panic("not implemented")
		}
	}
	return a.checkFuncScopes()
}

func (a *analyser) checkFuncScopes() error {
	for _, fn := range a.funcs {
		if err := a.checkBlock(fn.scope, fn.fn); err != nil {
			return err
		}
	}
	return nil
}

func (a *analyser) checkEnumDecl(scope *Scope, enum *parser.EnumDecl) error {
	// Add the enum to the parent scope.
	enumt := &types.Enum{}
	err := scope.AddType(enum.Type.Type, enumt)
	if err != nil {
		return participle.Errorf(enum.Pos, "%s", err)
	}
	// Intermediate scope for "self" (so we don't add it to the set of fields).
	enumScope := scope.Sub(enumt)
	err = enumScope.AddValue("self", &types.Value{Typ: enumt})
	if err != nil {
		return participle.Errorf(enum.Pos, "%s", err)
	}

	// Add generics to the enum scope.
	t := enum.Type
	err = a.declGenericParameters(enumScope, t)
	if err != nil {
		return err
	}

	// Create a sub-scope for all the fields.
	enumScope = enumScope.Sub(nil)
	for _, member := range enum.Members {
		switch {
		case member.CaseDecl != nil:
			if err := a.resolveCaseDecl(enumScope, enumt, member.CaseDecl); err != nil {
				return err
			}

		case member.VarDecl != nil:
			if err := a.checkVarDecl(enumScope, member.VarDecl); err != nil {
				return err
			}

		case member.FuncDecl != nil:
			funcScope, err := a.checkFuncDecl(enumScope, member.FuncDecl)
			if err != nil {
				return err
			}
			a.deferFunc(member.FuncDecl.Body, funcScope)

		case member.EnumDecl != nil:
			if err := a.checkEnumDecl(enumScope, member.EnumDecl); err != nil {
				return err
			}

		case member.ClassDecl != nil:
			if err := a.checkClassDecl(enumScope, member.ClassDecl); err != nil {
				return err
			}

		case member.InitialiserDecl != nil:
			initScope, init, err := a.resolveInitialiserDecl(enumScope, member.InitialiserDecl)
			if err != nil {
				return err
			}
			enumt.Init = init
			a.deferFunc(member.InitialiserDecl.Body, initScope)
		}
	}
	enumt.Flds = a.scopeToTypeFields(enumScope)
	return nil
}

func (a *analyser) declGenericParameters(scope *Scope, t *parser.Type) error {
	for _, gp := range t.Generics {
		err := scope.AddType(gp.Name, types.Generic{})
		if err != nil {
			return participle.AnnotateError(t.Pos, err)
		}
	}
	return nil
}

func (a *analyser) scopeToTypeFields(scope *Scope) []types.TypeField {
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

func (a *analyser) resolveCaseDecl(scope *Scope, enum *types.Enum, decl *parser.CaseDecl) error {
	ctype := &types.Case{
		Enum: enum,
		Name: decl.Name,
	}
	if decl.Type != nil {
		typ, err := a.resolveType(scope, decl.Type)
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

func (a *analyser) resolveType(scope *Scope, cse *parser.Type) (types.Type, error) {
	typ := scope.ResolveType(cse.Type)
	if typ == nil {
		return nil, participle.Errorf(cse.Pos, "unknown type %q", cse.Type)
	}
	return typ, nil
}

func (a *analyser) checkClassDecl(scope *Scope, class *parser.ClassDecl) error {
	clst := &types.Class{}
	err := scope.AddType(class.Type.Type, clst)
	if err != nil {
		return participle.Errorf(class.Pos, "%s", err)
	}
	// Intermediate scope for "self" (so we don't add it to the set of fields).
	classScope := scope.Sub(clst)
	err = classScope.AddValue("self", &types.Value{Typ: clst})
	if err != nil {
		return participle.AnnotateError(class.Pos, err)
	}
	err = a.declGenericParameters(classScope, class.Type)
	if err != nil {
		return err
	}

	// Create a sub-scope for all the fields.
	classScope = classScope.Sub(nil)
	for _, member := range class.Members {
		switch {
		case member.VarDecl != nil:
			if err := a.checkVarDecl(classScope, member.VarDecl); err != nil {
				return err
			}

		case member.FuncDecl != nil:
			funcScope, err := a.checkFuncDecl(classScope, member.FuncDecl)
			if err != nil {
				return err
			}
			a.deferFunc(member.FuncDecl.Body, funcScope)

		case member.EnumDecl != nil:
			if err := a.checkEnumDecl(classScope, member.EnumDecl); err != nil {
				return err
			}

		case member.ClassDecl != nil:
			if err := a.checkClassDecl(classScope, member.ClassDecl); err != nil {
				return err
			}

		case member.InitialiserDecl != nil:
			initScope, init, err := a.resolveInitialiserDecl(classScope, member.InitialiserDecl)
			if err != nil {
				return err
			}
			clst.Init = init
			a.deferFunc(member.InitialiserDecl.Body, initScope)
		}
	}
	clst.Flds = a.scopeToTypeFields(classScope)
	return nil
}

func (a *analyser) resolveInitialiserDecl(scope *Scope, decl *parser.InitialiserDecl) (*Scope, *types.Function, error) {
	fnt, err := a.makeFunction(scope, types.None, decl.Parameters)
	if err != nil {
		return nil, nil, err
	}
	funcScope := scope.Sub(fnt)
	err = a.addParametersToScope(funcScope, decl.Parameters)
	if err != nil {
		return nil, nil, err
	}
	return funcScope, fnt, nil
}

// Check function declaration (but not body).
//
// We pre-declare all symbols in a first pass before checking the function bodies in a
// second pass.
func (a *analyser) checkFuncDecl(scope *Scope, fn *parser.FuncDecl) (*Scope, error) {
	var (
		returnType types.Type
		err        error
	)
	if fn.Return != nil {
		returnType, err = a.resolveTerminalType(scope, fn.Return)
		if err != nil {
			return nil, err
		}
	} else {
		returnType = types.None
	}
	// Construct function type.
	fnt, err := a.makeFunction(scope, returnType, fn.Parameters)
	if err != nil {
		return nil, err
	}

	err = scope.AddType(fn.Name, fnt)
	if err != nil {
		return nil, err
	}

	// Create scope and add parameters to it.
	funcScope := scope.Sub(fnt)

	err = a.addParametersToScope(funcScope, fn.Parameters)
	if err != nil {
		return nil, err
	}

	return funcScope, nil
}

func (a *analyser) addParametersToScope(scope *Scope, parameters []*parser.Parameters) error {
	// Add parameters to scope.
	for _, param := range parameters {
		typ, err := a.resolveTerminalType(scope.Parent(), param.Type)
		if err != nil {
			return err
		}
		err = a.declVars(param.Pos, scope, &types.Value{Typ: typ}, param.Names...)
		if err != nil {
			return err
		}
	}
	return nil
}

func (a *analyser) makeFunction(scope *Scope, returnType types.Type, parameters []*parser.Parameters) (*types.Function, error) {
	fnt := &types.Function{ReturnType: returnType}
	for _, param := range parameters {
		typ, err := a.resolveTerminalType(scope, param.Type)
		if err != nil {
			return nil, err
		}
		for _, name := range param.Names {
			fnt.Parameters = append(fnt.Parameters, types.Parameter{Name: name, Type: typ})
		}
	}
	return fnt, nil
}

func (a *analyser) findEnclosingFunction(scope *Scope) *types.Function {
	if scope == nil {
		return nil
	}
	if f, ok := scope.Owner().(*types.Function); ok {
		return f
	}
	return a.findEnclosingFunction(scope.Parent())
}

func (a *analyser) checkStatement(scope *Scope, stmt *parser.Stmt) error {
	if stmt == nil {
		return nil
	}
	switch {
	case stmt.Return != nil:
		f := a.findEnclosingFunction(scope)
		if f == nil {
			return participle.Errorf(stmt.Return.Pos, "can't return from outside a function")
		}
		val, err := a.resolveExprValue(scope, stmt.Return.Value)
		if err != nil {
			return err
		}
		if val.Type().Coerce(f.ReturnType) == nil {
			return participle.Errorf(stmt.Return.Pos, "cannot return %s as %s", val.Kind(), f.ReturnType.Kind())
		}
		return nil

	case stmt.VarDecl != nil:
		return a.checkVarDecl(scope, stmt.VarDecl)

	case stmt.FuncDecl != nil:
		funcScope, err := a.checkFuncDecl(scope, stmt.FuncDecl)
		if err != nil {
			return err
		}
		return a.checkBlock(funcScope, stmt.FuncDecl.Body)

	case stmt.If != nil:
		stmt := stmt.If
		if err := a.checkBoolExpr(scope, stmt.Condition); err != nil {
			return err
		}
		if err := a.checkStatement(scope.Sub(nil), stmt.Main); err != nil {
			return err
		}
		return a.checkStatement(scope.Sub(nil), stmt.Else)

	case stmt.Go != nil:
		if stmt.Go.Call == nil {
			return participle.Errorf(stmt.Go.Pos, "go requires a function call")
		}
		_, err := a.resolveTerminal(scope, stmt.Go.Call)
		return err

	case stmt.Switch != nil:
		return a.checkSwitch(scope, stmt.Switch)

	case stmt.Expression != nil:
		_, err := a.resolveExpr(scope, stmt.Expression)
		return err

	case stmt.Block != nil:
		return a.checkBlock(scope.Sub(nil), stmt.Block)
	}
	panic(stmt.Pos.String())
}

func (a *analyser) checkSwitch(scope *Scope, stmt *parser.SwitchStmt) error {
	target, err := a.resolveExprValue(scope, stmt.Target)
	if err != nil {
		return err
	}
	switch target := target.Type().(type) {
	case *types.Enum:
		return a.checkSwitchOnEnum(scope, target, stmt)

	case *types.Case:
		return a.checkSwitchOnEnum(scope, target.Enum, stmt)

	default:
		return a.checkSwitchOnValue(scope, target, stmt)
	}
}

func (a *analyser) checkSwitchOnEnum(scope *Scope, enum *types.Enum, stmt *parser.SwitchStmt) error {
	// Resolve cases and patterns.
	// TODO: Check for an exhaustive match.
	cases := enum.Cases()
	seen := make(map[string]bool, len(cases))
	for _, cse := range cases {
		seen[cse.Name] = true
	}
	for _, cse := range stmt.Cases {
		blockScope := scope.Sub(nil)
		if cse.Default {
			if len(seen) == 0 {
				return participle.Errorf(cse.Pos, "cases already exhausted, default is redundant")
			}
			seen = map[string]bool{}
		} else {
			if cse.Case.ExprCase != nil {
				return participle.Errorf(cse.Case.ExprCase.Pos, "expected .<case>")
			}
			name, err := a.checkPatternMatch(blockScope, enum, cse.Case.EnumCase)
			if err != nil {
				return err
			}
			delete(seen, name)
		}
		err := a.checkStatements(blockScope, cse.Body)
		if err != nil {
			return err
		}
	}
	if len(seen) != 0 {
		caseStrings := []string{}
		for cse := range seen {
			caseStrings = append(caseStrings, cse)
		}
		return participle.Errorf(stmt.Pos, "cases not matched: %s", strings.Join(caseStrings, ", "))
	}
	return nil
}

// Check semantics of pattern match, and add any pattern variables to the scope.
func (a *analyser) checkPatternMatch(scope *Scope, enum *types.Enum, pattern *parser.EnumCase) (string, error) {
	var selected *types.Case
	for _, cse := range enum.Cases() {
		if pattern.Case == cse.Name {
			selected = cse
			break
		}
	}
	if selected == nil {
		return "", participle.Errorf(pattern.Pos, "invalid enum case %q", pattern.Case)
	}
	if selected.Case == nil {
		if pattern.Var != "" {
			return "", participle.Errorf(pattern.Pos, "case %q does not have a type to apply", selected.Name)
		}
		return selected.Name, nil
	}

	if pattern.Var == "" {
		return "", participle.Errorf(pattern.Pos, "typed enum case %q requires a variable", selected.Name)
	}

	// Case has an associated type.
	err := scope.AddValue(pattern.Var, &types.Value{Typ: selected.Case})
	if err != nil {
		return "", participle.AnnotateError(pattern.Pos, err)
	}
	return selected.Name, nil
}

func (a *analyser) checkSwitchOnValue(scope *Scope, target types.Type, stmt *parser.SwitchStmt) error {
	for _, cse := range stmt.Cases {
		// Non-default case.
		if cse.Case != nil {
			if cse.Case.EnumCase != nil {
				return participle.Errorf(cse.Case.EnumCase.Pos, "unexpected enum case")
			}
			resolvedCase, err := a.resolveExpr(scope, cse.Case.ExprCase)
			if err != nil {
				return err
			}
			if resolvedCase.Type().Coerce(target.Type()) == nil {
				return participle.Errorf(cse.Case.ExprCase.Pos, "can't select case of type %s from %s", resolvedCase, target)
			}
		}
		for _, stmt := range cse.Body {
			if err := a.checkStatement(scope, stmt); err != nil {
				return err
			}
		}
	}
	return nil
}

func (a *analyser) casesForEnum(t types.Type) map[string]*types.Case {
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

func (a *analyser) checkBoolExpr(scope *Scope, cond *parser.Expr) error {
	condition, err := a.resolveExprValue(scope, cond)
	if err != nil {
		return err
	}
	if condition.Kind() != types.KindBool {
		return participle.Errorf(cond.Pos, "condition must be bool but is %s", condition)
	}
	return nil
}

func (a *analyser) checkBlock(scope *Scope, block *parser.Block) error {
	if block == nil {
		return nil
	}
	statements := block.Statements
	return a.checkStatements(scope, statements)
}

func (a *analyser) checkStatements(scope *Scope, statements []*parser.Stmt) error {
	for _, stmt := range statements {
		if err := a.checkStatement(scope, stmt); err != nil {
			return err
		}
	}
	return nil
}

func (a *analyser) checkVarDecl(scope *Scope, varDecl *parser.VarDecl) error {
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
			dfltValue, err := a.resolveExprValue(scope, decl.Default)
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
			typ, err = a.resolveTerminalType(scope, decl.Type)
			if err != nil {
				return err
			}
			if dfltTyp != nil && typ.Coerce(dfltTyp) == nil {
				return participle.Errorf(decl.Default.Pos, "can't assign %s to %s", dfltTyp, typ)
			}
		}
		err = a.declVars(varDecl.Pos, scope, &types.Value{Typ: typ}, untyped...)
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

func (a *analyser) resolveExprValue(scope *Scope, expr *parser.Expr) (*types.Value, error) {
	ref, err := a.resolveExpr(scope, expr)
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

func (a *analyser) resolveExprType(scope *Scope, expr *parser.Expr) (types.Type, error) {
	ref, err := a.resolveExpr(scope, expr)
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
func (a *analyser) resolveExpr(scope *Scope, expr *parser.Expr) (types.Reference, error) {
	if expr.Unary != nil {
		return a.resolveUnary(scope, expr.Unary)
	}
	lhs, err := a.resolveExprValue(scope, expr.Left)
	if err != nil {
		return nil, err
	}
	rhs, err := a.resolveExprValue(scope, expr.Right)
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

func (a *analyser) resolveUnary(s *Scope, unary *parser.Unary) (types.Reference, error) {
	sym, err := a.resolveTerminal(s, unary.Terminal)
	if err != nil {
		return nil, err
	}
	if sym.Type().CanApply(unary.Op, types.None) {
		return nil, participle.Errorf(unary.Pos, "! requires a boolean but got %s", sym.Kind())
	}
	return a.resolveTerminal(s, unary.Terminal)
}

func (a *analyser) resolveTerminalType(scope *Scope, terminal *parser.Terminal) (types.Type, error) {
	ref, err := a.resolveTerminal(scope, terminal)
	if err != nil {
		return nil, err
	}
	val, ok := ref.(types.Type)
	if !ok {
		return nil, participle.Errorf(terminal.Pos, "expected a type but got %s value", ref.Kind())
	}
	return val, nil
}

func (a *analyser) resolveTerminalValue(scope *Scope, terminal *parser.Terminal) (*types.Value, error) {
	ref, err := a.resolveTerminal(scope, terminal)
	if err != nil {
		return nil, err
	}
	val, ok := ref.(*types.Value)
	if !ok {
		return nil, participle.Errorf(terminal.Pos, "expected a value but got %s", ref.Kind())
	}
	return val, nil
}

func (a *analyser) resolveTerminal(scope *Scope, terminal *parser.Terminal) (types.Reference, error) {
	switch {
	case terminal.Literal != nil:
		return a.resolveLiteral(terminal.Literal)

	case terminal.Ident != "":
		sym := scope.Resolve(terminal.Ident)
		if sym == nil {
			return nil, participle.Errorf(terminal.Pos, "unknown symbol %q", terminal.Ident)
		}
		switch {
		case terminal.Reference != nil:
			return a.resolveField(scope, sym, terminal.Reference)

		case terminal.Call != nil:
			return a.resolveCallLike(scope, sym, terminal)

		case terminal.Subscript != nil:
			panic(terminal.Subscript.Pos.String())
		}
		return sym, nil

	case terminal.Tuple != nil:
		// Sub-expression.
		if len(terminal.Tuple) == 1 {
			return a.resolveExpr(scope, terminal.Tuple[0])
		}
		return nil, participle.Errorf(terminal.Pos, "tuples are not supported yet")

	default:
		return nil, participle.Errorf(terminal.Pos, "can't resolve %s", terminal.Describe())
	}
}

// Resolve something that looks like a function call (function, case, class initialiser).
func (a *analyser) resolveCallLike(scope *Scope, ref types.Reference, terminal *parser.Terminal) (*types.Value, error) {
	switch ref := ref.(type) {
	case *types.Case: // Case(Type)
		if ref.Case == nil {
			return nil, participle.Errorf(terminal.Call.Pos, "untyped case %q should not be called", terminal.Ident)
		}
		// Synthesise case parameters.
		parameters := []types.Parameter{{Type: ref.Case, Name: ref.Name}}
		_, err := a.resolveCallActual(scope, ref.Case, parameters, terminal.Call)
		if err != nil {
			return nil, err
		}
		return &types.Value{Typ: ref}, nil

	case *types.Class:
		var parameters []types.Parameter
		if ref.Init != nil {
			parameters = ref.Init.Parameters
		}
		return a.resolveCallActual(scope, ref, parameters, terminal.Call)

	case *types.Enum:
		var parameters []types.Parameter
		if ref.Init != nil {
			parameters = ref.Init.Parameters
		}
		return a.resolveCallActual(scope, ref, parameters, terminal.Call)

	case *types.Function:
		return a.resolveCallActual(scope, ref.ReturnType, ref.Parameters, terminal.Call)

	default:
		return nil, participle.Errorf(terminal.Call.Pos, "can't call %s", ref)
	}
}

func (a *analyser) resolveCallActual(scope *Scope, returnType types.Type, parameters []types.Parameter, call *parser.Call) (*types.Value, error) {
	if len(parameters) != len(call.Parameters) {
		return nil, participle.Errorf(call.Pos,
			"%d parameters provided for function that takes %d parameters",
			len(call.Parameters), len(parameters))
	}
	for i, param := range call.Parameters {
		value, err := a.resolveExprValue(scope, param)
		if err != nil {
			return nil, err
		}
		parameter := parameters[i]
		if value.Type().Coerce(parameter.Type) == nil {
			return nil, participle.Errorf(param.Pos, "can't coerce %q from %s to %s",
				parameter.Name, value.Kind(), parameter.Type)
		}
	}
	return &types.Value{Typ: returnType}, nil
}

func (a *analyser) resolveField(scope *Scope, parent types.Reference, terminal *parser.Terminal) (types.Reference, error) {
	switch {
	case terminal.Ident != "":
		field := parent.FieldByName(terminal.Ident)
		if field == nil {
			return nil, participle.Errorf(terminal.Pos, "unknown field %s.%s", parent, terminal.Ident)
		}
		switch {
		case terminal.Reference != nil:
			return a.resolveField(scope, field, terminal.Reference)

		case terminal.Call != nil:
			return a.resolveCallLike(scope, field, terminal)

		case terminal.Subscript != nil:
			panic(terminal.Subscript.Pos.String())
		}
		return field, nil

	default:
		return nil, participle.Errorf(terminal.Pos, "invalid field reference %s", terminal.Describe())
	}
}

func (a *analyser) resolveLiteral(literal *parser.Literal) (types.Reference, error) {
	switch {
	case literal.Number != nil:
		return &types.Value{Typ: types.Number}, nil

	case literal.Str != nil:
		// TODO: Resolve interpolation vars eg. "\(x), \(y), \(z)".
		return &types.Value{Typ: types.String}, nil

	case literal.Bool != nil:
		return &types.Value{Typ: types.Bool}, nil

	default:
		return nil, participle.Errorf(literal.Pos, "unsupported literal %s", literal.Describe())
	}
}

// Declare vars of typ in scope.
func (a *analyser) declVars(pos lexer.Position, scope *Scope, value *types.Value, names ...string) error {
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
