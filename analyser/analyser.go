// Package analyser
package analyser

import (
	"math/big"
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

type funcAndScope struct {
	fn    *parser.Block
	scope *Scope
}

type analyser struct {
	p     *Program
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
	enumt := &types.Enum{
		Name: enum.Type.Type,
	}
	a.p.associate(enum, enumt)
	err := scope.AddType(enum.Type.Type, enumt)
	if err != nil {
		return participle.Errorf(enum.Pos, "%s", err)
	}
	// Intermediate scope for "self" (so we don't add it to the set of fields).
	enumScope := scope.Sub(enumt)
	err = enumScope.AddValue("self", types.Var(enumt))
	if err != nil {
		return participle.Errorf(enum.Pos, "%s", err)
	}

	// Add generics to the enum scope.
	t := enum.Type
	enumt.TParams, err = a.declGenericParameters(enumScope, t)
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

		case member.FuncDecl != nil:
			funcScope, err := a.checkFuncDecl(enumScope, member.FuncDecl)
			if err != nil {
				return err
			}
			a.deferFunc(member.FuncDecl.Body, funcScope)

		default:
			panic("??")
		}
	}
	enumt.Flds = a.scopeToTypeFields(enumScope)
	return nil
}

func (a *analyser) declGenericParameters(scope *Scope, t *parser.NamedTypeDecl) ([]types.NamedType, error) {
	var flds []types.NamedType
	for _, gp := range t.TypeParameter {
		if len(gp.Constraints) > 0 {
			return nil, participle.Errorf(t.Pos, "generic constraints are not supported yet")
		}
		flds = append(flds, types.NamedType{
			Nme: gp.Name,
		})
		a.p.associate(t, types.Any)
		err := scope.AddType(gp.Name, types.Any)
		if err != nil {
			return nil, participle.AnnotateError(t.Pos, err)
		}
	}
	return flds, nil
}

func (a *analyser) scopeToTypeFields(scope *Scope) []types.NamedType {
	symbols := scope.Symbols()
	var out []types.NamedType
	for name, sym := range symbols {
		out = append(out, types.NamedType{
			Nme: name,
			Typ: sym.Type(),
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

func (a *analyser) resolveType(scope *Scope, cse *parser.TypeDecl) (types.Type, error) {
	switch {
	case cse.Named != nil:
		typ := a.p.resolveConcreteType(cse, scope, cse.Named.Type)
		if typ == nil {
			return nil, participle.Errorf(cse.Pos, "unknown type %q", cse)
		}
		return typ, nil

	case cse.Array != nil:
		el, err := a.resolveType(scope, cse.Array.Element)
		if err != nil {
			return nil, err
		}
		return types.Array(el), nil

	case cse.DictOrSet != nil:
		key, err := a.resolveType(scope, cse.DictOrSet.Key)
		if err != nil {
			return nil, err
		}
		// Dict
		if cse.DictOrSet.Value != nil {
			value, err := a.resolveType(scope, cse.DictOrSet.Value)
			if err != nil {
				return nil, err
			}
			return types.Map(key, value), nil
		}
		return types.Set(key), nil

	default:
		return nil, participle.Errorf(cse.Pos, "unknown type %q", cse)
	}
}

func (a *analyser) checkClassDecl(scope *Scope, class *parser.ClassDecl) error {
	clst := &types.ClassType{
		Name: class.Type.Type,
	}
	// Intermediate scope for "self" (so we don't add it to the set of fields).
	classScope := scope.Sub(clst)
	err := scope.AddType(class.Type.Type, clst)
	if err != nil {
		return participle.AnnotateError(class.Pos, err)
	}
	err = classScope.AddValue("self", types.Var(clst))
	if err != nil {
		return participle.AnnotateError(class.Pos, err)
	}
	clst.TParams, err = a.declGenericParameters(classScope, class.Type)
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

		default:
			panic("??")
		}
	}
	clst.Flds = a.scopeToTypeFields(classScope)
	a.p.associate(class, clst)
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
		returnType, err = a.resolveTypeExpr(scope, fn.Return)
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
		typ, err := a.resolveTypeReference(scope.Parent(), param.Type)
		if err != nil {
			return err
		}
		err = a.declVars(param.Pos, scope, types.Let(typ), param.Names...)
		if err != nil {
			return err
		}
	}
	return nil
}

func (a *analyser) makeFunction(scope *Scope, returnType types.Type, parameters []*parser.Parameters) (*types.Function, error) {
	fnt := &types.Function{ReturnType: returnType}
	for _, param := range parameters {
		typ, err := a.resolveTypeReference(scope, param.Type)
		if err != nil {
			return nil, err
		}
		for _, name := range param.Names {
			fnt.Parameters = append(fnt.Parameters, types.NamedType{Nme: name, Typ: typ})
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
		if types.Coerce(val.Type(), f.ReturnType) == nil {
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
		if err := a.checkBlock(scope.Sub(nil), stmt.Main); err != nil {
			return err
		}
		return a.checkBlock(scope.Sub(nil), stmt.Else)

	case stmt.Switch != nil:
		return a.checkSwitch(scope, stmt.Switch)

	case stmt.ExprStmt != nil:
		return a.checkExprStmt(scope, stmt.ExprStmt)

	case stmt.Block != nil:
		return a.checkBlock(scope.Sub(nil), stmt.Block)

	case stmt.For != nil:
		return a.checkForStmt(scope, stmt.For)
	}
	panic("unsupported statement at " + stmt.Pos.String())
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
			if types.Coerce(resolvedCase.Type(), target.Type()) == nil {
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
		c, ok := f.Typ.(*types.Case)
		if !ok {
			continue
		}
		out[f.Nme] = c
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
	var untyped []*parser.VarDeclAsgn // Collect any vars that don't end up with types associated.
	if len(varDecl.Vars) == 0 {
		panic("no variables in decl")
	}
	var last *parser.VarDeclAsgn
	for _, decl := range varDecl.Vars {
		last = decl
		untyped = append(untyped, decl)
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
				return participle.Wrapf(decl.Default.Pos, err, "invalid initial value for %q", decl.Name)
			}
			ref, err := types.Concrete(dfltValue.Type())
			if err != nil {
				return participle.Wrapf(decl.Default.Pos, err, "invalid initial value for %q", decl.Name)
			}
			dfltTyp = ref.(types.Type)
		}
		if decl.Type == nil {
			if dfltTyp == nil {
				return participle.Errorf(decl.Pos, "type not specified for %q (and no default value provided)", decl.Name)
			}
			// Infer type from the default value.
			typ = dfltTyp
		} else {
			typ, err = a.resolveTypeExpr(scope, decl.Type)
			if err != nil {
				return participle.Wrapf(decl.Type.Pos, err, "invalid type for %q", decl.Name)
			}
			if dfltTyp != nil {
				if coerced := types.Coerce(dfltTyp, typ); coerced == nil {
					return participle.Errorf(decl.Default.Pos, "can't assign %s to %s", dfltTyp, typ)
				} else {
					typ = coerced
				}
			}
		}
		value := types.Var(typ)
		for _, sym := range untyped {
			a.p.associate(sym, value)
		}
		names := []string{}
		for _, decl := range untyped {
			names = append(names, decl.Name)
		}
		err = a.declVars(decl.Pos, scope, value, names...)
		if err != nil {
			return participle.Wrapf(decl.Pos, err, "invalid variable %q", decl.Name)
		}
		untyped = nil
	}
	if len(untyped) > 0 {
		return participle.Errorf(last.Pos, "type not specified for %q (and no default value provided)", last.Name)
	}
	return nil
}

func (a *analyser) resolveExprValue(scope *Scope, expr *parser.Expr) (*types.Value, error) {
	ref, err := a.resolveExpr(scope, expr)
	if err != nil {
		return nil, err
	}
	// Special case referencing enum case fields so they are correctly turned into values.
	if tfield, ok := ref.(types.NamedType); ok {
		ref = tfield.Typ
	}
	switch ref := ref.(type) {
	case *types.Value:
		return ref, nil

	case *types.Case:
		return &types.Value{Typ: ref}, nil

	case types.Field:
		return ref.Value, nil

	default:
		return nil, participle.Errorf(expr.Pos, "expected a value expression but got %s", ref)
	}
}

func (a *analyser) resolveTypeExpr(scope *Scope, expr *parser.Expr) (types.Type, error) {
	ref, err := a.resolveExpr(scope, expr)
	if err != nil {
		return nil, err
	}
	typ, ok := ref.(types.Type)
	if !ok {
		return nil, participle.Errorf(expr.Pos, "expected a type but got %s", ref)
	}
	return typ, nil
}

func (a *analyser) autoAssoc(node parser.Node, ref *types.Reference) {
	if *ref != nil {
		a.p.associate(node, *ref)
	}
}

// Check and resolve an expression to its type.
func (a *analyser) resolveExpr(scope *Scope, expr *parser.Expr) (ref types.Reference, err error) {
	defer a.autoAssoc(expr, &ref)
	if expr.Unary != nil {
		return a.resolveUnary(scope, expr.Unary)
	}
	lhs, err := a.resolveExpr(scope, expr.Left)
	if err != nil {
		return nil, err
	}
	rhs, err := a.resolveExpr(scope, expr.Right)
	if err != nil {
		return nil, err
	}
	if expr.Op == parser.OpBitOr {
		if typ := a.resolveAnonymousEnum(lhs, rhs); typ != nil {
			return typ, nil
		}
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
		ref := &types.Value{Typ: types.Bool}
		return ref, nil
	}
	panic(expr.Pos.String())
}

func (a *analyser) resolveAnonymousEnum(lhs, rhs types.Reference) types.Reference {
	lhsf := a.typeToFields(lhs)
	if lhsf == nil {
		return nil
	}
	rhsf := a.typeToFields(rhs)
	if rhsf == nil {
		return nil
	}
	// TODO: Check that case names don't collide.
	return &types.Enum{
		Flds: append(lhsf, rhsf...),
	}
}

func (a *analyser) typeToFields(typ types.Reference) []types.NamedType {
	var fields []types.NamedType
	switch typ := typ.(type) {
	case *types.Enum:
		fields = append(fields, typ.Fields()...)

	case types.Type:
		name := types.TypeName(typ)
		fields = append(fields, types.NamedType{
			Nme: name,
			Typ: &types.Case{
				Name: name,
				Case: typ,
			},
		})
	}
	return fields
}

func (a *analyser) resolveUnary(s *Scope, unary *parser.Unary) (ref types.Reference, err error) {
	defer a.autoAssoc(unary, &ref)
	sym, err := a.resolveReference(s, unary.Reference)
	if err != nil {
		return nil, err
	}
	if unary.Op != 0 && sym.Type().CanApply(unary.Op, types.None) {
		return nil, participle.Errorf(unary.Pos, "%s requires a boolean but got %s", unary.Op, sym.Kind())
	}
	return a.resolveReference(s, unary.Reference)
}

// Resolve a reference and ensure it refers to a type (not a value).
func (a *analyser) resolveTypeReference(scope *Scope, terminal *parser.Reference) (types.Type, error) {
	ref, err := a.resolveReference(scope, terminal)
	if err != nil {
		return nil, err
	}
	val, ok := ref.(types.Type)
	if !ok {
		return nil, participle.Errorf(terminal.Pos, "expected a type but got %s value", ref.Kind())
	}
	return val, nil
}

// Resolve a reference and ensure it refers to a value (not a type).
func (a *analyser) resolveValueReference(scope *Scope, terminal *parser.Reference) (*types.Value, error) {
	ref, err := a.resolveReference(scope, terminal)
	if err != nil {
		return nil, err
	}
	val, ok := ref.(*types.Value)
	if !ok {
		return nil, participle.Errorf(terminal.Pos, "expected a terminal value but got %s", ref.Kind())
	}
	return val, nil
}

func (a *analyser) resolveTerminal(scope *Scope, terminal *parser.Terminal) (ref types.Reference, err error) {
	defer a.autoAssoc(terminal, &ref)
	switch {
	case terminal.Literal != nil:
		return a.resolveLiteral(scope, terminal.Literal)

	case terminal.New != nil:
		typ, err := a.resolveTypeReference(scope, terminal.New.Type)
		if err != nil {
			return nil, err
		}
		if len(terminal.New.Init) != 0 {
			return nil, participle.Errorf(terminal.Pos, "constructor calls are not supported yet")
		}
		return &types.Value{Typ: typ}, nil

	case terminal.Ident != "":
		ref := a.p.resolveConcrete(terminal, scope, terminal.Ident)
		if ref == nil {
			return nil, participle.Errorf(terminal.Pos, "unknown symbol %q", terminal.Ident)
		}
		return ref, nil

	case terminal.Tuple != nil:
		// Sub-expression.
		if len(terminal.Tuple) == 1 {
			return a.resolveExpr(scope, terminal.Tuple[0])
		}
		return nil, participle.Errorf(terminal.Pos, "tuples are not supported yet")

	default:
		panic("??")
	}
}

func (a *analyser) resolveReference(scope *Scope, reference *parser.Reference) (ref types.Reference, err error) {
	defer a.autoAssoc(reference, &ref)
	ref, err = a.resolveTerminal(scope, reference.Terminal)
	if err != nil {
		return nil, err
	}
	ref, err = a.resolveReferenceNext(scope, ref, reference.Next)
	if err != nil {
		return nil, err
	}
	if reference.Optional {
		typ, ok := ref.(types.Type)
		if !ok {
			return nil, participle.Errorf(reference.Pos, "the optional modifier ? must be applied to a type, not %s", ref)
		}
		ref = types.Optional(typ)
	}
	return ref, nil
}

func (a *analyser) resolveReferenceNext(scope *Scope, ref types.Reference, next *parser.ReferenceNext) (types.Reference, error) {
	if next == nil {
		return ref, nil
	}
	switch {
	case next.Reference != nil:
		return a.resolveField(scope, ref, next.Reference)

	case next.Call != nil:
		return a.resolveCallLike(scope, ref, next)

	case next.Subscript != nil:
		panic("subscript not supported: " + next.Subscript.Pos.String())

	case next.Specialisation != nil:
		typ, ok := ref.(types.Type)
		if !ok {
			return nil, participle.Errorf(next.Pos, "type specialisation <> must be applied to a type, not %s", ref)
		}
		typeParams := typ.Fields()
		if len(next.Specialisation) != len(typeParams) {
			return nil, participle.Errorf(next.Pos, "need %d type parameters for %s but have %d", len(next.Specialisation), typ, len(typeParams))
		}
		params := []types.Type{}
		for i, param := range next.Specialisation {
			ptyp, err := a.resolveTypeReference(scope, param)
			if err != nil {
				return nil, participle.Wrapf(next.Pos, err, "type parameter %s", typeParams[i].Nme)
			}
			if typeParams[i].Typ != nil {
				return nil, participle.Errorf(param.Pos, "type constraints are not supported")
			}
			params = append(params, ptyp)
		}
		return types.Specialise(typ, params...), nil

	default:
		panic("??")
	}
}

// Resolve something that looks like a function call (function, case, class initialiser).
func (a *analyser) resolveCallLike(scope *Scope, ref types.Reference, ast *parser.ReferenceNext) (*types.Value, error) {
	switch ref := ref.(type) {
	case *types.Case: // Case(Type)
		if ref.Case == nil {
			return nil, participle.Errorf(ast.Call.Pos, "untyped case should not be called")
		}
		// Synthesise case parameters.
		parameters := []types.NamedType{{Typ: ref.Case, Nme: ref.Name}}
		_, err := a.resolveCallActual(scope, ref.Case, parameters, ast.Call)
		if err != nil {
			return nil, err
		}
		return &types.Value{Typ: ref}, nil

	case *types.ClassType:
		var parameters []types.NamedType
		if ref.Init != nil {
			parameters = ref.Init.Parameters
		}
		return a.resolveCallActual(scope, ref, parameters, ast.Call)

	case *types.Enum:
		var parameters []types.NamedType
		if ref.Init != nil {
			parameters = ref.Init.Parameters
		}
		return a.resolveCallActual(scope, ref, parameters, ast.Call)

	case *types.Function:
		return a.resolveCallActual(scope, ref.ReturnType, ref.Parameters, ast.Call)

	default:
		return nil, participle.Errorf(ast.Call.Pos, "can't call %s", ref)
	}
}

func (a *analyser) resolveCallActual(scope *Scope, returnType types.Type, parameters []types.NamedType, call *parser.Call) (*types.Value, error) {
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
		if types.Coerce(value.Type(), parameter.Typ) == nil {
			return nil, participle.Errorf(param.Pos, "can't coerce %q from %s to %s",
				parameter.Name(), value.Kind(), parameter.Type())
		}
	}
	return &types.Value{Typ: returnType}, nil
}

func (a *analyser) resolveField(scope *Scope, parent types.Reference, terminal *parser.Terminal) (types.Reference, error) {
	switch {
	case terminal.Ident != "":
		field := types.FieldByName(parent, terminal.Ident)
		if field == nil {
			return nil, participle.Errorf(terminal.Pos, "unknown field %s on %s", terminal.Ident, parent)
		}
		a.p.associateConcrete(terminal, field)
		a.p.associate(terminal, field)
		return field, nil

	default:
		return nil, participle.Errorf(terminal.Pos, "invalid field reference via %s", terminal.Describe())
	}
}

func (a *analyser) resolveLiteral(scope *Scope, literal *parser.Literal) (ref types.Reference, err error) {
	defer a.autoAssoc(literal, &ref)
	switch {
	case literal.Number != nil:
		n := big.Float(*literal.Number)
		if n.IsInt() {
			return &types.Value{Typ: types.LiteralInt}, nil
		}
		return &types.Value{Typ: types.LiteralFloat}, nil

	case literal.Str != nil:
		// TODO: Resolve interpolation vars eg. "{x}, {y}, {z}".
		return &types.Value{Typ: types.LiteralString}, nil

	case literal.Bool != nil:
		return &types.Value{Typ: types.Bool}, nil

	case literal.Array != nil:
		return a.resolveArrayLiteral(scope, literal.Array)

	case literal.DictOrSet != nil:
		return a.resolveDictOrSetLiteral(scope, literal.DictOrSet)

	default:
		panic("unsupported literal " + literal.Pos.String())
	}
}

func (a *analyser) resolveArrayLiteral(scope *Scope, array *parser.ArrayLiteral) (ref types.Reference, err error) {
	defer a.autoAssoc(array, &ref)
	var (
		element types.Reference
	)
	for _, v := range array.Values {
		element, err = a.checkCompoundTypeConsistency(scope, v, element)
		if err != nil {
			return nil, err
		}
	}
	if element == nil {
		return nil, participle.Errorf(array.Pos, "can't infer element type from empty array")
	}
	if _, ok := element.(*types.Value); ok {
		return &types.Value{Typ: types.Array(element.Type())}, nil
	}
	return types.Array(element.Type()), nil
}

func (a *analyser) resolveDictOrSetLiteral(scope *Scope, value *parser.DictOrSetLiteral) (types.Reference, error) {
	if value.Entries[0].Value != nil {
		return a.resolveDictLiteral(scope, value)
	}
	return a.resolveSetLiteral(scope, value)
}

func (a *analyser) resolveSetLiteral(scope *Scope, set *parser.DictOrSetLiteral) (ref types.Reference, err error) {
	defer a.autoAssoc(set, &ref)
	var (
		element types.Reference
	)
	for i, v := range set.Entries {
		if v.Value != nil {
			return nil, participle.Errorf(set.Pos, "dict value in set at index %d", i)
		}
		element, err = a.checkCompoundTypeConsistency(scope, v.Key, element)
		if err != nil {
			return nil, err
		}
	}
	if _, ok := element.(*types.Value); ok {
		return &types.Value{Typ: types.Set(element.Type())}, nil
	}
	return types.Set(element.Type()), nil
}

func (a *analyser) resolveDictLiteral(scope *Scope, dict *parser.DictOrSetLiteral) (ref types.Reference, err error) {
	defer a.autoAssoc(dict, &ref)
	var (
		key, value types.Reference
	)
	for i, v := range dict.Entries {
		if v.Value == nil {
			return nil, participle.Errorf(dict.Pos, "set value in dict at index %d", i)
		}
		key, err = a.checkCompoundTypeConsistency(scope, v.Key, key)
		if err != nil {
			return nil, err
		}
		// Check value type.
		value, err = a.checkCompoundTypeConsistency(scope, v.Value, value)
		if err != nil {
			return nil, err
		}
	}
	if _, ok := key.(*types.Value); ok {
		return &types.Value{Typ: types.Map(key.Type(), value.Type())}, nil
	}
	return types.Map(key.Type(), value.Type()), nil
}

// Declare vars of typ in scope.
func (a *analyser) declVars(pos lexer.Position, scope *Scope, value *types.Value, names ...string) error {
	if value == nil {
		return participle.Errorf(pos, "no type provided (type inference not implemented yet)")
	}
	for _, name := range names {
		err := scope.AddValue(name, value)
		if err != nil {
			return participle.AnnotateError(pos, err)
		}
	}
	return nil
}

func (a *analyser) checkCompoundTypeConsistency(scope *Scope, expr *parser.Expr, element types.Reference) (types.Reference, error) {
	t, err := a.resolveExpr(scope, expr)
	if err != nil {
		return nil, err
	}
	if element == nil {
		element, err = types.Concrete(t)
		if err != nil {
			return nil, participle.AnnotateError(expr.Pos, err)
		}
	} else {
		if types.Coerce(t.Type(), element.Type()) == nil {
			return nil, participle.Errorf(expr.Pos, "inconsistent element types %s and %s", element.Type(), t.Type())
		}
	}
	return element, nil
}

// Expressions used as statements must either be a function call or an assignment.
func (a *analyser) checkExprStmt(scope *Scope, stmt *parser.ExprStmt) error {
	lhs, err := a.resolveExprValue(scope, stmt.LHS)
	if err != nil {
		return err
	}
	if stmt.LHS.Unary == nil {
		return participle.Errorf(stmt.Pos, "statement with no effect")
	}
	if stmt.RHS == nil {
		// We don't have a right-hand side, so we have to check that the LHS is a function call.
		return a.checkExprStmtIsFunctionCall(scope, stmt.LHS.Unary)
	}
	if !lhs.Properties.Has(types.Assignable) {
		return participle.Errorf(stmt.LHS.Pos, "left hand side of assignment must be assignable")
	}
	rhs, err := a.resolveExprValue(scope, stmt.RHS)
	if err != nil {
		return err
	}
	if types.Coerce(rhs.Type(), lhs.Type()) == nil {
		return participle.Errorf(stmt.Pos, "couldn't assign %s to %s", rhs.Type(), lhs.Type())
	}
	return nil
}

func (a *analyser) checkExprStmtIsFunctionCall(scope *Scope, expr *parser.Unary) error {
	if expr.Op != 0 {
		return participle.Errorf(expr.Pos, "statement with no effect")
	}
	ref := expr.Reference.Next
	if ref == nil {
		return participle.Errorf(expr.Reference.Pos, "statement with no effect")
	}
	for ref.Next != nil {
		ref = ref.Next
	}
	// Reference terminal must be a call.
	if ref.Call == nil {
		return participle.Errorf(expr.Reference.Pos, "statement with no effect")
	}
	return nil
}

func (a *analyser) checkForStmt(scope *Scope, stmt *parser.ForStmt) error {
	panic("???")
}
