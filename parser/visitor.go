package parser

// go-sumtype:decl Node

// A Node in the AST.
type Node interface {
	accept(visitor VisitorFunc) error
}

// Next should be called by VisitorFunc to proceed with the walk.
//
// The walk will terminate if "err" is non-nil.
type Next func(err error) error

// VisitorFunc can be used to walk all nodes in the model.
type VisitorFunc func(node Node, next Next) error

// VisitFunc calls the visitor function on all nodes.
func VisitFunc(node Node, visit VisitorFunc) error {
	if node == nil {
		return nil
	}
	return node.accept(visit)
}

// Visitor type-safe interface.
type Visitor interface {
	VisitAST(n *AST) error
	VisitArrayLiteral(n ArrayLiteral) error
	VisitBlock(n Block) error
	VisitCall(n Call) error
	VisitCaseDecl(n *CaseDecl) error
	VisitCaseSelect(n CaseSelect) error
	VisitCaseStmt(n CaseStmt) error
	VisitClassDecl(n *ClassDecl) error
	VisitClassMember(n *ClassMember) error
	VisitDecl(n Decl) error
	VisitDictOrSetEntryLiteral(n DictOrSetEntryLiteral) error
	VisitDictOrSetLiteral(n DictOrSetLiteral) error
	VisitEnumCase(n EnumCase) error
	VisitEnumDecl(n *EnumDecl) error
	VisitEnumMember(n *EnumMember) error
	VisitExpr(n *Expr) error
	VisitForStmt(n ForStmt) error
	VisitFuncDecl(n *FuncDecl) error
	VisitIfStmt(n IfStmt) error
	VisitImportDecl(n *ImportDecl) error
	VisitInitialiserDecl(n *InitialiserDecl) error
	VisitLiteral(n *Literal) error
	VisitParameters(n Parameters) error
	VisitReference(n *Reference) error
	VisitReferenceNext(n *ReferenceNext) error
	VisitReturnStmt(n ReturnStmt) error
	VisitRootDecl(n *RootDecl) error
	VisitStmt(n Stmt) error
	VisitSwitchStmt(n SwitchStmt) error
	VisitTerminal(n Terminal) error
	VisitTypeDecl(n TypeDecl) error
	VisitTypeParamDecl(n TypeParamDecl) error
	VisitUnary(n *Unary) error
	VisitVarDecl(n *VarDecl) error
	VisitVarDeclAsgn(n VarDeclAsgn) error
}

// DefaultVisitor can be embedded to provide default no-op visitor methods.
type DefaultVisitor struct{}

var _ Visitor = DefaultVisitor{}

func (d DefaultVisitor) VisitAST(n *AST) error                                    { return nil }
func (d DefaultVisitor) VisitArrayLiteral(n ArrayLiteral) error                   { return nil }
func (d DefaultVisitor) VisitBlock(n Block) error                                 { return nil }
func (d DefaultVisitor) VisitCall(n Call) error                                   { return nil }
func (d DefaultVisitor) VisitCaseDecl(n *CaseDecl) error                          { return nil }
func (d DefaultVisitor) VisitCaseSelect(n CaseSelect) error                       { return nil }
func (d DefaultVisitor) VisitCaseStmt(n CaseStmt) error                           { return nil }
func (d DefaultVisitor) VisitClassDecl(n *ClassDecl) error                        { return nil }
func (d DefaultVisitor) VisitClassMember(n *ClassMember) error                    { return nil }
func (d DefaultVisitor) VisitDecl(n Decl) error                                   { return nil }
func (d DefaultVisitor) VisitDictOrSetEntryLiteral(n DictOrSetEntryLiteral) error { return nil }
func (d DefaultVisitor) VisitDictOrSetLiteral(n DictOrSetLiteral) error           { return nil }
func (d DefaultVisitor) VisitEnumCase(n EnumCase) error                           { return nil }
func (d DefaultVisitor) VisitEnumDecl(n *EnumDecl) error                          { return nil }
func (d DefaultVisitor) VisitEnumMember(n *EnumMember) error                      { return nil }
func (d DefaultVisitor) VisitExpr(n *Expr) error                                  { return nil }
func (d DefaultVisitor) VisitForStmt(n ForStmt) error                             { return nil }
func (d DefaultVisitor) VisitFuncDecl(n *FuncDecl) error                          { return nil }
func (d DefaultVisitor) VisitIfStmt(n IfStmt) error                               { return nil }
func (d DefaultVisitor) VisitImportDecl(n *ImportDecl) error                      { return nil }
func (d DefaultVisitor) VisitInitialiserDecl(n *InitialiserDecl) error            { return nil }
func (d DefaultVisitor) VisitLiteral(n *Literal) error                            { return nil }
func (d DefaultVisitor) VisitParameters(n Parameters) error                       { return nil }
func (d DefaultVisitor) VisitReference(n *Reference) error                        { return nil }
func (d DefaultVisitor) VisitReferenceNext(n *ReferenceNext) error                { return nil }
func (d DefaultVisitor) VisitReturnStmt(n ReturnStmt) error                       { return nil }
func (d DefaultVisitor) VisitRootDecl(n *RootDecl) error                          { return nil }
func (d DefaultVisitor) VisitStmt(n Stmt) error                                   { return nil }
func (d DefaultVisitor) VisitSwitchStmt(n SwitchStmt) error                       { return nil }
func (d DefaultVisitor) VisitTerminal(n Terminal) error                           { return nil }
func (d DefaultVisitor) VisitTypeDecl(n TypeDecl) error                           { return nil }
func (d DefaultVisitor) VisitTypeParamDecl(n TypeParamDecl) error                 { return nil }
func (d DefaultVisitor) VisitUnary(n *Unary) error                                { return nil }
func (d DefaultVisitor) VisitVarDecl(n *VarDecl) error                            { return nil }
func (d DefaultVisitor) VisitVarDeclAsgn(n VarDeclAsgn) error                     { return nil }

// Visit walks the AST calling the corresponding method on "visitor" for each AST node type.
func Visit(node Node, visitor Visitor) error {
	return VisitFunc(node, func(node Node, next Next) error {
		switch n := node.(type) {
		case *AST:
			return next(visitor.VisitAST(n))
		case ArrayLiteral:
			return next(visitor.VisitArrayLiteral(n))
		case Block:
			return next(visitor.VisitBlock(n))
		case Call:
			return next(visitor.VisitCall(n))
		case *CaseDecl:
			return next(visitor.VisitCaseDecl(n))
		case CaseSelect:
			return next(visitor.VisitCaseSelect(n))
		case CaseStmt:
			return next(visitor.VisitCaseStmt(n))
		case *ClassDecl:
			return next(visitor.VisitClassDecl(n))
		case *ClassMember:
			return next(visitor.VisitClassMember(n))
		case Decl:
			return next(visitor.VisitDecl(n))
		case DictOrSetEntryLiteral:
			return next(visitor.VisitDictOrSetEntryLiteral(n))
		case DictOrSetLiteral:
			return next(visitor.VisitDictOrSetLiteral(n))
		case EnumCase:
			return next(visitor.VisitEnumCase(n))
		case *EnumDecl:
			return next(visitor.VisitEnumDecl(n))
		case *EnumMember:
			return next(visitor.VisitEnumMember(n))
		case *Expr:
			return next(visitor.VisitExpr(n))
		case ForStmt:
			return next(visitor.VisitForStmt(n))
		case *FuncDecl:
			return next(visitor.VisitFuncDecl(n))
		case IfStmt:
			return next(visitor.VisitIfStmt(n))
		case *ImportDecl:
			return next(visitor.VisitImportDecl(n))
		case *InitialiserDecl:
			return next(visitor.VisitInitialiserDecl(n))
		case Parameters:
			return next(visitor.VisitParameters(n))
		case *ReferenceNext:
			return next(visitor.VisitReferenceNext(n))
		case *RootDecl:
			return next(visitor.VisitRootDecl(n))
		case SwitchStmt:
			return next(visitor.VisitSwitchStmt(n))
		case TypeDecl:
			return next(visitor.VisitTypeDecl(n))
		case *Unary:
			return next(visitor.VisitUnary(n))
		case VarDeclAsgn:
			return next(visitor.VisitVarDeclAsgn(n))
		case *Literal:
			return next(visitor.VisitLiteral(n))
		case *Reference:
			return next(visitor.VisitReference(n))
		case ReturnStmt:
			return next(visitor.VisitReturnStmt(n))
		case Stmt:
			return next(visitor.VisitStmt(n))
		case Terminal:
			return next(visitor.VisitTerminal(n))
		case TypeParamDecl:
			return next(visitor.VisitTypeParamDecl(n))
		case *VarDecl:
			return next(visitor.VisitVarDecl(n))
		default:
			panic("??")
		}
	})
}
