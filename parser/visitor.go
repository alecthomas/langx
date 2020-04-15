package parser

import (
	"errors"
	"reflect"
)

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
//
// Any method may return TerminateRecursion to stop recursion but continue with traversal.
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
	VisitDictOrSetEntryLiteral(n DictOrSetEntryLiteral) error
	VisitDictOrSetLiteral(n DictOrSetLiteral) error
	VisitEnumCase(n EnumCase) error
	VisitEnumDecl(n *EnumDecl) error
	VisitEnumMember(n *EnumMember) error
	VisitExpr(n *Expr) error
	VisitNew(n *NewExpr) error
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
	VisitArrayTypeDecl(n *ArrayTypeDecl) error
	VisitDictOrSetTypeDecl(n *DictOrSetTypeDecl) error
	VisitNamedTypeDecl(n *NamedTypeDecl) error
	VisitTypeParamDecl(n TypeParamDecl) error
	VisitUnary(n *Unary) error
	VisitVarDecl(n *VarDecl) error
	VisitVarDeclAsgn(n VarDeclAsgn) error
}

// TerminateRecursion should be returned by Visitor methods to terminate recursion.
var TerminateRecursion = errors.New("no recurse")

// Visit walks the AST calling the corresponding method on "visitor" for each AST node type.
func Visit(node Node, visitor Visitor) error {
	return VisitFunc(node, func(node Node, next Next) error {
		if node == nil || (reflect.ValueOf(node).Kind() == reflect.Ptr && reflect.ValueOf(node).IsNil()) {
			return nil
		}
		maybeNext := func(err error) error {
			if err == TerminateRecursion {
				return nil
			}
			return next(err)
		}
		switch n := node.(type) {
		case nil:
			return nil
		case *AST:
			return maybeNext(visitor.VisitAST(n))
		case ArrayLiteral:
			return maybeNext(visitor.VisitArrayLiteral(n))
		case Block:
			return maybeNext(visitor.VisitBlock(n))
		case Call:
			return maybeNext(visitor.VisitCall(n))
		case *CaseDecl:
			return maybeNext(visitor.VisitCaseDecl(n))
		case CaseSelect:
			return maybeNext(visitor.VisitCaseSelect(n))
		case CaseStmt:
			return maybeNext(visitor.VisitCaseStmt(n))
		case *ClassDecl:
			return maybeNext(visitor.VisitClassDecl(n))
		case *ClassMember:
			return maybeNext(visitor.VisitClassMember(n))
		case DictOrSetEntryLiteral:
			return maybeNext(visitor.VisitDictOrSetEntryLiteral(n))
		case DictOrSetLiteral:
			return maybeNext(visitor.VisitDictOrSetLiteral(n))
		case EnumCase:
			return maybeNext(visitor.VisitEnumCase(n))
		case *EnumDecl:
			return maybeNext(visitor.VisitEnumDecl(n))
		case *EnumMember:
			return maybeNext(visitor.VisitEnumMember(n))
		case *Expr:
			return maybeNext(visitor.VisitExpr(n))
		case ForStmt:
			return maybeNext(visitor.VisitForStmt(n))
		case *FuncDecl:
			return maybeNext(visitor.VisitFuncDecl(n))
		case IfStmt:
			return maybeNext(visitor.VisitIfStmt(n))
		case *ImportDecl:
			return maybeNext(visitor.VisitImportDecl(n))
		case *InitialiserDecl:
			return maybeNext(visitor.VisitInitialiserDecl(n))
		case Parameters:
			return maybeNext(visitor.VisitParameters(n))
		case *ReferenceNext:
			return maybeNext(visitor.VisitReferenceNext(n))
		case *RootDecl:
			return maybeNext(visitor.VisitRootDecl(n))
		case SwitchStmt:
			return maybeNext(visitor.VisitSwitchStmt(n))
		case TypeDecl:
			return maybeNext(visitor.VisitTypeDecl(n))
		case *NamedTypeDecl:
			return maybeNext(visitor.VisitNamedTypeDecl(n))
		case *ArrayTypeDecl:
			return maybeNext(visitor.VisitArrayTypeDecl(n))
		case *DictOrSetTypeDecl:
			return maybeNext(visitor.VisitDictOrSetTypeDecl(n))
		case *Unary:
			return maybeNext(visitor.VisitUnary(n))
		case VarDeclAsgn:
			return maybeNext(visitor.VisitVarDeclAsgn(n))
		case *Literal:
			return maybeNext(visitor.VisitLiteral(n))
		case *Reference:
			return maybeNext(visitor.VisitReference(n))
		case ReturnStmt:
			return maybeNext(visitor.VisitReturnStmt(n))
		case Stmt:
			return maybeNext(visitor.VisitStmt(n))
		case Terminal:
			return maybeNext(visitor.VisitTerminal(n))
		case TypeParamDecl:
			return maybeNext(visitor.VisitTypeParamDecl(n))
		case *VarDecl:
			return maybeNext(visitor.VisitVarDecl(n))
		default:
			panic("??")
		}
	})
}
