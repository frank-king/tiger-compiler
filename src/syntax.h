//
// Created by whjpji on 18-7-21.
//

#ifndef TIGER_COMPILER_SLP_H
#define TIGER_COMPILER_SLP_H

#include <memory>
#include <string>
#include <vector>
#include <ostream>
#include "lexer.h"
#include "parser.h"

using std::vector;
using std::string;
using std::unique_ptr;

namespace tiger::grammar {
class ParsedTerm;
}

namespace tiger::syntax {
using lex::Position;

class BaseElement;

class Program;
class DeclList;
class Expr;

class Expr;
class NilExpr;
class IntExpr;
class StrExpr;
class ArrayExpr;
class RecordExpr;
// class NewExpr;
class VarExpr;
class CallExpr;
// class MethodCallExpr;
class OpExpr;
class SeqExpr;
class AssignExpr;
class IfExpr;
class WhileExpr;
class ForExpr;
class BreakExpr;
class LetExpr;

class ExprList;

class VarExpr;
class SimpleVar;
class SubscriptVar;
class FieldVar;

class DeclList;
class Declaration;

class Declaration;
class TypeDecl;
// class ClassType;
class VarDecl;
class FuncDecl;
// class PrimDecl;
// class ImportDecl;

// class ClassFieldList;
// class ClassField;
// class ClassAttr;
// class ClassMethod;

class Type;
class AliasType;
class RecordType;
class ArrayType;
// class ClassType;

class Field;
class ValueField;
class FieldList;

/*
class Decl;
class Stmt;
class Expr;
class ExprList;
class BinOp;
*/


class BaseElement {
public:
  constexpr explicit BaseElement(const Position& pos) noexcept : pos_(pos) {}
  friend std::ostream& operator<<(std::ostream& os,
                                  const BaseElement& elem) noexcept {
    elem.print(os);
    return os;
  }
  virtual void print(std::ostream& os) const = 0;

protected:
  Position pos_;
};

class Program : public BaseElement {
public:
  using BaseElement::BaseElement;
};

class DeclList : public Program {
public:
  explicit DeclList(const Position& pos, vector<Declaration*>&& decls) noexcept
      : Program(pos), decls_(std::move(decls)) {}

  void print(std::ostream& os) const override;

protected:
  vector<Declaration*> decls_;
};

class Expr : public Program {
public:
  using Program::Program;
};

class NilExpr : public Expr {
public:
  using Expr::Expr;
  void print(std::ostream& os) const override;
};

class IntExpr : public Expr {
public:
  constexpr explicit IntExpr(const Position& pos, int value) noexcept
      : Expr(pos), value_(value) {}

  void print(std::ostream& os) const override;

protected:
  int value_;
};
class StrExpr : public Expr {
public:
  explicit StrExpr(const Position& pos, string value)
      : Expr(pos), value_(std::move(value)) {}

  void print(std::ostream& os) const override;

protected:
  string value_;
};
class ArrayExpr : public Expr {
public:
  explicit ArrayExpr(const Position& pos,
                     string type_name, Expr *size, Expr *init) noexcept
      : Expr(pos), typename_(std::move(type_name)), size_(size), init_(init) {}

  void print(std::ostream& os) const override;

protected:
  string typename_;
  Expr *size_;
  Expr *init_;
};
class RecordExpr : public Expr {
public:
  explicit RecordExpr(
      const Position& pos, string type_name, vector<ValueField*>&& fields) noexcept
      : Expr(pos), typename_(std::move(type_name)), fields_(std::move(fields)) {}

  void print(std::ostream& os) const override;

protected:
  string typename_;
  vector<ValueField*> fields_;
};
// class NewExpr : public Expr {};
class CallExpr : public Expr {
public:
  explicit CallExpr(const Position& pos, string funcname, vector<Expr*>&& args) noexcept
      : Expr(pos), funcname_(std::move(funcname)), args_(std::move(args)) {}

  void print(std::ostream& os) const override;

protected:
  string funcname_;
  vector<Expr*> args_;
};
// class MethodCallExpr : public Expr {};
class OpExpr : public Expr {
public:
  enum Kind {
    PLUS, MINUS, TIMES, DIVIDE,
    EQ, NEQ, LT, LE, GT, GE
  };

  constexpr explicit OpExpr(const Position& pos, Kind op, Expr *lhs, Expr *rhs) noexcept
      : Expr(pos), op_(op), lhs_(lhs), rhs_(rhs) {}

  void print(std::ostream& os) const override;

protected:
  Kind op_;
  Expr *lhs_;
  Expr *rhs_;
};
class SeqExpr : public Expr {
public:
  constexpr explicit SeqExpr(const Position& pos, ExprList *exprs) noexcept
    : Expr(pos), exprs_(exprs) {}

  void print(std::ostream& os) const override;

protected:
  ExprList *exprs_;
};
class AssignExpr : public Expr {
public:
  constexpr explicit AssignExpr(
      const Position& pos, VarExpr *var, Expr *value) noexcept
      : Expr(pos), var_(var), value_(value)  {}

  void print(std::ostream& os) const override;

protected:
  VarExpr *var_;
  Expr *value_;
};
class IfExpr : public Expr {
public:
  constexpr explicit IfExpr(const Position& pos,
                            Expr *test, Expr *then, Expr *elsee) noexcept
      : Expr(pos), test_(test), then_(then), else_(elsee) {}

  void print(std::ostream& os) const override;

protected:
  Expr *test_;
  Expr *then_;
  Expr *else_;
};
class WhileExpr : public Expr {
public:
  constexpr explicit WhileExpr(
      const Position& pos, Expr *test, Expr *body) noexcept
      : Expr(pos), test_(test), body_(body) {}

  void print(std::ostream& os) const override;

protected:
  Expr *test_;
  Expr *body_;
};
class ForExpr : public Expr {
public:
  explicit ForExpr(const Position& pos, string varname,
                   Expr *begin, Expr *end, Expr *body) noexcept
      : Expr(pos), varname_(std::move(varname)),
        begin_(begin), end_(end), body_(body) {}

  void print(std::ostream& os) const override;

protected:
  string varname_;
  Expr *begin_;
  Expr *end_;
  Expr *body_;
};
class BreakExpr : public Expr {
public:
  using Expr::Expr;
  void print(std::ostream& os) const override;
};
class LetExpr : public Expr {
public:
  constexpr explicit LetExpr(
      const Position& pos, DeclList *declList, ExprList *exprs) noexcept
      : Expr(pos), declList_(declList), exprs_(exprs) {}

  void print(std::ostream& os) const override;

protected:
  DeclList *declList_;
  ExprList *exprs_;
};

class ExprList : public BaseElement {
public:
  explicit ExprList(const Position& pos, vector<Expr*>&& exprs) noexcept
      : BaseElement(pos), exprs_(std::move(exprs)) {}

  void print(std::ostream& os) const override;

protected:
  vector<Expr*> exprs_;
};

class VarExpr : public Expr {
public:
  using Expr::Expr;
};

class SimpleVar : public VarExpr {
public:
  explicit SimpleVar(const Position& pos, string name)
      : VarExpr(pos), name_(std::move(name)) {}

  void print(std::ostream& os) const override;

protected:
  string name_;
};
class SubscriptVar : public VarExpr {
public:
  explicit SubscriptVar(const Position& pos, VarExpr *var, Expr *subscript) noexcept
      : VarExpr(pos), var_(var), subscript_(subscript) {}

  void print(std::ostream& os) const override;

protected:
  VarExpr *var_;
  Expr *subscript_;
};
class FieldVar : public VarExpr {
public:
  explicit FieldVar(const Position& pos, VarExpr *var, string field) noexcept
      : VarExpr(pos), var_(var), field_(std::move(field)) {}

  void print(std::ostream& os) const override;

protected:
  VarExpr *var_;
  string field_;
};

class Declaration : public BaseElement {
public:
  using BaseElement::BaseElement;
};

class TypeDecl : public Declaration {
public:
  explicit TypeDecl(const Position& pos, string name, Type *type) noexcept
      : Declaration(pos), name_(std::move(name)), type_(type) {}

  void print(std::ostream& os) const override;

protected:
  string name_;
  Type *type_;
};
// class ClassType : public Declaration {};
class VarDecl : public Declaration {
public:
  explicit VarDecl(const Position& pos, string varname,
                   string type_name, Expr *init) noexcept
      : Declaration(pos), varname_(std::move(varname)),
        typename_(std::move(type_name)), init_(init) {}

  void print(std::ostream& os) const override;

protected:
  string varname_;
  string typename_;
  Expr *init_;
};
class FuncDecl : public Declaration {
public:
  explicit FuncDecl(const Position& pos, string name,
                    FieldList *params, string result, Expr *body) noexcept
      : Declaration(pos), name_(std::move(name)),
        params_(params), result_(std::move(result)), body_(body) {}

  void print(std::ostream& os) const override;

protected:
  string name_;
  FieldList *params_;
  string result_;
  Expr *body_;
};
// class PrimDecl : public Declaration {};
// class ImportDecl : public Declaration {};

// class ClassField : public BaseElement {};
// class ClassAttr : public ClassField {};
// class ClassMethod : public ClassField {};

class Type : public BaseElement {
public:
  using BaseElement::BaseElement;
};
class AliasType : public Type {
public:
  explicit AliasType(const Position& pos, string type_name) noexcept
      : Type(pos), typename_(std::move(type_name)) {}

  void print(std::ostream& os) const override;

protected:
  string typename_;
};
class RecordType : public Type {
public:
  constexpr explicit RecordType(const Position& pos, FieldList *fields) noexcept
      : Type(pos), fields_(fields) {}

  void print(std::ostream& os) const override;

protected:
  FieldList *fields_;
};
class ArrayType : public Type {
public:
  explicit ArrayType(const Position& pos, string type_name) noexcept
      : Type(pos), typename_(std::move(type_name)) {}

  void print(std::ostream& os) const override;

protected:
  string typename_;
};
// class ClassType : public Type {};

class Field : public BaseElement {
public:
  explicit Field(const Position& pos, string name, string type_name) noexcept
      : BaseElement(pos),
        name_(std::move(name)), typename_(std::move(type_name)) {}

  void print(std::ostream& os) const override;

protected:
  string name_;
  string typename_;
};

class ValueField : public BaseElement {
public:
  explicit ValueField(const Position& pos, string name, Expr *value) noexcept
      : BaseElement(pos), name_(std::move(name)), value_(value) {}

  void print(std::ostream& os) const override;

protected:
  string name_;
  Expr *value_;
};

class FieldList : public BaseElement {
public:
  explicit FieldList(const Position& pos, vector<Field*>&& fields) noexcept :
      BaseElement(pos), fields_(std::move(fields)) {}

  void print(std::ostream& os) const override;

protected:
  vector<Field*> fields_;
};

class AbstractSyntax {
public:
  template <typename T, typename... Args>
  T *elem(Args&&... args) {
    elements_.emplace_back(std::make_unique<T>(std::forward<Args&&>(args)...));
    return dynamic_cast<T*>(elements_.back().get());
  }

  Program *abstract(const grammar::ParsedTerm *term);

protected:
  vector<unique_ptr<BaseElement>> elements_;
  Program *program_;
};

} // namespace tiger::syntax

#endif //TIGER_COMPILER_SLP_H
