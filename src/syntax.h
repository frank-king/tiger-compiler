//
// Created by whjpji on 18-7-21.
//

#ifndef TIGER_COMPILER_SLP_H
#define TIGER_COMPILER_SLP_H

#include <memory>
#include <string>
#include <vector>
#include <ostream>
#include <unordered_map>
#include <deque>
#include "lexer.h"
#include "parser.h"

using std::vector;
using std::deque;
using std::string;
using std::unique_ptr;
using std::unordered_map;

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

class Ty;
class AbstractSyntax;

class BaseElement {
public:
  constexpr explicit BaseElement(const Position& pos) noexcept : pos_(pos) {}
  virtual bool checkType(AbstractSyntax *syntax) const = 0;

  friend std::ostream& operator<<(std::ostream& os,
                                  const BaseElement& elem) noexcept {
    elem.print(os);
    return os;
  }
  virtual void print(std::ostream& os) const = 0;
  virtual bool operator==(const BaseElement& other) const noexcept {
    return typeid(*this) == typeid(other) && pos_ == other.pos_;
  }
  bool operator!=(const BaseElement& other) const noexcept {
    return !operator==(other);
  }
  constexpr const Position& position() const noexcept { return pos_; }

protected:
  Position pos_;
};

class Program : public BaseElement {
public:
  using BaseElement::BaseElement;
  friend std::ostream& operator<<(std::ostream& os,
                                  const Program& program) noexcept {
    program.print(os);
    return os;
  }
};

class DeclList : public Program {
public:
  explicit DeclList(const Position& pos, vector<Declaration*>&& decls) noexcept
      : Program(pos), decls_(std::move(decls)) {}

  bool checkType(AbstractSyntax *syntax) const override;

  void print(std::ostream& os) const override;
  bool operator==(const BaseElement& _other) const noexcept override;

protected:
  vector<Declaration*> decls_;
};

class Expr : public Program {
public:
  using Program::Program;

  virtual Ty *evalType(AbstractSyntax *syntax) const = 0;
};

class NilExpr : public Expr {
public:
  using Expr::Expr;
  void print(std::ostream& os) const override;

  bool checkType(AbstractSyntax *syntax) const override { return false; }
};

class IntExpr : public Expr {
public:
  constexpr explicit IntExpr(const Position& pos, int value) noexcept
      : Expr(pos), value_(value) {}

  void print(std::ostream& os) const override;
  bool operator==(const BaseElement& other) const noexcept override {
    return Expr::operator==(other) &&
        value_ == dynamic_cast<decltype(*this)>(other).value_;
  }
  bool checkType(AbstractSyntax *syntax) const override { return true; }
  Ty *evalType(AbstractSyntax *syntax) const override;

protected:
  int value_;
};
class StrExpr : public Expr {
public:
  explicit StrExpr(const Position& pos, string value)
      : Expr(pos), value_(std::move(value)) {}

  void print(std::ostream& os) const override;
  bool operator==(const BaseElement& other) const noexcept override {
    return Expr::operator==(other) &&
        value_ == dynamic_cast<decltype(*this)>(other).value_;
  }
  bool checkType(AbstractSyntax *syntax) const override { return true; }
  Ty *evalType(AbstractSyntax *syntax) const override;

protected:
  string value_;
};
class ArrayExpr : public Expr {
public:
  explicit ArrayExpr(const Position& pos,
                     string type_name, Expr *size, Expr *init) noexcept
      : Expr(pos), typename_(std::move(type_name)), size_(size), init_(init) {}

  void print(std::ostream& os) const override;
  bool operator==(const BaseElement& _other) const noexcept override {
    if (!BaseElement::operator==(_other))
      return false;
    else {
      auto other = dynamic_cast<decltype(*this)>(_other);
      return typename_ == other.typename_ &&
          *size_ == *other.size_ && *init_ == *other.init_;
    }
  }
  bool checkType(AbstractSyntax *syntax) const override;
  Ty *evalType(AbstractSyntax *syntax) const override;

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
  bool operator==(const BaseElement& _other) const noexcept override;

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
  bool operator==(const BaseElement& _other) const noexcept override {
    if (!BaseElement::operator==(_other))
      return false;
    else {
      auto other = dynamic_cast<decltype(*this)>(_other);
      if (funcname_ != other.funcname_ || args_.size() != other.args_.size())
        return false;
      for (size_t i = 0; i < args_.size(); ++i)
        if (*args_[i] != *other.args_[i])
          return false;
      return true;
    }
  }

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
  bool operator==(const BaseElement& _other) const noexcept override {
    if (!BaseElement::operator==(_other))
      return false;
    else {
      auto other = dynamic_cast<decltype(*this)>(_other);
      return op_ == other.op_ && *lhs_ == *other.lhs_ && *rhs_ == *other.rhs_;
    }
  }

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
  bool operator==(const BaseElement& _other) const noexcept override;

protected:
  ExprList *exprs_;
};
class AssignExpr : public Expr {
public:
  constexpr explicit AssignExpr(
      const Position& pos, VarExpr *var, Expr *value) noexcept
      : Expr(pos), var_(var), value_(value)  {}

  void print(std::ostream& os) const override;
  bool operator==(const BaseElement& _other) const noexcept override;

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
  bool operator==(const BaseElement& _other) const noexcept override {
    if (!BaseElement::operator==(_other))
      return false;
    else {
      auto other = dynamic_cast<decltype(*this)>(_other);
      return *test_ == *other.test_ && *then_ == *other.then_ &&
          (!else_ && !other.else_ || *else_ == *other.else_);
    }
  }

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
  bool operator==(const BaseElement& _other) const noexcept override {
    if (!BaseElement::operator==(_other))
      return false;
    else {
      auto other = dynamic_cast<decltype(*this)>(_other);
      return *test_ == *other.test_ && *body_ == *other.body_;
    }
  }

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
  bool operator==(const BaseElement& _other) const noexcept override {
    if (!BaseElement::operator==(_other))
      return false;
    else {
      auto other = dynamic_cast<decltype(*this)>(_other);
      return varname_ == other.varname_ && *begin_ == *other.begin_ &&
          *end_ == *other.end_ && *body_ == *other.body_;
    }
  }

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
  bool operator==(const BaseElement& _other) const noexcept override;

protected:
  DeclList *declList_;
  ExprList *exprs_;
};

class ExprList : public BaseElement {
public:
  explicit ExprList(const Position& pos, vector<Expr*>&& exprs) noexcept
      : BaseElement(pos), exprs_(std::move(exprs)) {}

  void print(std::ostream& os) const override;
  bool operator==(const BaseElement& _other) const noexcept override {
    if (!BaseElement::operator==(_other))
      return false;
    else {
      auto other = dynamic_cast<decltype(*this)>(_other);
      if (exprs_.size() != other.exprs_.size())
        return false;
      for (size_t i = 0; i < exprs_.size(); ++i)
        if (*exprs_[i] != *other.exprs_[i])
          return false;
      return true;
    }
  }

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
  bool operator==(const BaseElement& _other) const noexcept override {
    if (!BaseElement::operator==(_other))
      return false;
    else {
      auto other = dynamic_cast<decltype(*this)>(_other);
      return name_ == other.name_;
    }
  }

protected:
  string name_;
};
class SubscriptVar : public VarExpr {
public:
  explicit SubscriptVar(const Position& pos, VarExpr *var, Expr *subscript) noexcept
      : VarExpr(pos), var_(var), subscript_(subscript) {}

  void print(std::ostream& os) const override;
  bool operator==(const BaseElement& _other) const noexcept override {
    if (!BaseElement::operator==(_other))
      return false;
    else {
      auto other = dynamic_cast<decltype(*this)>(_other);
      return *var_ == *other.var_ && *subscript_ == *other.subscript_;
    }
  }

protected:
  VarExpr *var_;
  Expr *subscript_;
};
class FieldVar : public VarExpr {
public:
  explicit FieldVar(const Position& pos, VarExpr *var, string field) noexcept
      : VarExpr(pos), var_(var), field_(std::move(field)) {}

  void print(std::ostream& os) const override;
  bool operator==(const BaseElement& _other) const noexcept override {
    if (!BaseElement::operator==(_other))
      return false;
    else {
      auto other = dynamic_cast<decltype(*this)>(_other);
      return *var_ == *other.var_ && field_ == other.field_;
    }
  }

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
  bool operator==(const BaseElement& _other) const noexcept override;

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
  bool operator==(const BaseElement& _other) const noexcept override {
    if (!BaseElement::operator==(_other))
      return false;
    else {
      auto other = dynamic_cast<decltype(*this)>(_other);
      return varname_ == other.varname_ &&
          typename_ == other.typename_ && *init_ == *other.init_;
    }
  }

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
  bool operator==(const BaseElement& _other) const noexcept override;

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
  bool operator==(const BaseElement& _other) const noexcept override {
    if (!BaseElement::operator==(_other))
      return false;
    else {
      auto other = dynamic_cast<decltype(*this)>(_other);
      return typename_ == other.typename_;
    }
  }

protected:
  string typename_;
};
class RecordType : public Type {
public:
  constexpr explicit RecordType(const Position& pos, FieldList *fields) noexcept
      : Type(pos), fields_(fields) {}

  void print(std::ostream& os) const override;
  bool operator==(const BaseElement& _other) const noexcept override;

protected:
  FieldList *fields_;
};
class ArrayType : public Type {
public:
  explicit ArrayType(const Position& pos, string type_name) noexcept
      : Type(pos), typename_(std::move(type_name)) {}

  void print(std::ostream& os) const override;
  bool operator==(const BaseElement& _other) const noexcept override {
    if (!BaseElement::operator==(_other))
      return false;
    else {
      auto other = dynamic_cast<decltype(*this)>(_other);
      return typename_ == other.typename_;
    }
  }

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
  bool operator==(const BaseElement& _other) const noexcept override {
    if (!BaseElement::operator==(_other))
      return false;
    else {
      auto other = dynamic_cast<decltype(*this)>(_other);
      return name_ == other.name_ && typename_ == other.typename_;
    }
  }

protected:
  string name_;
  string typename_;
};

class ValueField : public BaseElement {
public:
  explicit ValueField(const Position& pos, string name, Expr *value) noexcept
      : BaseElement(pos), name_(std::move(name)), value_(value) {}

  void print(std::ostream& os) const override;
  bool operator==(const BaseElement& _other) const noexcept override {
    if (!BaseElement::operator==(_other))
      return false;
    else {
      auto other = dynamic_cast<decltype(*this)>(_other);
      return name_ == other.name_ && *value_ == *other.value_;
    }
  }

protected:
  string name_;
  Expr *value_;
};

class FieldList : public BaseElement {
public:
  explicit FieldList(const Position& pos, vector<Field*>&& fields) noexcept :
      BaseElement(pos), fields_(std::move(fields)) {}

  void print(std::ostream& os) const override;
  bool operator==(const BaseElement& _other) const noexcept override {
    if (!BaseElement::operator==(_other))
      return false;
    else {
      auto other = dynamic_cast<decltype(*this)>(_other);
      if (fields_.size() != other.fields_.size())
        return false;
      for (size_t i = 0; i < fields_.size(); ++i)
        if (*fields_[i] != *other.fields_[i])
          return false;
      return true;
    }
  }

protected:
  vector<Field*> fields_;
};

class Ty {
public:
  virtual constexpr bool isInt() const noexcept { return false; }
  virtual constexpr bool isStr() const noexcept { return false; }
  virtual constexpr bool isRecord() const noexcept { return false; }
  virtual constexpr bool isArray() const noexcept { return false; }
  virtual constexpr bool isVoid() const noexcept { return false; }
  virtual constexpr bool is(const Ty *other) const noexcept {
    return other != nullptr && typeid(*this) == typeid(*other);
  }
};

class TyField {
public:
  explicit TyField(string name, Ty *type) noexcept
      : name_(std::move(name)), type_(type) {}
protected:
  string name_;
  Ty *type_;
};

class RecordTy : public Ty {
public:
  explicit RecordTy(vector<TyField>&& fields) noexcept
      : fields_(std::move(fields)) {}
  constexpr bool isRecord() const noexcept override { return true; }
  virtual constexpr bool is(const Ty *other) const noexcept {
    return false;
  }
protected:
  vector<TyField> fields_;
};

class ArrayTy : public Ty {
public:
  explicit ArrayTy(Ty *type) noexcept : type_(type) {}
  constexpr bool isArray() const noexcept override { return true; }
  virtual constexpr bool is(const Ty *other) const noexcept {
    return Ty::is(other) &&
        type_->is(dynamic_cast<const ArrayTy*>(other).type_);
  }
protected:
  Ty *type_;
};

class AliasTy : public Ty {
public:
  explicit AliasTy(string name, Ty *type) noexcept
      : name_(std::move(name)), type_(type) {}
  virtual constexpr bool isInt() const noexcept { return type_->isInt(); }
  virtual constexpr bool isStr() const noexcept { return type_->isStr(); }
  virtual constexpr bool isRecord() const noexcept { return type_->isRecord(); }
  virtual constexpr bool isArray() const noexcept { return type_->isArray(); }
  virtual constexpr bool is(const Ty *other) const noexcept { return other->is(type_); }
protected:
  string name_;
  Ty *type_;
};

struct IntTy : public Ty {
  virtual constexpr bool isInt() const noexcept { return true; }
};
struct StrTy : public Ty {
  virtual constexpr bool isStr() const noexcept { return true; }
};
struct VoidTy : public Ty {
  virtual constexpr bool isVoid() const noexcept { return true; }
};
struct NilTy : public Ty {
  virtual constexpr bool is(const Ty *other) const noexcept { return false; }
};

class Func {
public:
  explicit Func(vector<TyField>&& fields, Ty *result) noexcept
      : fields_(std::move(fields)), result_(result) {}
protected:
  vector<TyField> fields_;
  Ty *result_;
};

class SymbolTable {
public:
  Ty *type(const string& key) const {
    if (auto it = types_.find(key); it != types_.end())
      return it->second;
    else
      return nullptr;
  }
  Ty *var(const string& key) const {
    if (auto it = vars_.find(key); it != vars_.end())
      return it->second;
    else
      return nullptr;
  }
  Func *func(const string& key) const {
    if (auto it = funcs_.find(key); it != funcs_.end())
      return it->second;
    else
      return nullptr;
  }

  void type(const string& key, Ty *type) { types_.emplace(key, type); }
  void var(const string& key, Ty *var) { vars_.emplace(key, var); }
  void func(const string& key, Func *func) { funcs_.emplace(key, func); }

protected:
  unordered_map<string, Ty*> types_;
  unordered_map<string, Ty*> vars_;
  unordered_map<string, Func*> funcs_;
};

class AbstractSyntax {
public:
  constexpr explicit AbstractSyntax() noexcept
      : types_({std::make_unique<VoidTy>(),
                std::make_unique<IntTy>(),
                std::make_unique<StrTy>(),
                std::make_unique<NilTy>()}),
        program_(nullptr) { }
  template <typename T, typename... Args>
  T *elem(Args&&... args) {
    elements_.emplace_back(std::make_unique<T>(std::forward<Args&&>(args)...));
    return dynamic_cast<T*>(elements_.back().get());
  }

  Program *abstract(const grammar::ParsedTerm *term);
  bool checkType();

  Ty *typeOf(const string& name) const {
    for (const auto& table : symbolTables_)
      if (auto ty = table.type(name))
        return ty;
    return nullptr;
  }

  Ty *varOf(const string& name) const {
    for (const auto& table : symbolTables_)
      if (auto ty = table.var(name))
        return ty;
    return nullptr;
  }

  Func *funcOf(const string& name) const {
    for (const auto& table : symbolTables_)
      if (auto ty = table.func(name))
        return ty;
    return nullptr;
  }

  void pushSymbolTable() { symbolTables_.emplace_front(); }
  void popSymbolTable() { symbolTables_.pop_front(); }

  constexpr Ty *voidTy() const noexcept { return types_[0].get(); }
  constexpr Ty *intTy() const noexcept { return types_[1].get(); }
  constexpr Ty *strTy() const noexcept { return types_[2].get(); }
  constexpr Ty *nilTy() const noexcept { return types_[3].get(); }
  Ty *recordTy(vector<TyField>&& fields) {
    return types_.emplace_back(std::make_unique<RecordTy>(std::move(fields))).get();
  }
  Ty *arrayTy(Ty *type) {
    return types_.emplace_back(std::make_unique<ArrayTy>(type)).get();
  }
  Ty *aliasTy(string name, Ty *type) {
    return types_.emplace_back(std::make_unique<AliasTy>(std::move(name), type)).get();
  }
  Func *func(vector<TyField>&& fields, Ty *result) {
    return funcs_.emplace_back(std::make_unique<Func>(std::move(fields), result)).get();
  }

protected:
  deque<SymbolTable> symbolTables_;
  vector<unique_ptr<BaseElement>> elements_;
  vector<unique_ptr<Ty>> types_;
  vector<unique_ptr<Func>> funcs_;
  Program *program_;
};

} // namespace tiger::syntax

#endif //TIGER_COMPILER_SLP_H
