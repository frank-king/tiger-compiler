//
// Created by whjpji on 18-7-21.
//

#ifndef TIGER_COMPILER_SLP_H
#define TIGER_COMPILER_SLP_H

#include <memory>
#include <string>
#include <ostream>

using std::string;
using std::unique_ptr;

namespace tiger::slp {
class Stmt;
class Expr;
class ExprList;
class BinOp;

class BaseElement {
public:
  friend std::ostream& operator<<(std::ostream& os,
                                  const BaseElement& elem) noexcept;
  virtual void print(std::ostream& os) const noexcept = 0;
};

class Stmt : public BaseElement {
};

class CompoundStmt : public Stmt {
public:
  explicit CompoundStmt(unique_ptr<Stmt>&& stmt1,
                        unique_ptr<Stmt>&& stmt2) noexcept
      : stmt1_(std::move(stmt1)), stmt2_(std::move(stmt2)) {}

  void print(std::ostream& os) const noexcept override;
protected:
  unique_ptr<Stmt> stmt1_, stmt2_;
};

class AssignStmt : public Stmt {
public:
  explicit AssignStmt(string&& id, unique_ptr<Expr>&& expr) noexcept
      : id_(std::forward<string>(id)), expr_(std::move(expr)) {}

  void print(std::ostream& os) const noexcept override;
protected:
  string id_;
  unique_ptr<Expr> expr_;
};

class PrintStmt : public Stmt {
public:
  explicit PrintStmt(unique_ptr<ExprList>&& exprs) noexcept
      : exprs_(std::move(exprs)) {}

  void print(std::ostream& os) const noexcept override;
protected:
  unique_ptr<ExprList> exprs_;
};

class Expr : public BaseElement {
};

class IdExpr : public Expr {
public:
  explicit IdExpr(string&& id) noexcept : id_(std::forward<string>(id)) {}
  explicit IdExpr(const char* id) noexcept : IdExpr(string(id)) {}

  void print(std::ostream& os) const noexcept override;
protected:
  string id_;
};

class NumExpr : public Expr {
public:
  constexpr explicit NumExpr(int num) noexcept : num_(num) {}

  void print(std::ostream& os) const noexcept override;
protected:
  int num_;
};

class OpExpr : public Expr {
public:
  explicit OpExpr(unique_ptr<Expr>&& left,
                  unique_ptr<BinOp>&& op,
                  unique_ptr<Expr>&& right) noexcept
      : left_(std::move(left)),
        op_(std::move(op)),
        right_(std::move(right)) {}

  void print(std::ostream& os) const noexcept override;
protected:
  unique_ptr<Expr> left_;
  unique_ptr<BinOp> op_;
  unique_ptr<Expr> right_;
};

class EseqExpr : public Expr {
public:
  explicit EseqExpr(unique_ptr<Stmt>&& stmt,
                    unique_ptr<Expr>&& expr) noexcept
      : stmt_(std::move(stmt)), expr_(std::move(expr)) {}

  void print(std::ostream& os) const noexcept override;
protected:
  unique_ptr<Stmt> stmt_;
  unique_ptr<Expr> expr_;
};

class ExprList : public BaseElement {
};

class PairExprList : public ExprList {
public:
  explicit PairExprList(unique_ptr<Expr>&& head,
                        unique_ptr<ExprList>&& tail) noexcept
      : head_(std::move(head)), tail_(std::move(tail)) {}

  void print(std::ostream& os) const noexcept override;
protected:
  unique_ptr<Expr> head_;
  unique_ptr<ExprList> tail_;
};

class LastExprList : public ExprList {
public:
  explicit LastExprList(unique_ptr<Expr>&& last) noexcept
      : last_(std::move(last)) {}

  void print(std::ostream& os) const noexcept override;
protected:
  unique_ptr<Expr> last_;
};

class BinOp : public BaseElement {
public:
  enum class OpType { kAdd, kSub, kMul, kDiv };
  constexpr BinOp(OpType type) noexcept : type_(type) {}
  void print(std::ostream& os) const noexcept override;

protected:
  OpType type_;
};
} // namespace tiger::slp

#endif //TIGER_COMPILER_SLP_H
