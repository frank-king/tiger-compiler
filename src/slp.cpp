//
// Created by whjpji on 18-7-21.
//

#include "slp.h"

namespace tiger::slp {
std::ostream& operator<<(std::ostream& os, const BaseElement& elem) noexcept {
  elem.print(os);
  return os;
}

void CompoundStmt::print(std::ostream& os) const noexcept {
  stmt1_->print(os);
  os << ";" << std::endl;
  stmt2_->print(os);
}

void AssignStmt::print(std::ostream& os) const noexcept {
  os << id_ << " := ";
  expr_->print(os);
}

void PrintStmt::print(std::ostream& os) const noexcept {
  os << "print(";
  exprs_->print(os);
  os << ")";
}

void IdExpr::print(std::ostream& os) const noexcept { os << id_; }

void NumExpr::print(std::ostream& os) const noexcept { os << num_; }

void OpExpr::print(std::ostream& os) const noexcept {
  left_->print(os);
  os << " ";
  op_->print(os);
  os << " ";
  right_->print(os);
}

void EseqExpr::print(std::ostream& os) const noexcept {
  os << "(";
  stmt_->print(os);
  os << ", ";
  expr_->print(os);
  os << ")";
}

void PairExprList::print(std::ostream& os) const noexcept {
  head_->print(os);
  os << ", ";
  tail_->print(os);
}

void LastExprList::print(std::ostream& os) const noexcept { last_->print(os); }

void BinOp::print(std::ostream& os) const noexcept {
  switch (type_) {
  case OpType::kAdd: os << "+";
    break;
  case OpType::kSub: os << "-";
    break;
  case OpType::kMul: os << "*";
    break;
  case OpType::kDiv: os << "/";
    break;
  }
}
} // namespace tiger::slp
