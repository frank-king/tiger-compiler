//
// Created by whjpji on 18-7-21.
//

#include <regex>
#include "util.h"
#include "syntax.h"

namespace tiger::syntax {
using grammar::ParsedTerm;

void DeclList::print(std::ostream& os) const {
  for (auto decl : decls_)
    os << *decl << "\n";
}

void NilExpr::print(std::ostream& os) const { os << "nil"; }
void IntExpr::print(std::ostream& os) const { os << value_; }
void StrExpr::print(std::ostream& os) const { os << value_; }

void ArrayExpr::print(std::ostream& os) const {
  os << typename_ << "[" << *size_ << "] of " << *init_;
}

void RecordExpr::print(std::ostream& os) const {
  os << "{";
  printWithDelimiter(os, fields_, ", ", [&os](ValueField *field) mutable {
    os << *field;
  });
  os << "}";
}

void CallExpr::print(std::ostream& os) const {
  os << funcname_ << "(";
  printWithDelimiter(os, args_, ", ", [&os](Expr *expr) mutable {
    os << *expr;
  });
  os << ")";
}

void OpExpr::print(std::ostream& os) const {
  os << *lhs_ << " ";
  switch (op_) {
  case PLUS: os << "+"; break;
  case MINUS: os << "-"; break;
  case TIMES: os << "*"; break;
  case DIVIDE: os << "/"; break;
  case EQ: os << "="; break;
  case NEQ: os << "<>"; break;
  case LT: os << "<"; break;
  case LE: os << "<="; break;
  case GT: os << ">"; break;
  case GE: os << ">="; break;
  default: break;
  }
  os << " " << *rhs_ ;
}

void SeqExpr::print(std::ostream& os) const {
  os << "(\n" << *exprs_ << "\n)";
}

void IfExpr::print(std::ostream& os) const {
  os << "(if " << *test_ << " then " << then_;
  if (else_) {
    os << " else " << else_;
  }
  os << ")";
}

void WhileExpr::print(std::ostream& os) const {
  os << "while " << *test_ << " do " << *body_;
}

void ForExpr::print(std::ostream& os) const {
  os << "for " << varname_ << " := " << *begin_ << " to " << end_ << " do " << *body_;
}

void BreakExpr::print(std::ostream& os) const { os << "break"; }

void LetExpr::print(std::ostream& os) const {
  os << "let\n" << *declList_ << "in\n" << *exprs_ << "end";
}

void SimpleVar::print(std::ostream& os) const { os << name_; }

void SubscriptVar::print(std::ostream& os) const {
  os << *var_ << "[" << *subscript_ << "]";
}
void FieldVar::print(std::ostream& os) const {
  os << *var_ << "." << field_;
}
void TypeDecl::print(std::ostream& os) const {
  os << "type " << name_ << " = " << *type_;
}
void VarDecl::print(std::ostream& os) const {
  os << "var " << varname_ << ": " << typename_ << " = " << *init_;
}
void FuncDecl::print(std::ostream& os) const {
  os << "function " << name_ << "(" << *params_ << "): "
      << result_ << " = " << *body_;
}

void AliasType::print(std::ostream& os) const { os << typename_; }

void RecordType::print(std::ostream& os) const {
  os << "{" << *fields_ << "}";
}

void ArrayType::print(std::ostream& os) const {
  os << "array of " << typename_;
}

void Field::print(std::ostream& os) const {
  os << name_ << ": " << typename_;
}

void FieldList::print(std::ostream& os) const {
  os << "{";
  printWithDelimiter(os, fields_, ", ", [&os](Field *field) mutable {
    os << *field;
  });
  os << "}";

}
void ValueField::print(std::ostream& os) const {
  os << name_ << ": " << *value_;
}
void AssignExpr::print(std::ostream& os) const {
  os << *var_ << " := " << *value_;
}
void ExprList::print(std::ostream& os) const {
  printWithDelimiter(os, exprs_, ";\n", [&os](Expr *expr) mutable {
    os << *expr;
  });
}
Program *AbstractSyntax::abstract(const ParsedTerm *term) {
  return program_ = term->abstract<Program>(*this);
}
} // namespace tiger::syntax
