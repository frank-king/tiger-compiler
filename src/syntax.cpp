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
void StrExpr::print(std::ostream& os) const {
  os << "\"";
  for (auto ch : value_) {
    switch (ch) {
    case '\n': os << "\\n"; break;
    case '\t': os << "\\t"; break;
    case '\"': os << "\\\""; break;
    case '\v': os << "\\v"; break;
    case '\\': os << "\\\\"; break;
    default:
      if (isprint(ch))
        os << ch;
      else
        os << "\\" << static_cast<int>(ch);
      break;
    }
  }
  os << "\"";
}

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
  os << "(if " << *test_ << " then " << *then_;
  if (else_) {
    os << " else " << *else_;
  }
  os << ")";
}

void WhileExpr::print(std::ostream& os) const {
  os << "while " << *test_ << " do " << *body_;
}

void ForExpr::print(std::ostream& os) const {
  os << "for " << varname_ << " := " << *begin_ << " to " << *end_ << " do " << *body_;
}

void BreakExpr::print(std::ostream& os) const { os << "break"; }

void LetExpr::print(std::ostream& os) const {
  os << "let\n" << *declList_ << "\nin\n" << *exprs_ << "\nend";
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

bool DeclList::operator==(const BaseElement& _other) const noexcept {
  if (!BaseElement::operator==(_other))
    return false;
  else {
    auto other = dynamic_cast<decltype(*this)>(_other);
    if (decls_.size() != other.decls_.size())
      return false;
    for (size_t i = 0; i < decls_.size(); ++i)
      if (*decls_[i] != *other.decls_[i])
        return false;
    return true;
  }
}
bool RecordExpr::operator==(const BaseElement& _other) const noexcept {
  if (!BaseElement::operator==(_other))
    return false;
  else {
    auto other = dynamic_cast<decltype(*this)>(_other);
    if (typename_ != other.typename_ || fields_.size() != other.fields_.size())
      return false;
    for (size_t i = 0; i < fields_.size(); ++i)
      if (!(*fields_[i] == *other.fields_[i]))
        return false;
    return true;
  }
}
bool SeqExpr::operator==(const BaseElement& _other) const noexcept {
  if (!BaseElement::operator==(_other))
    return false;
  else {
    auto other = dynamic_cast<decltype(*this)>(_other);
    return *exprs_ == *other.exprs_;
  }
}
bool AssignExpr::operator==(const BaseElement& _other) const noexcept {
  if (!BaseElement::operator==(_other))
    return false;
  else {
    auto other = dynamic_cast<decltype(*this)>(_other);
    return *var_ == *other.var_ && *value_ == *other.value_;
  }
}
bool LetExpr::operator==(const BaseElement& _other) const noexcept {
  if (!BaseElement::operator==(_other))
    return false;
  else {
    auto other = dynamic_cast<decltype(*this)>(_other);
    return *declList_ == *other.declList_ && *exprs_ == *other.exprs_;
  }
}
bool TypeDecl::operator==(const BaseElement& _other) const noexcept {
  if (!BaseElement::operator==(_other))
    return false;
  else {
    auto other = dynamic_cast<decltype(*this)>(_other);
    return name_ == other.name_ && *type_ == *other.type_;
  }
}
bool FuncDecl::operator==(const BaseElement& _other) const noexcept {
  if (!BaseElement::operator==(_other))
    return false;
  else {
    auto other = dynamic_cast<decltype(*this)>(_other);
    return name_ == other.name_ && *params_ == *other.params_ &&
        result_ == other.result_ && *body_ == *other.body_;
  }
}
bool RecordType::operator==(const BaseElement& _other) const noexcept {
  if (!BaseElement::operator==(_other))
    return false;
  else {
    auto other = dynamic_cast<decltype(*this)>(_other);
    return *fields_ == *other.fields_;
  }
}

Program *AbstractSyntax::abstract(const ParsedTerm *term) {
  return program_ = term->abstract<Program>(*this);
}
bool AbstractSyntax::checkType() {
  return program_->checkType(this);
}

bool DeclList::checkType(AbstractSyntax *syntax) const {
  return std::all_of(decls_.begin(), decls_.end(), [](Declaration *decl) {
    return decl->checkType(syntax);
  });
}
Ty *IntExpr::evalType(AbstractSyntax *syntax) const { return syntax->intTy(); }
Ty *StrExpr::evalType(AbstractSyntax *syntax) const { return syntax->strTy(); }

bool ArrayExpr::checkType(AbstractSyntax *syntax) const {
  return size_->checkType(syntax) && size_->evalType(syntax)->isInt() &&
      init_->checkType(syntax) && init_->evalType(syntax)->is(syntax->typeOf(typename_));
}
Ty *ArrayExpr::evalType(AbstractSyntax *syntax) const {
  return nullptr;
}
} // namespace tiger::syntax
