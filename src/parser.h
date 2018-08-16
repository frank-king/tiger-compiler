//
// Created by whjpji on 18-7-23.
//

#ifndef TIGER_COMPILER_PARSER_H
#define TIGER_COMPILER_PARSER_H

#include <memory>
#include <vector>
#include <deque>
#include <unordered_map>
#include <unordered_set>
#include <ostream>
#include <valarray>
#include <functional>
#include "lexer.h"
#include "syntax.h"
#include "util.h"

using std::vector;
using std::valarray;
using std::deque;
using std::unique_ptr;
using std::unordered_map;
using std::unordered_set;
using std::function;

namespace tiger::syntax {
class BaseElement;
class AbstractSyntax;
}

namespace tiger::grammar {
using lex::Token;


class GrammarSymbol {
public:
  virtual bool isTerminal() const noexcept = 0;
  virtual bool isNonTerminal() const noexcept = 0;
  virtual string name() const noexcept = 0;
  virtual bool operator==(const GrammarSymbol& rhs) const noexcept {
    return name() == rhs.name();
  }
  bool operator!=(const GrammarSymbol& rhs) const noexcept {
    return !operator==(rhs);
  }
  friend std::ostream &operator<<(std::ostream &os, const GrammarSymbol &symbol) {
#ifndef NDEBUG
    symbol.indentedPrint(os);
#else
    symbol.print(os);
#endif
    return os;
  }
  virtual void indentedPrint(std::ostream& os, size_t indent = 0) const {
    printIndent(os, indent);
    os << name();
  }
  virtual void print(std::ostream& os, size_t indent = 0) const {
    os << name();
  }
  virtual lex::Position position() const noexcept {
    return lex::Position();
  }

protected:
  static void printIndent(std::ostream& os, size_t indent) {
    for (size_t i = 0; i < indent; ++i)
      os << "\t";
  }
};

class Terminal : public GrammarSymbol {
public:
  explicit Terminal(Token token) : token_(std::move(token)) {}
  explicit Terminal(Terminal&& other) noexcept = default;
  explicit Terminal(const Terminal& other) = default;
  bool isTerminal() const noexcept override { return true; }
  bool isNonTerminal() const noexcept override { return false; }
  string name() const noexcept override { return token_.name(); }
  constexpr const Token& token() const noexcept { return token_; }

  friend std::ostream& operator<<(std::ostream& os, const Terminal& terminal) {
#ifndef NDEBUG
    terminal.indentedPrint(os);
#else
    terminal.print(os);
#endif
    return os;
  }
  void indentedPrint(std::ostream& os, size_t indent = 0) const override {
    printIndent(os, indent);
    os << token_;
  }
  void print(std::ostream& os, size_t indent = 0) const override {
    os << token_;
  }
  bool operator==(const GrammarSymbol& rhs) const noexcept override {
    return GrammarSymbol::operator==(rhs) && rhs.isTerminal() &&
        operator==(dynamic_cast<const Terminal&>(rhs));
  }
  bool operator==(const Terminal& rhs) const noexcept {
    return token_ == rhs.token_;
  }
  lex::Position position() const noexcept override {
    return token_.position();
  }

protected:
  Token token_;
};

class NonTerminal : public GrammarSymbol {
public:
  explicit NonTerminal(string name, bool isClosure = false)
      : name_(std::move(name)), isClosure_(isClosure) {}
  explicit NonTerminal(const char_t *name, bool isClosure = false)
      : name_(name), isClosure_(isClosure) {}
  explicit NonTerminal(NonTerminal&& other) noexcept = default;
  explicit NonTerminal(const NonTerminal& other) = default;
  bool isTerminal() const noexcept override { return false; }
  bool isNonTerminal() const noexcept override { return true; }
  constexpr bool isClosure() const noexcept { return isClosure_; }
  string name() const noexcept override { return name_; }
  friend std::ostream& operator<<(std::ostream& os, const NonTerminal& nonterminal) {
    nonterminal.indentedPrint(os);
    return os;
  }
  constexpr void nullable(bool nullable) noexcept { nullable_ = nullable; }
  constexpr bool nullable() const noexcept { return nullable_; }

protected:
  string name_;
  bool nullable_ = false;
  bool isClosure_ = false;
};

class Production;

class ParsedTerm : public NonTerminal {
public:
  // explicit ParsedTerm(string name) : NonTerminal(std::move(name)) {}
  // explicit ParsedTerm(const char_t *name) : NonTerminal(name) {}
  explicit ParsedTerm(const NonTerminal& other, const Production *prod = nullptr)
      : NonTerminal(other), prod_(prod) {}
  explicit ParsedTerm(ParsedTerm&& other) = default;
  explicit ParsedTerm(string name, deque<GrammarSymbol*>&& syms,
                      const Production *prod = nullptr)
      : NonTerminal(std::move(name)) {
    for (auto sym : syms) {
      children_.emplace_back(sym);
    }
  }

  void addChildFront(unique_ptr<GrammarSymbol>&& child) {
    children_.emplace_front(std::move(child));
  }
  void addChildBack(unique_ptr<GrammarSymbol>&& child) {
    children_.emplace_back(std::move(child));
  }

  const auto& children() const & noexcept { return children_; }
  auto children() && noexcept { return std::move(children_); }
  // GrammarSymbol *operator[](size_t index) const noexcept { return children_[index].get(); }
  bool empty() const noexcept { return  children_.empty(); }
  constexpr const Production *production() const noexcept { return prod_; }


  template <typename T>
  const T *get(size_t index) const {
    return dynamic_cast<T*>(children_[index].get());
  }

  lex::Position position() const noexcept {
    if (children_.empty())
      return lex::Position();
    else
      return children_.front()->position();
  }

  template <typename T = syntax::BaseElement>
  T *abstract(syntax::AbstractSyntax& absSyntax) const;

  friend std::ostream& operator<<(std::ostream& os, const ParsedTerm& term) {
#ifndef NDEBUG
    term.indentedPrint(os);
#else
    term.print(os);
#endif
    return os;
  }
  void indentedPrint(std::ostream& os, size_t indent = 0) const override {
    printIndent(os, indent);
    os << name() << "(";
    if (children_.size() == 1 && children_.back()->isTerminal()) {
      os << *children_.back();
    } else {
      os << "\n";
      for (size_t i = 0; i < children_.size(); ++i) {
        children_[i]->print(os, indent + 1);
        os << "\n";
      }
      printIndent(os, indent);
    }
    os << ")";
  }
  void print(std::ostream& os, size_t indent = 0) const override {
    os << name() << "(";
    for (size_t i = 0; i + 1 < children_.size(); ++i)
      os << *children_[i] << " ";
    if (!children_.empty())
      os << *children_.back();
    os << ")";
  }

  bool operator==(const GrammarSymbol& rhs) const noexcept override {
    return GrammarSymbol::operator==(rhs) && rhs.isNonTerminal() &&
        operator==(dynamic_cast<const ParsedTerm&>(rhs));
  }
  bool operator!=(const ParsedTerm& rhs) const noexcept {
    return !(rhs == *this);
  }
  bool operator==(const ParsedTerm& rhs) const noexcept {
    if (children_.size() != rhs.children_.size())
      return false;
    for (auto it = children_.cbegin(), itr = rhs.children_.cbegin();
         it != children_.cend() && itr != rhs.children_.cend(); ++it, ++itr)
      if (**it != **itr)
        return false;
    return true;
  }

protected:
  deque<unique_ptr<GrammarSymbol>> children_;
  const Production *prod_;
};

class Production {
public:
  using SemanticFunc = function<
      syntax::BaseElement*(const ParsedTerm*, syntax::AbstractSyntax& syntax)>;
  enum Associative : int8_t { NONE = -1, LEFT, RIGHT };
  enum PriorityClass : uint8_t {
    UNDEF = 0x7f,
    PROTECTED = 0,
    FIRST, SECOND, THIRD, FOURTH, FIFTH, SIXTH, SEVENTH, EIGHTH, NINTH, TENTH,
  };
  enum PriorityComp { DEFAULT = -2, ERROR = -1, HIGHER, LOWER, };

  struct Attribute {
    Associative assoc;
    PriorityClass prior;

    constexpr Attribute() noexcept : assoc(RIGHT), prior(UNDEF) {}
    constexpr Attribute(Associative assoc, PriorityClass prior) noexcept
        : assoc(assoc), prior(prior) {}
  };

  Production(NonTerminal *head, vector<GrammarSymbol*>&& body,
             SemanticFunc&& semantic)
      : head_(head), body_(std::move(body)), semantic_(std::move(semantic)) {}
  Production(NonTerminal *head, vector<GrammarSymbol*>&& body)
      : head_(head), body_(std::move(body)) {}
  Production(NonTerminal *head, vector<GrammarSymbol*>&& body,
             SemanticFunc&& semantic, Attribute attr)
      : head_(head), body_(std::move(body)), semantic_(std::move(semantic)),
        attr_(attr) {}
  Production(NonTerminal *head, vector<GrammarSymbol*>&& body, Attribute attr)
      : head_(head), body_(std::move(body)), attr_(attr) {}
  Production(const Production& other) = default;
  Production(Production&& other) noexcept = default;
  Production& operator=(const Production& other) = default;
  Production& operator=(Production&& other) noexcept = default;
  size_t size() const noexcept { return body_.size(); }
  bool empty() const noexcept { return body_.empty(); }
  GrammarSymbol *operator[](size_t index) const noexcept { return body_[index]; }
  constexpr NonTerminal *head() const noexcept { return head_; }
  friend std::ostream& operator<<(std::ostream& os,
                                  const Production& production) {
    os << production.head_->name() << " ::=";
    for (auto sym : production.body_) {
      os << " " << *sym;
    }
    return os;
  }

  bool operator==(const Production& rhs) const {
    if (head_->name() != rhs.head_->name())
      return false;
    if (body_.size() != rhs.body_.size())
      return false;
    for (auto it = body_.cbegin(), itr = rhs.body_.cbegin();
         it != body_.cend(), itr != rhs.body_.cend(); ++it, ++itr)
      if ((*it)->name() != (*itr)->name())
        return false;
    return true;
  }
  bool operator!=(const Production& rhs) const {
    return !(rhs == *this);
  }

  auto operator()(const ParsedTerm *term, syntax::AbstractSyntax& absSyntax) const {
    return semantic_(term, absSyntax);
  }

  PriorityComp priorierThan(const Production *rhs) const {
    if (prior() == UNDEF && rhs->prior() == UNDEF)
      return DEFAULT;
    else if (prior() < rhs->prior())
      return HIGHER;
    else if (prior() > rhs->prior())
      return LOWER;
    else if (assoc() == NONE || rhs->assoc() == NONE)
      return ERROR;
    else if (assoc() == LEFT)
      return HIGHER;
    else if (assoc() == RIGHT)
      return LOWER;
    else
      return ERROR;
  }
  constexpr const auto& body() const noexcept { return body_; }
protected:
  constexpr Associative assoc() const noexcept { return attr_.assoc; }
  constexpr PriorityClass prior() const noexcept { return attr_.prior; }

  NonTerminal *head_;
  vector<GrammarSymbol*> body_;
  SemanticFunc semantic_;
  Attribute attr_;
};

template<typename T>
T *ParsedTerm::abstract(syntax::AbstractSyntax& absSyntax) const {
  return dynamic_cast<T*>((*production())(this, absSyntax));
}

class Grammar {
public:
  explicit Grammar(vector<unique_ptr<Terminal>>&& terms,
                   vector<unique_ptr<NonTerminal>>&& nonterms,
                   vector<unique_ptr<Production>>&& prods,
                   unordered_map<Token::Kind, Terminal*> termTypeHash,
                   NonTerminal *aug, Terminal *eof) noexcept;
  explicit Grammar(Grammar&& other) = default;

  constexpr const auto& terminals() const noexcept { return terms_; }
  constexpr const auto& nonterminals() const noexcept { return nonterms_; }
  constexpr const auto& productions() const noexcept { return prods_; }
  constexpr NonTerminal *aug() const noexcept { return aug_; }
  constexpr Terminal *eof() const noexcept { return eof_; }
  size_t indexOfTerm(Terminal *term) const noexcept { return termHash_.at(term); }
  size_t indexOfToken(Token::Kind kind) const noexcept { return termTypeHash_.at(kind); }
  size_t indexOfNonterm(NonTerminal *nonterm) const noexcept {
    return nontermHash_.at(nonterm);
  }
  size_t indexOfProd(const Production *production) const noexcept {
    return prodHash_.at(production);
  }
  auto prodRange(NonTerminal *nonterm) const noexcept {
    return prodRanges_[indexOfNonterm(nonterm)];
  }
  const auto& firstsOf(NonTerminal *nonterm) const noexcept {
    return firsts_[indexOfNonterm(nonterm)];
  }
  const auto& followsOf(NonTerminal *nonterm) const noexcept {
    return follows_[indexOfNonterm(nonterm)];
  }
  const auto& firstsOf(size_t index) const noexcept { return firsts_[index]; }
  const auto& followsOf(size_t index) const noexcept { return follows_[index]; }

  class Builder {
  public:
    Terminal *term(const Token& token);
    NonTerminal *nonterm(string name);
    NonTerminal *closure(string name);

    template <typename... Args>
    Builder& prod(NonTerminal *nonterm,
                  vector<GrammarSymbol*>&& syms,
                  Args&&... args);
    // Builder& prod(NonTerminal *nonterm, vector<GrammarSymbol*>&& syms,
    //               Production::SemanticFunc&& semantic);
    // Builder& prod(NonTerminal *nonterm,
    //               vector<GrammarSymbol*>&& syms,
    //               Production::SemanticFunc&& semantic,
    //               Production::Attribute attr);
    /*
    Builder& addProductions(std::vector<Production>&& prods) {
      for (auto&& prod : std::move(prods)) {
        prods_.emplace_back(new Production(std::move(prod)));
      }
      return *this;
    }
    */
    Builder& startAt(NonTerminal *start) noexcept {
      prods_.emplace_back(new Production(
          aug_ = nonterm(" aug"), {start,}, Production::SemanticFunc()));
      return *this;
    }
    Grammar build() noexcept;

  protected:
    vector<unique_ptr<Terminal>> terms_;
    vector<unique_ptr<NonTerminal>> nonterms_;
    vector<unique_ptr<Production>> prods_;
    unordered_map<string, NonTerminal*> nontermHash_;
    unordered_map<Token::Kind, Terminal*> termHash_;
    NonTerminal *aug_ = nullptr;
  };

  static unique_ptr<const Grammar> tigerGrammar();

protected:
  void computeFirsts();
  void computeFollows();

  vector<unique_ptr<Terminal>> terms_;
  vector<unique_ptr<NonTerminal>> nonterms_;
  vector<unique_ptr<Production>> prods_;
  unordered_map<Terminal*, size_t> termHash_;
  unordered_map<NonTerminal*, size_t> nontermHash_;
  unordered_map<const Production*, size_t> prodHash_;
  vector<std::pair<size_t, size_t>> prodRanges_;
  unordered_map<Token::Kind, size_t> termTypeHash_;
  vector<valarray<bool>> firsts_;
  vector<valarray<bool>> follows_;
  NonTerminal *aug_;
  Terminal *eof_;
};

template<typename... Args>
Grammar::Builder& Grammar::Builder::prod(
    NonTerminal *nonterm, vector<GrammarSymbol *>&& syms, Args&& ... args) {
  if (syms.empty())
    nonterm->nullable(true);
  prods_.emplace_back(std::make_unique<Production>(
      nonterm, std::move(syms), std::move(args)...));
  return *this;
}

class GrammarItem {
public:
  explicit constexpr GrammarItem(const Production *prod) noexcept
      : prod_(prod) {}

  constexpr const Production *production() const noexcept { return prod_; }
  friend std::ostream& operator<<(std::ostream& os, const GrammarItem& item) {
    item.print(os);
    return os;
  }

protected:
  virtual void print(std::ostream& os) const = 0;
  const Production *const prod_;
};

class LR0Item : public GrammarItem {
private:
  class SubRange {
  public:
    explicit constexpr SubRange(const LR0Item *item) noexcept : item_(item) {}

    auto begin() { return item_->production()->body().begin() + item_->index(); }
    auto end() { return item_->production()->body().end(); }
    auto cbegin() const { return item_->production()->body().cbegin() + item_->index(); }
    auto cend() const { return item_->production()->body().cend(); }
  private:
    const LR0Item *item_;
  };

public:
  explicit constexpr LR0Item(const Production *prod, size_t index) noexcept
      : GrammarItem(prod), index_(index) {}

  GrammarSymbol *nextSym() const {
    if (const auto& prodBody = prod_->body(); index_ < prodBody.size())
      return prodBody[index_];
    else
      return nullptr;
  }
  SubRange nextSyms() const { return SubRange(this); }
  constexpr size_t index() const noexcept { return index_; }

protected:
  void print(std::ostream& os) const override {
    os << prod_->head()->name() << " ::=";
    for (size_t i = 0; i < index_; ++i) {
      os << " " << prod_->body()[i]->name();
    }
    os << " .";
    for (size_t i = index_; i < prod_->body().size(); ++i) {
      os << " " << prod_->body()[i]->name();
    }
  }

  size_t index_;
};

class GrammarState {
public:
  // explicit GrammarState(size_t nNonterms, GrammarItem *item)
  //     : added_(nNonterms, false), items_(1, item) {}
  explicit GrammarState(vector<GrammarItem*>&& items) : items_(std::move(items)) {}
  void addItem(GrammarItem *item) { items_.push_back(item); }
  auto begin() noexcept { return items_.begin(); }
  auto end() noexcept { return items_.end(); }
  auto begin() const noexcept { return items_.begin(); }
  auto end() const noexcept { return items_.end(); }
  auto cbegin() const noexcept { return items_.cbegin(); }
  auto cend() const noexcept { return items_.cend(); }
  size_t nItems() const noexcept { return items_.size(); }
  GrammarItem *item(size_t index) const noexcept { return items_[index]; }
  friend std::ostream& operator<<(std::ostream& os, const GrammarState& state) {
    for (auto item : state.items_)
      os << *item << std::endl;
    return os;
  }
protected:
  vector<GrammarItem*> items_;
};

struct Action {
  enum Kind { SHIFT, REDUCE, ACCEPT, ERROR = -1, UNDEF = -2, } kind
      ;
  union { size_t toState; size_t useProd; size_t param; };

  constexpr Action() noexcept : kind(UNDEF), param() {}
  constexpr Action(Kind kind, size_t param = 0) noexcept : kind(kind), param(param) {}
  constexpr bool empty() const noexcept { return kind == UNDEF; }
  constexpr bool isReduce() const noexcept { return kind == REDUCE; }
  constexpr bool isShift() const noexcept { return kind == SHIFT; }

  static constexpr Action SHIFT_(size_t toState) noexcept {
    return Action{SHIFT, toState};
  }
  static constexpr Action REDUCE_(size_t useProd) noexcept {
    return Action{REDUCE, useProd};
  }

  friend std::ostream& operator<<(std::ostream& os, const Action& action) {
    switch (action.kind) {
    case SHIFT: return os << "shift state " << action.toState;
    case REDUCE: return os << "reduce with " << action.useProd;
    case ACCEPT: return os << "accept";
    case ERROR: return os << "error";
    default: return os;
    }
  }
};

class Parser {
public:
  virtual unique_ptr<ParsedTerm> parse(lex::Lexer& lexer) = 0;
  const Grammar *grammar() const noexcept { return grammar_.get(); }

protected:
  explicit Parser(unique_ptr<const Grammar>&& grammar) noexcept
      : grammar_(std::move(grammar)) {}
  virtual void init() = 0;
  unique_ptr<const Grammar> grammar_;
};

class LRParser : public Parser {
public:
  constexpr const auto& parseTable() const noexcept { return parseTable_; }
  constexpr const auto& gotoTable() const noexcept { return gotoTable_; }

protected:
  explicit LRParser(unique_ptr<const Grammar>&& grammar) noexcept
      : Parser(std::move(grammar)) { }
  void init() override {
    yieldItems();
    generateParseTable();
  }
  virtual void generateParseTable() = 0;
  virtual size_t state(GrammarItem *item);
  virtual size_t state(vector<GrammarItem *>&& items);
  virtual void yieldClosure(GrammarState *state) = 0;
  virtual void yieldItems() = 0;

  vector<unique_ptr<GrammarItem>> items_;
  vector<unique_ptr<GrammarState>> states_;
  vector<vector<Action>> parseTable_;
  vector<vector<size_t>> gotoTable_;
  unordered_map<vector<bool>, size_t> stateHash_;
  unordered_map<GrammarItem*, size_t> itemHash_;
  // unordered_map<GrammarState*, size_t> stateHash_;
};

class SLRParser final : public LRParser {
public:
  unique_ptr<ParsedTerm> parse(lex::Lexer& lexer) override;

  static unique_ptr<SLRParser> newInstance(unique_ptr<const Grammar>&& grammar) {
    unique_ptr<SLRParser> parser(new SLRParser(std::move(grammar)));
    parser->init();
    return parser;
  }

protected:
  explicit SLRParser(unique_ptr<const Grammar>&& grammar) noexcept
      : LRParser(std::move(grammar)) { }
  void generateParseTable() override;
  void yieldClosure(GrammarState *state) override;
  virtual void yieldItems();
  LR0Item *item(const Production *prod, size_t index);

  vector<vector<LR0Item*>> itemTable_;
};
} // namespace tiger::syntax

#endif //TIGER_COMPILER_PARSER_H
