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
#include "lexer.h"

using std::vector;
using std::valarray;
using std::deque;
using std::unique_ptr;
using std::unordered_map;
using std::unordered_set;

namespace tiger::syntax {
using lex::Token;
class Symbol {
public:
  friend std::ostream &operator<<(std::ostream &os, const Symbol &symbol) {
    symbol.print(os);
    return os;
  }
  virtual bool isTerminal() const noexcept = 0;
  virtual bool isNonTerminal() const noexcept = 0;
  virtual string name() const noexcept = 0;
  virtual bool operator==(const Symbol& rhs) const noexcept {
    return name() == rhs.name();
  }
  bool operator!=(const Symbol& rhs) const noexcept {
    return !operator==(rhs);
  }

protected:
  virtual void print(std::ostream& os) const { os << name(); }
};

class Terminal : public Symbol {
public:
  explicit Terminal(Token token) : token_(std::move(token)) {}
  explicit Terminal(Terminal&& other) noexcept = default;
  bool isTerminal() const noexcept override { return true; }
  bool isNonTerminal() const noexcept override { return false; }
  string name() const noexcept override { return token_.name(); }
  friend std::ostream& operator<<(std::ostream& os, const Terminal& terminal) {
    terminal.print(os);
    return os;
  }
  bool operator==(const Symbol& rhs) const noexcept override {
    return Symbol::operator==(rhs)
        && token_ == dynamic_cast<const Terminal&>(rhs).token_;
  }

protected:
  void print(std::ostream& os) const override {
    os << token_;
  }

  Token token_;
};

class NonTerminal : public Symbol {
public:
  explicit NonTerminal(string name) : name_(std::move(name)) {}
  explicit NonTerminal(const char_t *name) : name_(name) {}
  explicit NonTerminal(NonTerminal&& other) noexcept = default;
  bool isTerminal() const noexcept override { return false; }
  bool isNonTerminal() const noexcept override { return true; }
  string name() const noexcept override { return name_; }
  friend std::ostream& operator<<(std::ostream& os, const NonTerminal& nonterminal) {
    nonterminal.print(os);
    return os;
  }
  constexpr void nullable(bool nullable) noexcept { nullable_ = nullable; }
  constexpr bool nullable() const noexcept { return nullable_; }

protected:
  string name_;
  bool nullable_ = false;
};

class ParsedTerm : public NonTerminal {
public:
  explicit ParsedTerm(string name) : NonTerminal(std::move(name)) {}
  explicit ParsedTerm(const char_t *name) : NonTerminal(name) {}
  explicit ParsedTerm(ParsedTerm&& other) = default;
  explicit ParsedTerm(string name, deque<Symbol*>&& syms)
      : NonTerminal(std::move(name)) {
    for (auto sym : syms) {
      children_.emplace_back(sym);
    }
  }

  void addChildFront(unique_ptr<Symbol>&& child) {
    children_.emplace_front(std::move(child));
  }
  void addChildBack(unique_ptr<Symbol>&& child) {
    children_.emplace_back(std::move(child));
  }
  Symbol *operator[](size_t index) const noexcept { return children_[index].get(); }

  friend std::ostream& operator<<(std::ostream& os, const ParsedTerm& term) {
    term.print(os);
    return os;
  }
  bool operator==(const ParsedTerm& rhs) const {
    if (name() != rhs.name())
      return false;
    if (children_.size() != rhs.children_.size())
      return false;
    for (auto it = children_.cbegin(), itr = rhs.children_.cbegin();
         it != children_.cend() && itr != rhs.children_.cend(); ++it, ++itr)
      if (**it != **itr)
        return false;
    return true;
  }
  bool operator!=(const ParsedTerm& rhs) const {
    return !(rhs == *this);
  }

protected:
  void print(std::ostream& os) const override {
    os << name() << "(";
    for (size_t i = 0; i + 1 < children_.size(); ++i)
      os << *children_[i] << " ";
    if (!children_.empty())
      os << *children_.back();
    os << ")";
  }

  deque<unique_ptr<Symbol>> children_;
};

class Production {
public:
  enum Associative : int8_t { NONE = -1, LEFT, RIGHT };
  enum PriorityClass : uint8_t {
    UNDEF = 0x7f,
    PROTECTED = 0,
    FIRST, SECOND, THIRD, FOURTH, FIFTH, SIXTH, SEVENTH, EIGHTH, NINTH, TENTH,
  };
  enum PriorityComp { ERROR = -1, HIGHER, LOWER, };

  struct Attribute {
    Associative assoc;
    PriorityClass prior;

    constexpr Attribute() noexcept : assoc(RIGHT), prior(UNDEF) {}
    constexpr Attribute(Associative assoc, PriorityClass prior) noexcept
        : assoc(assoc), prior(prior) {}
  };

  Production(NonTerminal *head, vector<Symbol*>&& body)
      : head_(head), body_(std::move(body)) {}
  Production(NonTerminal *head, vector<Symbol*>&& body, Attribute attr)
      : head_(head), body_(std::move(body)), attr_(attr) {}
  Production(const Production& other) = default;
  Production(Production&& other) noexcept = default;
  Production& operator=(const Production& other) = default;
  Production& operator=(Production&& other) noexcept = default;
  size_t size() const noexcept { return body_.size(); }
  bool empty() const noexcept { return body_.empty(); }
  Symbol *operator[](size_t index) const noexcept { return body_[index]; }
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

  PriorityComp priorierThan(const Production *rhs) const {
    if (prior() == UNDEF || rhs->prior() == UNDEF ||
        assoc() == NONE || rhs->assoc() == NONE)
      return ERROR;
    else if (prior() < rhs->prior())
      return HIGHER;
    else if (prior() > rhs->prior())
      return LOWER;
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
  vector<Symbol*> body_;
  Attribute attr_;
};

class Grammar {
public:
  explicit Grammar(vector<unique_ptr<Terminal>>&& terms,
                   vector<unique_ptr<NonTerminal>>&& nonterms,
                   vector<unique_ptr<Production>>&& prods,
                   unordered_map<Token::Type, Terminal*> termTypeHash,
                   NonTerminal *aug, Terminal *eof) noexcept;
  explicit Grammar(Grammar&& other) = default;

  constexpr const auto& terminals() const noexcept { return terms_; }
  constexpr const auto& nonterminals() const noexcept { return nonterms_; }
  constexpr const auto& productions() const noexcept { return prods_; }
  constexpr NonTerminal *aug() const noexcept { return aug_; }
  constexpr Terminal *eof() const noexcept { return eof_; }
  size_t indexOfTerm(Terminal *term) const noexcept { return termHash_.at(term); }
  size_t indexOfToken(Token::Type type) const noexcept { return termTypeHash_.at(type); }
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
    Builder& prod(NonTerminal *nonterm, vector<Symbol*>&& syms) {
      if (syms.empty())
        nonterm->nullable(true);
      prods_.emplace_back(std::make_unique<Production>(nonterm, std::move(syms)));
      return *this;
    }
    Builder& prod(NonTerminal *nonterm, vector<Symbol*>&& syms, Production::Attribute attr) {
      prods_.emplace_back(std::make_unique<Production>(nonterm, std::move(syms), attr));
      return *this;
    }
    /*
    Builder& addProductions(std::vector<Production>&& prods) {
      for (auto&& prod : std::move(prods)) {
        prods_.emplace_back(new Production(std::move(prod)));
      }
      return *this;
    }
    */
    Builder& startAt(NonTerminal *start) noexcept {
      prods_.emplace_back(new Production(aug_ = nonterm(" aug"), {start,}));
      return *this;
    }
    Grammar build() noexcept;

  protected:
    vector<unique_ptr<Terminal>> terms_;
    vector<unique_ptr<NonTerminal>> nonterms_;
    vector<unique_ptr<Production>> prods_;
    unordered_map<string, NonTerminal*> nontermHash_;
    unordered_map<Token::Type, Terminal*> termHash_;
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
  unordered_map<Token::Type, size_t> termTypeHash_;
  vector<valarray<bool>> firsts_;
  vector<valarray<bool>> follows_;
  NonTerminal *aug_;
  Terminal *eof_;
};

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

  Symbol *nextSym() const {
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
  enum Type { SHIFT, REDUCE, ACCEPT, ERROR = -1, UNDEF = -2, } type;
  union { size_t toState; size_t useProd; size_t param; };

  constexpr Action() noexcept : type(UNDEF), param() {}
  constexpr Action(Type type, size_t param = 0) noexcept : type(type), param(param) {}
  constexpr bool empty() const noexcept { return type == UNDEF; }
  constexpr bool isReduce() const noexcept { return type == REDUCE; }
  constexpr bool isShift() const noexcept { return type == SHIFT; }

  static constexpr Action SHIFT_(size_t toState) noexcept {
    return Action{SHIFT, toState};
  }
  static constexpr Action REDUCE_(size_t useProd) noexcept {
    return Action{REDUCE, useProd};
  }

  friend std::ostream& operator<<(std::ostream& os, const Action& action) {
    switch (action.type) {
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
