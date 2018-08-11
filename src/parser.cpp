//
// Created by whjpji on 18-7-23.
//

#include <algorithm>
#include <functional>
#include <stack>
#include <boost/log/trivial.hpp>
#include "parser.h"

namespace tiger::syntax {

Grammar::Grammar(vector<unique_ptr<Terminal>>&& terms,
                 vector<unique_ptr<NonTerminal>>&& nonterms,
                 vector<unique_ptr<Production>>&& prods,
                 unordered_map<Token::Type, Terminal*> termTypeHash,
                 NonTerminal *aug,
                 Terminal *eof) noexcept
    : terms_(std::move(terms)),
      nonterms_(std::move(nonterms)),
      prods_(std::move(prods)), aug_(aug), eof_(eof) {
  for (size_t i = 0; i < terms_.size(); ++i)
    termHash_.emplace(terms_[i].get(), i);
  for (size_t i = 0; i < nonterms_.size(); ++i)
    nontermHash_.emplace(nonterms_[i].get(), i);
  for (size_t i = 0; i < prods_.size(); ++i)
    prodHash_.emplace(prods_[i].get(), i);
  for (auto [type, term] : termTypeHash) {
    termTypeHash_.emplace(type, termHash_[term]);
  }
  prodRanges_.resize(nonterms_.size());
  NonTerminal *head = nullptr;
  size_t rangeBegin = 0;
  for (size_t i = 0; i < prods_.size(); ++i) {
    if (prods_[i]->head() != head) {
      if (head)
        prodRanges_[nontermHash_[head]] = std::make_pair(rangeBegin, i);
      head = prods_[i]->head();
      rangeBegin = i;
    }
  }
  if (head)
    prodRanges_[nontermHash_[head]] = std::make_pair(rangeBegin, prods_.size());

  computeFirsts();
  computeFollows();
}

void Grammar::computeFirsts() {
  const auto& EMPTY_TERM = terms_.size();
  firsts_.resize(nonterms_.size());
  for (size_t iNonterm = 0; iNonterm < nonterms_.size(); ++iNonterm) {
    firsts_[iNonterm].resize(terms_.size() + 1, false);
  }
  vector<bool> computed(nonterms_.size(), false);

  // Recursively compute the first sets.
  std::function<void(size_t iProd)> compute = [&](size_t iNonterm) {
    // size_t iNonterm = nontermHash_[prod.head()];
    computed[iNonterm] = true;
    auto [begin, end] = prodRanges_[iNonterm];
    for (size_t iProd = begin; iProd < end; ++iProd) {
      const auto& prod = prods_[iProd];
      // If X -> EMPTY, then add EMPTY to FIRST(X).
      if (prod->body().empty()) {
        firsts_[iNonterm][EMPTY_TERM] = true;
        computed[iNonterm] = true;
        return;
      }
      bool emptyBefore = true;
      // For X ::= X1 X2 X3 ... Xn, if EMPTY is in X[1], X[2], ..., X[i],
      // then recursively compute and add all of the first sets of
      // X[1], X[2], ..., X[i-1] to X.
      for (const auto& item : prod->body()) {
        if (item->isTerminal()) {
          auto term = dynamic_cast<Terminal *>(item);
          firsts_[iNonterm][termHash_[term]] = true;
        } else {
          auto nextNonterm = dynamic_cast<NonTerminal *>(item);
          size_t iNextNonterm = nontermHash_[nextNonterm];
          if (iNextNonterm != iNonterm) {
            if (!computed[iNextNonterm])
              compute(iNextNonterm);
            for (size_t iTerm = 0; iTerm < firsts_[iNextNonterm].size();
                 ++iTerm)
              if (firsts_[iNextNonterm][iTerm])
                firsts_[iNonterm][iTerm] = true;
          }
          if (!firsts_[nontermHash_[nextNonterm]][EMPTY_TERM]) {
            break;
          }
        }
      }
    }
  };

  // Compute first sets for each non-terminals.
  for (size_t i = 0; i < nonterms_.size(); ++i)
    if (!computed[i])
      compute(i);
}

void Grammar::computeFollows() {
  const auto& EMPTY_TERM = terms_.size();
  follows_.resize(nonterms_.size());
  for (size_t iNonterm = 0; iNonterm < nonterms_.size(); ++iNonterm) {
    follows_[iNonterm].resize(terms_.size(), false);
  }

  follows_[nontermHash_[aug_]][termHash_[eof_]] = true;
  // If there is A ::= a B b, then everything in
  // FIRST(b) except EMPTY is in FOLLOW(B).
  for (auto& prod : prods_) {
    const auto& prodBody = prod->body();
    for (auto i = prodBody.begin(); i < prodBody.end(); ++i) {
      if (auto item = *i; item->isNonTerminal()) {
        size_t iNonterm = nontermHash_[dynamic_cast<NonTerminal*>(item)];
        if (i < prodBody.end() - 1) {
          if (auto nextItem = i[1]; nextItem->isTerminal()) {
            size_t iNextTerm = termHash_[dynamic_cast<Terminal*>(nextItem)];
            follows_[iNonterm][iNextTerm] = true;
          } else {
            size_t iNextNonterm = nontermHash_[dynamic_cast<NonTerminal*>(nextItem)];
            // Except the EMPTY terminal.
            for (size_t iTerm = 0; iTerm < terms_.size(); ++iTerm) {
              if (firsts_[iNonterm][iTerm])
                follows_[iNonterm][iTerm] = true;
            }
          }
        }
      }
    }
  }

  // If there is A ::= a B, or A ::= a B b, and EMPTY is in FIRST(b),
  // then everything in FOLLOW(A) is in FOLLOW(B).
  for (auto& prod : prods_) {
    const auto& prodBody = prod->body();
    for (auto i = prodBody.rbegin(); i < prodBody.rend(); ++i) {
      auto item = *i;
      if (item->isTerminal())
        break;

      size_t iHeadNonterm = nontermHash_[prod->head()];
      size_t iNonterm = nontermHash_[dynamic_cast<NonTerminal*>(item)];
      for (size_t iTerm = 0; iTerm < terms_.size(); ++iTerm) {
        if (follows_[iHeadNonterm][iTerm])
          follows_[iNonterm][iTerm] = true;
      }
      if (!firsts_[iNonterm][EMPTY_TERM])
        break;
    }
  }
}

Grammar Grammar::Builder::build() noexcept {
  std::stable_sort(
      prods_.begin(), prods_.end(), [](const unique_ptr<Production>& a,
                                       const unique_ptr<Production>& b) {
        return a->head()->name() < b->head()->name();
  });
  return Grammar(std::move(terms_), std::move(nonterms_), std::move(prods_),
                 std::move(termHash_), aug_, term(Token::EOF_TOK));
}

Terminal *Grammar::Builder::term(const Token& token) {
  if (auto iter = termHash_.find(token.type()); iter != termHash_.end()) {
    return iter->second;
  } else {
    return termHash_.emplace(
            token.type(), terms_.emplace_back(new Terminal(token)).get())
        .first->second;
  }
}

NonTerminal *Grammar::Builder::nonterm(string name) {
  if (auto iter = nontermHash_.find(name); iter != nontermHash_.end()) {
    return iter->second;
  } else {
    return nontermHash_.emplace(
            name, nonterms_.emplace_back(new NonTerminal(name)).get())
        .first->second;
  }
}

size_t LRParser::state(GrammarItem *item) {
  return state(vector<GrammarItem *>(1, item));
}

size_t LRParser::state(vector<GrammarItem *>&& items) {
  vector<bool> itemSets(items_.size());
  for (auto gItem : items)
    itemSets[itemHash_[gItem]] = true;
  if (auto iter = stateHash_.find(itemSets); iter != stateHash_.end()) {
    return iter->second;
  } else {
    gotoTable_.emplace_back(grammar_->nonterminals().size());
    parseTable_.emplace_back(grammar_->terminals().size());
    auto state = states_.emplace_back(new GrammarState(std::move(items))).get();
    // stateHash_.emplace(state, states_.size() - 1);
    yieldClosure(state);
    return stateHash_.emplace(itemSets, states_.size() - 1).first->second;
  }
}

LR0Item *SLRParser::item(const Production *prod, size_t index) {
  auto iProd = static_cast<size_t>(grammar_->indexOfProd(const_cast<Production*>(prod)));
  return itemTable_[iProd][index];
  // if (auto& item = itemTable_[iProd][index])
  //   return item;
  // else
  //   return item = dynamic_cast<LR0Item*>(
  //       items_.emplace_back(new LR0Item(prod, index)).get());
}

void SLRParser::yieldItems() {
  for (const auto& prod : grammar_->productions()) {
    itemTable_.emplace_back();
    for (size_t i = 0; i < prod->body().size() + 1; ++i)
      itemTable_.back().emplace_back(
          dynamic_cast<LR0Item*>(items_.emplace_back(new LR0Item(prod.get(), i)).get()));
  }
  for (size_t i = 0; i < items_.size(); ++i) {
    itemHash_.emplace(items_[i].get(), i);
  }
}

void SLRParser::yieldClosure(GrammarState *state) {
  vector<bool> added(grammar_->nonterminals().size());
  // For each item A ::= a . B b in state I, add all productions of B in I.
  for (size_t i = 0; i < state->nItems(); ++i) {
    auto lr0Item = dynamic_cast<LR0Item*>(state->item(i));
    auto nonterm = lr0Item->production()->head();
    // if (size_t iNonTerm = grammar_->indexOfNonterm(nonterm); !state->isAdded(iNonTerm)) {
      // state->setAdded(iNonTerm);
      if (auto sym = lr0Item->nextSym(); sym && sym->isNonTerminal()) {
        auto nextNonterm = dynamic_cast<NonTerminal *>(sym);
        size_t iNextNonterm = grammar_->indexOfNonterm(nextNonterm);
        if (!added[iNextNonterm]) {
          auto[begin, end] = grammar_->prodRange(nextNonterm);
          for (size_t iProd = begin; iProd < end; ++iProd) {
            const auto& prod = grammar_->productions()[iProd];
            state->addItem(item(prod.get(), 0));
          }
          added[iNextNonterm] = true;
        }
      }
    // }
  }
  // BOOST_LOG_TRIVIAL(debug) << "yield closure:\n" << *state;
}

void SLRParser::generateParseTable() {
  auto startState = state(item(grammar_->productions()[0].get(), 0));
  size_t nNonterms = grammar_->nonterminals().size();
  size_t nTerms = grammar_->terminals().size();
  size_t nSyms = nNonterms + nTerms;
  vector<vector<GrammarItem*>> gotoItems(nSyms);
  vector<const Production*> prodOfTerm(nTerms, nullptr);
  // For each item i(`A ::= a . B b`) in each state, compute the goto
  // function GOTO(i, B) = item j(`A ::= a B. b`). Then group them
  // symbol by symbol, and add the yielded states if it is different from
  // any other states existed before.
  for (size_t iState = 0; iState < states_.size(); ++iState) {
    for (auto& items : gotoItems)
      items.clear();
    for (auto& prod : prodOfTerm)
      prod = nullptr;
    for (auto gItem : *states_[iState]) {
      auto lr0Item = dynamic_cast<LR0Item*>(gItem);
      if (Symbol *nextSym = lr0Item->nextSym()) {
        LR0Item *nextItem = item(lr0Item->production(), lr0Item->index() + 1);
        if (nextSym->isNonTerminal()) {
          auto nextNonterm = dynamic_cast<NonTerminal *>(nextSym);
          gotoItems[grammar_->indexOfNonterm(nextNonterm)].push_back(nextItem);
        } else {
          auto nextTerm = dynamic_cast<Terminal *>(nextSym);
          size_t iNextTerm = grammar_->indexOfTerm(nextTerm);
          if (!prodOfTerm[iNextTerm])
            prodOfTerm[iNextTerm] = nextItem->production();
          gotoItems[iNextTerm + nNonterms].push_back(
              nextItem);
        }
      } else {
        // When reaching the last item of a prod, saying `A ::= a B .`,
        // set its action to REDUCE by this prod for all terminals
        // in FOLLOW(A). If it is the starting prod `S' ::= S .`, set
        // its action to ACCEPT for the terminating terminal $.
        auto iProd = grammar_->indexOfProd(lr0Item->production());
        auto follows = grammar_->followsOf(lr0Item->production()->head());
        for (size_t iTerm = 0; iTerm < nTerms; ++iTerm) {
          if (follows[iTerm]) {
            if (auto& action = parseTable_[iState][iTerm]; action.empty()) {
              action = Action::REDUCE_(iProd);
              if (iProd == 0 && iTerm == grammar_->indexOfToken(Token::EOF_TOK))
                action = Action::ACCEPT;
            }
          }
        }
      }
    }
    // Set GOTO table for each grouped non-terminals.
    for (size_t iNonterm = 0; iNonterm < nNonterms; ++iNonterm)
      if (auto&& items = std::move(gotoItems[iNonterm]); !items.empty()) {
        gotoTable_[iState][iNonterm] = state(std::move(items));
      }
    // Set SHIFT actions of PARSE table for each grouped terminals.
    for (size_t iTerm = 0; iTerm < nTerms; ++iTerm)
      if (auto&& items = std::move(gotoItems[iTerm + nNonterms]); !items.empty()) {
        auto& action = parseTable_[iState][iTerm];
        bool shift = false;
        if (!action.empty() && action.isReduce()) {
          const Production *lprod = grammar_->productions()[action.useProd].get();
          const Production *rprod = prodOfTerm[iTerm];
          if (auto comp = lprod->priorierThan(rprod); comp == Production::LOWER)
            shift = true;
          else if (comp == Production::ERROR) {
            action = Action::ERROR;
          }
        } else
          shift = true;
        if (shift)
          action = Action::SHIFT_(state(std::move(items)));
      }
  }
}

unique_ptr<ParsedTerm> SLRParser::parse(lex::Lexer& lexer) {
  struct Node { size_t iState; unique_ptr<Symbol> sym; };
  std::stack<Node> stack;
  stack.push({0, nullptr});
  Token token(Token::EOF_TOK);
  bool advance = true;
  while (!stack.empty()) {
    const auto& [iState, sym] = stack.top();
    if (advance) {
      advance = false;
      token = lexer.nextToken();
    }
    size_t iTerm = grammar_->indexOfToken(token.type());
    Action action = parseTable_[iState][iTerm];
    switch (action.type) {
    // For action SHIFT(a, I), fetch the next token a, and push it onto the stack
    // with state I.
    case Action::SHIFT:
      advance = true;
      stack.push({action.toState, std::make_unique<Terminal>(std::move(token))});
      break;
    // For action REDUCE(p, I), pop |p| symbols, use prod p to reduce
    // them into a non-terminal, and push the reduced non-terminal A with a new
    // state checked from GOTO(A, I'), where I' is the state on the top of stack
    // before A has been pushed.
    case Action::REDUCE: {
      const Production *prod = grammar_->productions()[action.useProd].get();
      auto parsedTerm = std::make_unique<ParsedTerm>(prod->head()->name());
      for (size_t i = 0; i < prod->size(); ++i) {
        parsedTerm->addChildFront(std::move(stack.top().sym));
        stack.pop();
      }
      size_t iNewState = gotoTable_[stack.top().iState][grammar_->indexOfNonterm(prod->head())];
      stack.push({iNewState, std::move(parsedTerm)});
      break;
    }
    // For action ACCEPT, just output the parse tree and terminate the parsing
    // procedure.
    case Action::ACCEPT:
      return unique_ptr<ParsedTerm>(dynamic_cast<ParsedTerm*>(stack.top().sym.release()));
    case Action::ERROR: default:
      // TODO: process the errors.
      break;
    }
  }
  return std::make_unique<ParsedTerm>(grammar_->aug()->name());
}

unique_ptr<const Grammar> Grammar::tigerGrammar() {
  using ProdType = Production;
  Grammar::Builder g;
  g.prod(g.nonterm("program"), {g.nonterm("exp"),})
      .prod(g.nonterm("program"), {g.nonterm("decs"),})
          // Literals.
      .prod(g.nonterm("exp"), {g.term(Token::NIL)})
      .prod(g.nonterm("exp"), {g.term(Token::INT)})
      .prod(g.nonterm("exp"), {g.term(Token::STRING)})
          // Array and record creations.
      .prod(g.nonterm("exp"), {g.nonterm("type_id"), g.term(Token::LBRACK), g.nonterm("exp"), g.term(Token::RBRACK), g.term(Token::OF), g.nonterm("exp")})
      .prod(g.nonterm("exp"), {g.nonterm("type_id"), g.term(Token::LBRACE), g.term(Token::RBRACE),})
      .prod(g.nonterm("exp"), {g.nonterm("type_id"), g.term(Token::LBRACE), g.term(Token::ID), g.term(Token::EQ), g.nonterm("exp"), g.nonterm("closure1"), g.term(Token::RBRACE),})
      .prod(g.nonterm("closure1"), {})
      .prod(g.nonterm("closure1"), {g.nonterm("closure1"), g.term(Token::COMMA), g.term(Token::ID), g.term(Token::EQ), g.nonterm("exp"),})
          // Object creation.
      // .prod(g.nonterm("exp"), {g.term(Token::NEW), g.nonterm("type_id"),})

          // Variables, field, elements of an array.
      .prod(g.nonterm("exp"), {g.nonterm("lvalue"),})

          // Function call
      .prod(g.nonterm("exp"), {g.term(Token::ID), g.term(Token::LPAREN), g.term(Token::RPAREN),})
      .prod(g.nonterm("exp"), {g.term(Token::ID), g.term(Token::LPAREN), g.nonterm("exp"), g.nonterm("closure2"), g.term(Token::RPAREN),})
      .prod(g.nonterm("closure2"), {})
      .prod(g.nonterm("closure2"), {g.nonterm("closure2"), g.term(Token::COMMA), g.nonterm("exp"),})

          // Method call.
      .prod(g.nonterm("exp"), {g.nonterm("lvalue"), g.term(Token::DOT), g.term(Token::ID), g.term(Token::LPAREN), g.term(Token::RPAREN),})
      .prod(g.nonterm("exp"), {g.nonterm("lvalue"), g.term(Token::DOT), g.term(Token::ID), g.term(Token::LPAREN), g.nonterm("exp"), g.nonterm("closure2"), g.term(Token::RPAREN),})

          // Operations.
      .prod(g.nonterm("exp"), {g.term(Token::MINUS), g.nonterm("exp"),}, {Production::RIGHT, Production::PROTECTED})
      .prod(g.nonterm("exp"), {g.term(Token::LPAREN), g.nonterm("exps"), g.term(Token::RPAREN),})

          // Assigns.
      .prod(g.nonterm("exp"), {g.nonterm("lvalue"), g.term(Token::ASSIGN), g.nonterm("exp"),})

          // Control structures.
      .prod(g.nonterm("exp"), {g.term(Token::IF), g.nonterm("exp"), g.term(Token::THEN), g.nonterm("exp"),})
      .prod(g.nonterm("exp"), {g.term(Token::IF), g.nonterm("exp"), g.term(Token::THEN), g.nonterm("exp"), g.term(Token::ELSE), g.nonterm("exp"),})
      .prod(g.nonterm("exp"), {g.term(Token::WHILE), g.nonterm("exp"), g.term(Token::DO), g.nonterm("exp"),})
      .prod(g.nonterm("exp"), {g.term(Token::FOR), g.term(Token::ID), g.term(Token::ASSIGN), g.nonterm("exp"), g.term(Token::TO), g.nonterm("exp"), g.term(Token::DO), g.nonterm("exp"),})
      .prod(g.nonterm("exp"), {g.term(Token::BREAK),})
      .prod(g.nonterm("exp"), {g.term(Token::LET), g.nonterm("decs"), g.term(Token::IN), g.nonterm("exps"), g.term(Token::END),})

      .prod(g.nonterm("lvalue"), {g.term(Token::ID),})
      .prod(g.nonterm("lvalue"), {g.nonterm("lvalue"), g.term(Token::DOT), g.term(Token::ID),})
      .prod(g.nonterm("lvalue"), {g.nonterm("lvalue"), g.term(Token::LBRACK), g.nonterm("exp"), g.term(Token::RBRACK),})

      .prod(g.nonterm("exps"), {})
      .prod(g.nonterm("exps"), {g.nonterm("exp"), g.nonterm("closure3"),})
      .prod(g.nonterm("closure3"), {g.nonterm("closure3"), g.term(Token::SEMICOLON), g.nonterm("exp"),})

      .prod(g.nonterm("decs"), {})
      .prod(g.nonterm("decs"), {g.nonterm("decs"), g.nonterm("dec"),})

          // Type declaration.
      .prod(g.nonterm("dec"), {g.term(Token::TYPE), g.term(Token::ID), g.term(Token::EQ), g.nonterm("ty"),})
          // Class definition.
      // .prod(g.nonterm("dec"), {g.term(Token::CLASS), g.term(Token::ID), g.term(Token::LBRACE), g.nonterm("classfields"), g.term(Token::RBRACE),})
      // .prod(g.nonterm("dec"), {g.term(Token::CLASS), g.term(Token::ID), g.term(Token::EXTENDS),  g.nonterm("type_id"), g.term(Token::LBRACE), g.nonterm("classfields"), g.term(Token::RBRACE),})
          // Variable declaration.
      .prod(g.nonterm("dec"), {g.nonterm("vardec"),})
          // Function declaration.
      .prod(g.nonterm("dec"), {g.term(Token::FUNCTION), g.term(Token::ID), g.term(Token::LPAREN), g.nonterm("tyfields"), g.term(Token::RPAREN), g.term(Token::EQ), g.nonterm("exp"),})
      .prod(g.nonterm("dec"), {g.term(Token::FUNCTION), g.term(Token::ID), g.term(Token::LPAREN), g.nonterm("tyfields"), g.term(Token::RPAREN), g.term(Token::COLON), g.nonterm("type_id"), g.term(Token::EQ), g.nonterm("exp"),})
          // Primitive declaration.
      // .prod(g.nonterm("dec"), {g.term(Token::PRIMITIVE), g.term(Token::ID), g.term(Token::LPAREN), g.nonterm("tyfields"), g.term(Token::RPAREN),})
      // .prod(g.nonterm("dec"), {g.term(Token::PRIMITIVE), g.term(Token::ID), g.term(Token::LPAREN), g.nonterm("tyfields"), g.term(Token::RPAREN), g.term(Token::COLON), g.nonterm("type_id"),})
          // Import a set of declaration.
      // .prod(g.nonterm("dec"), {g.term(Token::IMPORT), g.term(Token::STRING),})

      .prod(g.nonterm("vardec"), {g.term(Token::VAR), g.term(Token::ID), g.term(Token::ASSIGN), g.nonterm("exp"),})
      .prod(g.nonterm("vardec"), {g.term(Token::VAR), g.term(Token::ID), g.term(Token::COLON), g.nonterm("type_id"), g.term(Token::ASSIGN), g.nonterm("exp"),})

      // .prod(g.nonterm("classfields"), {})
      // .prod(g.nonterm("classfields"), {g.nonterm("classfields"), g.nonterm("classfield"),})
          // Class fields.
      // .prod(g.nonterm("classfield"), {g.nonterm("vardec"),})
      // .prod(g.nonterm("classfield"), {g.term(Token::METHOD), g.term(Token::ID), g.term(Token::LPAREN), g.nonterm("tyfields"), g.term(Token::RPAREN), g.term(Token::EQ), g.nonterm("exp"),})
      // .prod(g.nonterm("classfield"), {g.term(Token::METHOD), g.term(Token::ID), g.term(Token::LPAREN), g.nonterm("tyfields"), g.term(Token::RPAREN), g.term(Token::COLON), g.nonterm("type_id"), g.term(Token::EQ), g.nonterm("exp"),})

          // Types.
      .prod(g.nonterm("ty"), {g.nonterm("type_id"),})
      .prod(g.nonterm("ty"), {g.term(Token::LBRACE), g.nonterm("tyfields"), g.term(Token::RBRACE),})
      .prod(g.nonterm("ty"), {g.term(Token::ARRAY), g.term(Token::OF), g.nonterm("type_id"),})
      // .prod(g.nonterm("ty"), {g.term(Token::CLASS), g.term(Token::LBRACE), g.nonterm("classfields"), g.term(Token::RBRACE),})
      // .prod(g.nonterm("ty"), {g.term(Token::CLASS), g.term(Token::EXTENDS),  g.nonterm("type_id"), g.term(Token::LBRACE), g.nonterm("classfields"), g.term(Token::RBRACE),})

      .prod(g.nonterm("tyfields"), {})
      .prod(g.nonterm("tyfields"), {g.term(Token::ID), g.term(Token::COLON), g.nonterm("type_id"), g.nonterm("closure4"),})
      .prod(g.nonterm("closure4"), {})
      .prod(g.nonterm("closure4"), {g.nonterm("closure4"), g.term(Token::COMMA), g.term(Token::ID), g.term(Token::COLON), g.nonterm("type_id"),})

      .prod(g.nonterm("type_id"), {g.term(Token::ID),})

          // Operators.
      .prod(g.nonterm("exp"), {g.nonterm("exp"), g.term(Token::TIMES), g.nonterm("exp")}, {Production::LEFT, Production::FIRST})
      .prod(g.nonterm("exp"), {g.nonterm("exp"), g.term(Token::DIVIDE), g.nonterm("exp")}, {Production::LEFT, Production::FIRST})

      .prod(g.nonterm("exp"), {g.nonterm("exp"), g.term(Token::PLUS), g.nonterm("exp")}, {Production::LEFT, Production::SECOND})
      .prod(g.nonterm("exp"), {g.nonterm("exp"), g.term(Token::MINUS), g.nonterm("exp")}, {Production::LEFT, Production::SECOND})

      .prod(g.nonterm("exp"), {g.nonterm("exp"), g.term(Token::EQ), g.nonterm("exp")}, {Production::NONE, Production::THIRD})
      .prod(g.nonterm("exp"), {g.nonterm("exp"), g.term(Token::NEQ), g.nonterm("exp")}, {Production::NONE, Production::THIRD})
      .prod(g.nonterm("exp"), {g.nonterm("exp"), g.term(Token::GT), g.nonterm("exp")}, {Production::NONE, Production::THIRD})
      .prod(g.nonterm("exp"), {g.nonterm("exp"), g.term(Token::LT), g.nonterm("exp")}, {Production::NONE, Production::THIRD})
      .prod(g.nonterm("exp"), {g.nonterm("exp"), g.term(Token::GE), g.nonterm("exp")}, {Production::NONE, Production::THIRD})
      .prod(g.nonterm("exp"), {g.nonterm("exp"), g.term(Token::LE), g.nonterm("exp")}, {Production::NONE, Production::THIRD})

      .prod(g.nonterm("exp"), {g.nonterm("exp"), g.term(Token::AND), g.nonterm("exp")}, {Production::LEFT, Production::FOURTH})
      .prod(g.nonterm("exp"), {g.nonterm("exp"), g.term(Token::OR), g.nonterm("exp")}, {Production::LEFT, Production::FOURTH})
      .startAt(g.nonterm("program"));

  return std::make_unique<const Grammar>(g.build());
}

} // namespace tiger::syntax

