//
// Created by whjpji on 18-7-23.
//

#include <boost/log/trivial.hpp>
#include "gtest/gtest.h"
#include "parser.h"

TEST(parser_test, test_empty_productions) {
  using namespace tiger::lex;
  using namespace tiger::syntax;
  Grammar::Builder g;
  g
      .prod(g.nonterm("tuple"), {g.term(Token::LPAREN), g.term(Token::RPAREN),})
      .prod(g.nonterm("tuple"), {g.term(Token::LPAREN), g.nonterm("item"), g.closure("closure"), g.term(Token::RPAREN),})
      .prod(g.closure("closure"), {g.term(Token::COMMA), g.nonterm("item"), g.closure("closure"),})
      .prod(g.closure("closure"), {})
      .prod(g.nonterm("item"), {g.nonterm("tuple")})
      .prod(g.nonterm("item"), {g.term(Token::ID)})
      .startAt(g.nonterm("tuple"));

  unique_ptr<const Grammar> grammar(new Grammar(g.build()));

  string program("(a, b, (c), ())");
  Lexer lexer(program);
  auto parser = SLRParser::newInstance(std::move(grammar));

  auto parseTree = parser->parse(lexer);
  auto expectedParseTree = std::make_unique<ParsedTerm>("tuple", deque<Symbol*>{
      new Terminal(Token::LPAREN),
      new ParsedTerm("item", deque<Symbol *>{
          new Terminal(Token::ID_("a")),
      }),
      new Terminal(Token::COMMA),
      new ParsedTerm("item", deque<Symbol *>{
          new Terminal(Token::ID_("b")),
      }),
      new Terminal(Token::COMMA),
      new ParsedTerm("item", deque<Symbol *>{
          new ParsedTerm("tuple", deque<Symbol *>{
              new Terminal(Token::LPAREN),
              new ParsedTerm("item", deque<Symbol *>{
                  new Terminal(Token::ID_("c")),
              }),
              new Terminal(Token::RPAREN),
          }),
      }),
      new Terminal(Token::COMMA),
      new ParsedTerm("item", deque<Symbol *>{
          new ParsedTerm("tuple", deque<Symbol *>{
              new Terminal(Token::LPAREN),
              new Terminal(Token::RPAREN),
          }),
      }),
      new Terminal(Token::RPAREN),
  });

  EXPECT_EQ(*parseTree, *expectedParseTree);
}

TEST(parser_test, test_ambiguious_grammar) {
  using namespace tiger::lex;
  using namespace tiger::syntax;
  Grammar::Builder g;
  g.prod(g.nonterm("E"), {g.nonterm("E"), g.term(Token::TIMES), g.nonterm("E"),}, {Production::LEFT, Production::FIRST})
      .prod(g.nonterm("E"), {g.nonterm("E"), g.term(Token::PLUS), g.nonterm("E"),}, {Production::LEFT, Production::SECOND})
      .prod(g.nonterm("E"), {g.term(Token::LPAREN), g.nonterm("E"), g.term(Token::RPAREN),})
      .prod(g.nonterm("E"), {g.term(Token::ID),})
      .startAt(g.nonterm("E"));

  unique_ptr<const Grammar> grammar(new Grammar(g.build()));

  string program("(a * b + c * d) * e");
  Lexer lexer(program);
  auto parser = SLRParser::newInstance(std::move(grammar));

  auto parseTree = parser->parse(lexer);
  auto expectedParseTree = std::make_unique<ParsedTerm>("E", deque<Symbol*>{
      new ParsedTerm("E", deque<Symbol*>{
          new Terminal(Token::LPAREN),
          new ParsedTerm("E", deque<Symbol *>{
              new ParsedTerm("E", deque<Symbol *>{
                  new ParsedTerm("E", deque<Symbol *>{
                      new Terminal(Token::ID_("a")),
                  }),
                  new Terminal(Token::TIMES),
                  new ParsedTerm("E", deque<Symbol *>{
                      new Terminal(Token::ID_("b")),
                  }),
              }),
              new Terminal(Token::PLUS),
              new ParsedTerm("E", deque<Symbol *>{
                  new ParsedTerm("E", deque<Symbol *>{
                      new Terminal(Token::ID_("c")),
                  }),
                  new Terminal(Token::TIMES),
                  new ParsedTerm("E", deque<Symbol *>{
                      new Terminal(Token::ID_("d")),
                  }),
              }),
          }),
          new Terminal(Token::RPAREN),
      }),
      new Terminal(Token::TIMES),
      new ParsedTerm("E", deque<Symbol *>{
          new Terminal(Token::ID_("e")),
      }),
  });

  EXPECT_EQ(*parseTree, *expectedParseTree);
}

TEST(parser_test, test_simple_grammar) {
  using namespace tiger::lex;
  using namespace tiger::syntax;

  Grammar::Builder g;
  g.prod(g.nonterm("E"), {g.nonterm("E"), g.term(Token::PLUS), g.nonterm("T"),})
      .prod(g.nonterm("E"), {g.nonterm("T"),})
      .prod(g.nonterm("T"), {g.nonterm("T"), g.term(Token::TIMES), g.nonterm("F"),})
      .prod(g.nonterm("T"), {g.nonterm("F"),})
      .prod(g.nonterm("F"), {g.term(Token::LPAREN), g.nonterm("E"), g.term(Token::RPAREN),})
      .prod(g.nonterm("F"), {g.term(Token::ID),})
      .startAt(g.nonterm("E"));

  unique_ptr<const Grammar> grammar(new Grammar(g.build()));

  vector<unique_ptr<Terminal>> terms;
  vector<unique_ptr<NonTerminal>> nonterms;
  nonterms.emplace_back(new NonTerminal("E"));
  nonterms.emplace_back(new NonTerminal("T"));
  nonterms.emplace_back(new NonTerminal("F"));
  nonterms.emplace_back(new NonTerminal(" aug"));

  terms.emplace_back(new Terminal(Token::PLUS));
  terms.emplace_back(new Terminal(Token::TIMES));
  terms.emplace_back(new Terminal(Token::LPAREN));
  terms.emplace_back(new Terminal(Token::RPAREN));
  terms.emplace_back(new Terminal(Token::ID));
  terms.emplace_back(new Terminal(Token::EOF_TOK));

  vector<unique_ptr<Production>> productions;
  productions.emplace_back(new Production(g.nonterm(" aug"), {g.nonterm("E"),}));
  productions.emplace_back(new Production(g.nonterm("E"), {g.nonterm("E"), g.term(Token::PLUS), g.nonterm("T"),}));
  productions.emplace_back(new Production(g.nonterm("E"), {g.nonterm("T"),}));
  productions.emplace_back(new Production(g.nonterm("F"), {g.term(Token::LPAREN), g.nonterm("E"), g.term(Token::RPAREN),}));
  productions.emplace_back(new Production(g.nonterm("F"), {g.term(Token::ID),}));
  productions.emplace_back(new Production(g.nonterm("T"), {g.nonterm("T"), g.term(Token::TIMES), g.nonterm("F"),}));
  productions.emplace_back(new Production(g.nonterm("T"), {g.nonterm("F"),}));

  EXPECT_EQ(grammar->terminals().size(), terms.size());
  for (auto it1 = grammar->terminals().cbegin(), it2 = terms.cbegin();
       it1 != grammar->terminals().cend() && it2 != terms.cend();
       ++it1, ++it2) {
    EXPECT_EQ(**it1, **it2);
  }
  EXPECT_EQ(grammar->nonterminals().size(), nonterms.size());
  for (auto it1 = grammar->nonterminals().cbegin(), it2 = nonterms.cbegin();
       it1 != grammar->nonterminals().cend() && it2 != nonterms.cend();
       ++it1, ++it2) {
    EXPECT_EQ(**it1, **it2);
  }
  EXPECT_EQ(grammar->productions().size(), productions.size());
  for (auto it1 = grammar->productions().cbegin(), it2 = productions.cbegin();
       it1 != grammar->productions().cend() && it2 != productions.cend();
       ++it1, ++it2) {
    EXPECT_EQ(**it1, **it2);
  }

  vector<valarray<bool>> firstsOf(nonterms.size());
  vector<valarray<bool>> followsOf(nonterms.size());
  for (auto& firsts : firstsOf)
    firsts.resize(terms.size() + 1, false);
  for (auto& follows : followsOf)
    follows.resize(terms.size(), false);
  firstsOf[0][2] = firstsOf[0][4] = true;
  firstsOf[1][2] = firstsOf[1][4] = true;
  firstsOf[2][2] = firstsOf[2][4] = true;
  firstsOf[3][2] = firstsOf[3][4] = true;

  followsOf[0][0] = followsOf[0][3] = followsOf[0][5] = true;
  followsOf[1][0] = followsOf[1][1] = followsOf[1][3] = followsOf[1][5] = true;
  followsOf[2][0] = followsOf[2][1] = followsOf[2][3] = followsOf[2][5] = true;
  followsOf[3][5] = true;
  for (size_t i = 0; i < nonterms.size(); ++i) {
    EXPECT_TRUE((grammar->firstsOf(i) == firstsOf[i]).min());
    EXPECT_TRUE((grammar->followsOf(i) == followsOf[i]).min());
  }

  string program("a * b + c");
  Lexer lexer(program);
  auto parser = SLRParser::newInstance(std::move(grammar));


  auto parseTree = parser->parse(lexer);
  auto expectedParseTree = std::make_unique<ParsedTerm>("E", deque<Symbol*>{
      new ParsedTerm("E", deque<Symbol*>{
          new ParsedTerm("T", deque<Symbol*>{
              new ParsedTerm("T", deque<Symbol*>{
                  new ParsedTerm("F", deque<Symbol*>{
                      new Terminal(Token::ID_("a")),
                  }),
              }),
              new Terminal(Token::TIMES),
              new ParsedTerm("F", deque<Symbol*>{
                  new Terminal(Token::ID_("b")),
              }),
          }),
      }),
      new Terminal(Token::PLUS),
      new ParsedTerm("T", deque<Symbol*>{
          new ParsedTerm("F", deque<Symbol*>{
              new Terminal(Token::ID_("c")),
          }),
      }),
  });

  EXPECT_EQ(*parseTree, *expectedParseTree);
}

TEST(parser_test, test_grammar) {
  using namespace tiger::syntax;

  auto parser = SLRParser::newInstance(Grammar::tigerGrammar());
  const Grammar *grammar = parser->grammar();

  vector<unique_ptr<Terminal>> terms;
  vector<unique_ptr<NonTerminal>> nonterms;

  nonterms.emplace_back(new NonTerminal("exp"));
  nonterms.emplace_back(new NonTerminal("program"));
  nonterms.emplace_back(new NonTerminal("decs"));
  // nonterms.emplace_back(new NonTerminal("type_id"));
  nonterms.emplace_back(new NonTerminal("closure1"));
  nonterms.emplace_back(new NonTerminal("lvalue"));
  nonterms.emplace_back(new NonTerminal("closure2"));
  // nonterms.emplace_back(new NonTerminal("opexp"));
  nonterms.emplace_back(new NonTerminal("exps"));
  nonterms.emplace_back(new NonTerminal("closure3"));
  nonterms.emplace_back(new NonTerminal("closure4"));
  nonterms.emplace_back(new NonTerminal("closure5"));
  nonterms.emplace_back(new NonTerminal("dec"));
  nonterms.emplace_back(new NonTerminal("ty"));
  // nonterms.emplace_back(new NonTerminal("classfields"));
  nonterms.emplace_back(new NonTerminal("vardec"));
  nonterms.emplace_back(new NonTerminal("tyfields"));
  // nonterms.emplace_back(new NonTerminal("classfield"));
  nonterms.emplace_back(new NonTerminal("closure6"));
  nonterms.emplace_back(new NonTerminal(" aug"));

  terms.emplace_back(new Terminal(Token::NIL));
  terms.emplace_back(new Terminal(Token::INT));
  terms.emplace_back(new Terminal(Token::STRING));
  terms.emplace_back(new Terminal(Token::ID));
  terms.emplace_back(new Terminal(Token::LBRACK));
  terms.emplace_back(new Terminal(Token::RBRACK));
  terms.emplace_back(new Terminal(Token::OF));
  terms.emplace_back(new Terminal(Token::LBRACE));
  terms.emplace_back(new Terminal(Token::RBRACE));
  // terms.emplace_back(new Terminal(Token::ID));
  terms.emplace_back(new Terminal(Token::EQ));
  terms.emplace_back(new Terminal(Token::COMMA));
  // terms.emplace_back(new Terminal(Token::NEW));
  terms.emplace_back(new Terminal(Token::LPAREN));
  terms.emplace_back(new Terminal(Token::RPAREN));
  terms.emplace_back(new Terminal(Token::DOT));
  terms.emplace_back(new Terminal(Token::MINUS));
  terms.emplace_back(new Terminal(Token::ASSIGN));
  terms.emplace_back(new Terminal(Token::IF));
  terms.emplace_back(new Terminal(Token::THEN));
  terms.emplace_back(new Terminal(Token::ELSE));
  terms.emplace_back(new Terminal(Token::WHILE));
  terms.emplace_back(new Terminal(Token::DO));
  terms.emplace_back(new Terminal(Token::FOR));
  terms.emplace_back(new Terminal(Token::TO));
  terms.emplace_back(new Terminal(Token::BREAK));
  terms.emplace_back(new Terminal(Token::LET));
  terms.emplace_back(new Terminal(Token::IN));
  terms.emplace_back(new Terminal(Token::END));
  terms.emplace_back(new Terminal(Token::SEMICOLON));
  terms.emplace_back(new Terminal(Token::TYPE));
  // terms.emplace_back(new Terminal(Token::CLASS));
  // terms.emplace_back(new Terminal(Token::EXTENDS));
  terms.emplace_back(new Terminal(Token::FUNCTION));
  terms.emplace_back(new Terminal(Token::COLON));
  // terms.emplace_back(new Terminal(Token::PRIMITIVE));
  // terms.emplace_back(new Terminal(Token::IMPORT));
  terms.emplace_back(new Terminal(Token::VAR));
  // terms.emplace_back(new Terminal(Token::METHOD));
  terms.emplace_back(new Terminal(Token::ARRAY));
  terms.emplace_back(new Terminal(Token::TIMES));
  terms.emplace_back(new Terminal(Token::DIVIDE));
  terms.emplace_back(new Terminal(Token::PLUS));
  terms.emplace_back(new Terminal(Token::NEQ));
  terms.emplace_back(new Terminal(Token::GT));
  terms.emplace_back(new Terminal(Token::LT));
  terms.emplace_back(new Terminal(Token::GE));
  terms.emplace_back(new Terminal(Token::LE));
  terms.emplace_back(new Terminal(Token::AND));
  terms.emplace_back(new Terminal(Token::OR));
  terms.emplace_back(new Terminal(Token::EOF_TOK));

  EXPECT_EQ(grammar->terminals().size(), terms.size());
  for (auto it1 = grammar->terminals().cbegin(), it2 = terms.cbegin();
       it1 != grammar->terminals().cend() && it2 != terms.cend();
       ++it1, ++it2) {
    EXPECT_EQ(**it1, **it2);
  }
  EXPECT_EQ(grammar->nonterminals().size(), nonterms.size());
  for (auto it1 = grammar->nonterminals().cbegin(), it2 = nonterms.cbegin();
       it1 != grammar->nonterminals().cend() && it2 != nonterms.cend();
       ++it1, ++it2) {
    EXPECT_EQ(**it1, **it2);
  }
}

TEST(parser_test, test_parser) {
  using namespace tiger::lex;
  using namespace tiger::syntax;

  string program(R"(
    /* A program to solve the 8-queens problem */
    let
      var N := 8

      type intArray = array of int

      var row := intArray [ N ] of 0
      var col := intArray [ N ] of 0
      var diag1 := intArray [N+N-1] of 0
      var diag2 := intArray [N+N-1] of 0

      function printboard() =
          (for i := 0 to N-1
            do (for j := 0 to N-1
                 do print(if col[i]=j then " O" else " .");
                print("\n"));
            print("\n"))
      function try(c:int) =
        if c=N
        then printboard()
        else for r := 0 to N-1
              do if row[r]=0 & diag1[r+c]=0 & diag2[r+7-c]=0
                 then (row[r]:=1; diag1[r+c]:=1; diag2[r+7-c]:=1;
                       col[c]:=r;
                       try(c+1);
                       row[r]:=0; diag1[r+c]:=0; diag2[r+7-c]:=0)
      in try(0)
    end
  )");
  Lexer lexer(program);
  auto parser = SLRParser::newInstance(Grammar::tigerGrammar());
  unique_ptr<ParsedTerm> parseTree = parser->parse(lexer);
#ifndef NDEBUG
  std::clog << *parseTree;
#endif
  using Symbols = deque<Symbol*>;
  auto expectedParseTree = std::make_unique<ParsedTerm>("program", Symbols{
      new ParsedTerm("exp", Symbols{
          new Terminal(Token::LET),
          new ParsedTerm("decs", Symbols{
              new ParsedTerm("dec", Symbols{
                  new ParsedTerm("vardec", Symbols{
                      new Terminal(Token::VAR),
                      new Terminal(Token::ID_("N")),
                      new Terminal(Token::ASSIGN),
                      new ParsedTerm("exp", Symbols{
                          new Terminal(Token::INT_(8)),
                      }),
                  }),
              }),
              new ParsedTerm("dec", Symbols{
                  new Terminal(Token::TYPE),
                  new Terminal(Token::ID_("intArray")),
                  new Terminal(Token::EQ),
                  new ParsedTerm("ty", Symbols{
                      new Terminal(Token::ARRAY),
                      new Terminal(Token::OF),
                      new Terminal(Token::ID_("int")),
                  }),
              }),
              new ParsedTerm("dec", Symbols{
                  new ParsedTerm("vardec", Symbols{
                      new Terminal(Token::VAR),
                      new Terminal(Token::ID_("row")),
                      new Terminal(Token::ASSIGN),
                      new ParsedTerm("exp", Symbols{
                          new Terminal(Token::ID_("intArray")),
                          new Terminal(Token::LBRACK),
                          new ParsedTerm("exp", Symbols{
                              new ParsedTerm("lvalue", Symbols{
                                  new Terminal(Token::ID_("N")),
                              }),
                          }),
                          new Terminal(Token::RBRACK),
                          new Terminal(Token::OF),
                          new ParsedTerm("exp", Symbols{
                              new Terminal(Token::INT_(0)),
                          }),
                      }),
                  }),
              }),
              new ParsedTerm("dec", Symbols{
                  new ParsedTerm("vardec", Symbols{
                      new Terminal(Token::VAR),
                      new Terminal(Token::ID_("col")),
                      new Terminal(Token::ASSIGN),
                      new ParsedTerm("exp", Symbols{
                          new Terminal(Token::ID_("intArray")),
                          new Terminal(Token::LBRACK),
                          new ParsedTerm("exp", Symbols{
                              new ParsedTerm("lvalue", Symbols{
                                  new Terminal(Token::ID_("N")),
                              }),
                          }),
                          new Terminal(Token::RBRACK),
                          new Terminal(Token::OF),
                          new ParsedTerm("exp", Symbols{
                              new Terminal(Token::INT_(0)),
                          }),
                      }),
                  }),
              }),
              new ParsedTerm("dec", Symbols{
                  new ParsedTerm("vardec", Symbols{
                      new Terminal(Token::VAR),
                      new Terminal(Token::ID_("diag1")),
                      new Terminal(Token::ASSIGN),
                      new ParsedTerm("exp", Symbols{
                          new Terminal(Token::ID_("intArray")),
                          new Terminal(Token::LBRACK),
                          new ParsedTerm("exp", Symbols{
                              new ParsedTerm("exp", Symbols{
                                  new ParsedTerm("exp", Symbols{
                                      new ParsedTerm("lvalue", Symbols{
                                          new Terminal(Token::ID_("N")),
                                      }),
                                  }),
                                  new Terminal(Token::PLUS),
                                  new ParsedTerm("exp", Symbols{
                                      new ParsedTerm("lvalue", Symbols{
                                          new Terminal(Token::ID_("N")),
                                      }),
                                  }),
                              }),
                              new Terminal(Token::MINUS),
                              new ParsedTerm("exp", Symbols{
                                  new Terminal(Token::INT_(1)),
                              }),
                          }),
                          new Terminal(Token::RBRACK),
                          new Terminal(Token::OF),
                          new ParsedTerm("exp", Symbols{
                              new Terminal(Token::INT_(0)),
                          }),
                      }),
                  }),
              }),
              new ParsedTerm("dec", Symbols{
                  new ParsedTerm("vardec", Symbols{
                      new Terminal(Token::VAR),
                      new Terminal(Token::ID_("diag2")),
                      new Terminal(Token::ASSIGN),
                      new ParsedTerm("exp", Symbols{
                          new Terminal(Token::ID_("intArray")),
                          new Terminal(Token::LBRACK),
                          new ParsedTerm("exp", Symbols{
                              new ParsedTerm("exp", Symbols{
                                  new ParsedTerm("exp", Symbols{
                                      new ParsedTerm("lvalue", Symbols{
                                          new Terminal(Token::ID_("N")),
                                      }),
                                  }),
                                  new Terminal(Token::PLUS),
                                  new ParsedTerm("exp", Symbols{
                                      new ParsedTerm("lvalue", Symbols{
                                          new Terminal(Token::ID_("N")),
                                      }),
                                  }),
                              }),
                              new Terminal(Token::MINUS),
                              new ParsedTerm("exp", Symbols{
                                  new Terminal(Token::INT_(1)),
                              }),
                          }),
                          new Terminal(Token::RBRACK),
                          new Terminal(Token::OF),
                          new ParsedTerm("exp", Symbols{
                              new Terminal(Token::INT_(0)),
                          }),
                      }),
                  }),
              }),
              new ParsedTerm("dec", Symbols{
                  new Terminal(Token::FUNCTION),
                  new Terminal(Token::ID_("printboard")),
                  new Terminal(Token::LPAREN),
                  new Terminal(Token::RPAREN),
                  new Terminal(Token::EQ),
                  new ParsedTerm("exp", Symbols{
                      new Terminal(Token::LPAREN),
                      new ParsedTerm("exps", Symbols{
                          new ParsedTerm("exp", Symbols{
                              new Terminal(Token::FOR),
                              new Terminal(Token::ID_("i")),
                              new Terminal(Token::ASSIGN),
                              new ParsedTerm("exp", Symbols{
                                  new Terminal(Token::INT_(0)),
                              }),
                              new Terminal(Token::TO),
                              new ParsedTerm("exp", Symbols{
                                  new ParsedTerm("exp", Symbols{
                                      new ParsedTerm("lvalue", Symbols{
                                          new Terminal(Token::ID_("N")),
                                      }),
                                  }),
                                  new Terminal(Token::MINUS),
                                  new ParsedTerm("exp", Symbols{
                                      new Terminal(Token::INT_(1)),
                                  }),
                              }),
                              new Terminal(Token::DO),
                              new ParsedTerm("exp", Symbols{
                                  new Terminal(Token::LPAREN),
                                  new ParsedTerm("exps", Symbols{
                                      new ParsedTerm("exp", Symbols{
                                          new Terminal(Token::FOR),
                                          new Terminal(Token::ID_("j")),
                                          new Terminal(Token::ASSIGN),
                                          new ParsedTerm("exp", Symbols{
                                              new Terminal(Token::INT_(0)),
                                          }),
                                          new Terminal(Token::TO),
                                          new ParsedTerm("exp", Symbols{
                                              new ParsedTerm("exp", Symbols{
                                                  new ParsedTerm("lvalue", Symbols{
                                                      new Terminal(Token::ID_("N")),
                                                  }),
                                              }),
                                              new Terminal(Token::MINUS),
                                              new ParsedTerm("exp", Symbols{
                                                  new Terminal(Token::INT_(1)),
                                              }),
                                          }),
                                          new Terminal(Token::DO),
                                          new ParsedTerm("exp", Symbols{
                                              new Terminal(Token::ID_("print")),
                                              new Terminal(Token::LPAREN),
                                              new ParsedTerm("exp", Symbols{
                                                  new Terminal(Token::IF),
                                                  new ParsedTerm("exp", Symbols{
                                                      new ParsedTerm("exp", Symbols{
                                                          new ParsedTerm("lvalue", Symbols{
                                                              new Terminal(Token::ID_("col")),
                                                              new Terminal(Token::LBRACK),
                                                              new ParsedTerm("exp", Symbols{
                                                                  new ParsedTerm("lvalue", Symbols{
                                                                      new Terminal(Token::ID_("i")),
                                                                  }),
                                                              }),
                                                              new Terminal(Token::RBRACK),
                                                          }),
                                                      }),
                                                      new Terminal(Token::EQ),
                                                      new ParsedTerm("exp", Symbols{
                                                          new ParsedTerm("lvalue", Symbols{
                                                              new Terminal(Token::ID_("j")),
                                                          }),
                                                      }),
                                                  }),
                                                  new Terminal(Token::THEN),
                                                  new ParsedTerm("exp", Symbols{
                                                      new Terminal(Token::STRING_(" O")),
                                                  }),
                                                  new Terminal(Token::ELSE),
                                                  new ParsedTerm("exp", Symbols{
                                                      new Terminal(Token::STRING_(" .")),
                                                  }),
                                              }),
                                              new Terminal(Token::RPAREN),
                                          }),
                                      }),
                                      new Terminal(Token::SEMICOLON),
                                      new ParsedTerm("exp", Symbols{
                                          new Terminal(Token::ID_("print")),
                                          new Terminal(Token::LPAREN),
                                          new ParsedTerm("exp", Symbols{
                                              new Terminal(Token::STRING_("\n")),
                                          }),
                                          new Terminal(Token::RPAREN),
                                      }),
                                  }),
                                  new Terminal(Token::RPAREN),
                              }),
                          }),
                          new Terminal(Token::SEMICOLON),
                          new ParsedTerm("exp", Symbols{
                              new Terminal(Token::ID_("print")),
                              new Terminal(Token::LPAREN),
                              new ParsedTerm("exp", Symbols{
                                  new Terminal(Token::STRING_("\n")),
                              }),
                              new Terminal(Token::RPAREN),
                          }),
                      }),
                      new Terminal(Token::RPAREN),
                  }),
              }),
              new ParsedTerm("dec", Symbols{
                  new Terminal(Token::FUNCTION),
                  new Terminal(Token::ID_("try")),
                  new Terminal(Token::LPAREN),
                  new ParsedTerm("tyfields", Symbols{
                      new Terminal(Token::ID_("c")),
                      new Terminal(Token::COLON),
                      new Terminal(Token::ID_("int")),
                  }),
                  new Terminal(Token::RPAREN),
                  new Terminal(Token::EQ),
                  new ParsedTerm("exp", Symbols{
                      new Terminal(Token::IF),
                      new ParsedTerm("exp", Symbols{
                          new ParsedTerm("exp", Symbols{
                              new ParsedTerm("lvalue", Symbols{
                                  new Terminal(Token::ID_("c")),
                              }),
                          }),
                          new Terminal(Token::EQ),
                          new ParsedTerm("exp", Symbols{
                              new ParsedTerm("lvalue", Symbols{
                                  new Terminal(Token::ID_("N")),
                              }),
                          }),
                      }),
                      new Terminal(Token::THEN),
                      new ParsedTerm("exp", Symbols{
                          new Terminal(Token::ID_("printboard")),
                          new Terminal(Token::LPAREN),
                          new Terminal(Token::RPAREN),
                      }),
                      new Terminal(Token::ELSE),
                      new ParsedTerm("exp", Symbols{
                          new Terminal(Token::FOR),
                          new Terminal(Token::ID_("r")),
                          new Terminal(Token::ASSIGN),
                          new ParsedTerm("exp", Symbols{
                              new Terminal(Token::INT_(0)),
                          }),
                          new Terminal(Token::TO),
                          new ParsedTerm("exp", Symbols{
                              new ParsedTerm("exp", Symbols{
                                  new ParsedTerm("lvalue", Symbols{
                                      new Terminal(Token::ID_("N")),
                                  }),
                              }),
                              new Terminal(Token::MINUS),
                              new ParsedTerm("exp", Symbols{
                                  new Terminal(Token::INT_(1)),
                              }),
                          }),
                          new Terminal(Token::DO),
                          new ParsedTerm("exp", Symbols{
                              new Terminal(Token::IF),
                              new ParsedTerm("exp", Symbols{
                                  new ParsedTerm("exp", Symbols{
                                      new ParsedTerm("exp", Symbols{
                                          new ParsedTerm("exp", Symbols{
                                              new ParsedTerm("lvalue", Symbols{
                                                  new Terminal(Token::ID_("row")),
                                                  new Terminal(Token::LBRACK),
                                                  new ParsedTerm("exp", Symbols{
                                                      new ParsedTerm("lvalue", Symbols{
                                                          new Terminal(Token::ID_("r")),
                                                      }),
                                                  }),
                                                  new Terminal(Token::RBRACK),
                                              }),
                                          }),
                                          new Terminal(Token::EQ),
                                          new ParsedTerm("exp", Symbols{
                                              new Terminal(Token::INT_(0)),
                                          }),
                                      }),
                                      new Terminal(Token::AND),
                                      new ParsedTerm("exp", Symbols{
                                          new ParsedTerm("exp", Symbols{
                                              new ParsedTerm("lvalue", Symbols{
                                                  new Terminal(Token::ID_("diag1")),
                                                  new Terminal(Token::LBRACK),
                                                  new ParsedTerm("exp", Symbols{
                                                      new ParsedTerm("exp", Symbols{
                                                          new ParsedTerm("lvalue", Symbols{
                                                              new Terminal(Token::ID_("r")),
                                                          }),
                                                      }),
                                                      new Terminal(Token::PLUS),
                                                      new ParsedTerm("exp", Symbols{
                                                          new ParsedTerm("lvalue", Symbols{
                                                              new Terminal(Token::ID_("c")),
                                                          }),
                                                      }),
                                                  }),
                                                  new Terminal(Token::RBRACK),
                                              }),
                                          }),
                                          new Terminal(Token::EQ),
                                          new ParsedTerm("exp", Symbols{
                                              new Terminal(Token::INT_(0)),
                                          }),
                                      }),
                                  }),
                                  new Terminal(Token::AND),
                                  new ParsedTerm("exp", Symbols{
                                      new ParsedTerm("exp", Symbols{
                                          new ParsedTerm("lvalue", Symbols{
                                              new Terminal(Token::ID_("diag2")),
                                              new Terminal(Token::LBRACK),
                                              new ParsedTerm("exp", Symbols{
                                                  new ParsedTerm("exp", Symbols{
                                                      new ParsedTerm("exp", Symbols{
                                                          new ParsedTerm("lvalue", Symbols{
                                                              new Terminal(Token::ID_("r")),
                                                          }),
                                                      }),
                                                      new Terminal(Token::PLUS),
                                                      new ParsedTerm("exp", Symbols{
                                                          new Terminal(Token::INT_(7)),
                                                      }),
                                                  }),
                                                  new Terminal(Token::MINUS),
                                                  new ParsedTerm("exp", Symbols{
                                                      new ParsedTerm("lvalue", Symbols{
                                                          new Terminal(Token::ID_("c")),
                                                      }),
                                                  }),
                                              }),
                                              new Terminal(Token::RBRACK),
                                          }),
                                      }),
                                      new Terminal(Token::EQ),
                                      new ParsedTerm("exp", Symbols{
                                          new Terminal(Token::INT_(0)),
                                      }),
                                  }),
                              }),
                              new Terminal(Token::THEN),
                              new ParsedTerm("exp", Symbols{
                                  new Terminal(Token::LPAREN),
                                  new ParsedTerm("exps", Symbols{
                                      new ParsedTerm("exp", Symbols{
                                          new ParsedTerm("lvalue", Symbols{
                                              new Terminal(Token::ID_("row")),
                                              new Terminal(Token::LBRACK),
                                              new ParsedTerm("exp", Symbols{
                                                  new ParsedTerm("lvalue", Symbols{
                                                      new Terminal(Token::ID_("r")),
                                                  }),
                                              }),
                                              new Terminal(Token::RBRACK),
                                          }),
                                          new Terminal(Token::ASSIGN),
                                          new ParsedTerm("exp", Symbols{
                                              new Terminal(Token::INT_(1)),
                                          }),
                                      }),
                                      new Terminal(Token::SEMICOLON),
                                      new ParsedTerm("exp", Symbols{
                                          new ParsedTerm("lvalue", Symbols{
                                              new Terminal(Token::ID_("diag1")),
                                              new Terminal(Token::LBRACK),
                                              new ParsedTerm("exp", Symbols{
                                                  new ParsedTerm("exp", Symbols{
                                                      new ParsedTerm("lvalue", Symbols{
                                                          new Terminal(Token::ID_("r")),
                                                      }),
                                                  }),
                                                  new Terminal(Token::PLUS),
                                                  new ParsedTerm("exp", Symbols{
                                                      new ParsedTerm("lvalue", Symbols{
                                                          new Terminal(Token::ID_("c")),
                                                      }),
                                                  }),
                                              }),
                                              new Terminal(Token::RBRACK),
                                          }),
                                          new Terminal(Token::ASSIGN),
                                          new ParsedTerm("exp", Symbols{
                                              new Terminal(Token::INT_(1)),
                                          }),
                                      }),
                                      new Terminal(Token::SEMICOLON),
                                      new ParsedTerm("exp", Symbols{
                                          new ParsedTerm("lvalue", Symbols{
                                              new Terminal(Token::ID_("diag2")),
                                              new Terminal(Token::LBRACK),
                                              new ParsedTerm("exp", Symbols{
                                                  new ParsedTerm("exp", Symbols{
                                                      new ParsedTerm("exp", Symbols{
                                                          new ParsedTerm("lvalue", Symbols{
                                                              new Terminal(Token::ID_("r")),
                                                          }),
                                                      }),
                                                      new Terminal(Token::PLUS),
                                                      new ParsedTerm("exp", Symbols{
                                                          new Terminal(Token::INT_(7)),
                                                      }),
                                                  }),
                                                  new Terminal(Token::MINUS),
                                                  new ParsedTerm("exp", Symbols{
                                                      new ParsedTerm("lvalue", Symbols{
                                                          new Terminal(Token::ID_("c")),
                                                      }),
                                                  }),
                                              }),
                                              new Terminal(Token::RBRACK),
                                          }),
                                          new Terminal(Token::ASSIGN),
                                          new ParsedTerm("exp", Symbols{
                                              new Terminal(Token::INT_(1)),
                                          }),
                                      }),
                                      new Terminal(Token::SEMICOLON),
                                      new ParsedTerm("exp", Symbols{
                                          new ParsedTerm("lvalue", Symbols{
                                              new Terminal(Token::ID_("col")),
                                              new Terminal(Token::LBRACK),
                                              new ParsedTerm("exp", Symbols{
                                                  new ParsedTerm("lvalue", Symbols{
                                                      new Terminal(Token::ID_("c")),
                                                  }),
                                              }),
                                              new Terminal(Token::RBRACK),
                                          }),
                                          new Terminal(Token::ASSIGN),
                                          new ParsedTerm("exp", Symbols{
                                              new ParsedTerm("lvalue", Symbols{
                                                  new Terminal(Token::ID_("r")),
                                              }),
                                          }),
                                      }),
                                      new Terminal(Token::SEMICOLON),
                                      new ParsedTerm("exp", Symbols{
                                          new Terminal(Token::ID_("try")),
                                          new Terminal(Token::LPAREN),
                                          new ParsedTerm("exp", Symbols{
                                              new ParsedTerm("exp", Symbols{
                                                  new ParsedTerm("lvalue", Symbols{
                                                      new Terminal(Token::ID_("c")),
                                                  }),
                                              }),
                                              new Terminal(Token::PLUS),
                                              new ParsedTerm("exp", Symbols{
                                                  new Terminal(Token::INT_(1)),
                                              }),
                                          }),
                                          new Terminal(Token::RPAREN),
                                      }),
                                      new Terminal(Token::SEMICOLON),
                                      new ParsedTerm("exp", Symbols{
                                          new ParsedTerm("lvalue", Symbols{
                                              new Terminal(Token::ID_("row")),
                                              new Terminal(Token::LBRACK),
                                              new ParsedTerm("exp", Symbols{
                                                  new ParsedTerm("lvalue", Symbols{
                                                      new Terminal(Token::ID_("r")),
                                                  }),
                                              }),
                                              new Terminal(Token::RBRACK),
                                          }),
                                          new Terminal(Token::ASSIGN),
                                          new ParsedTerm("exp", Symbols{
                                              new Terminal(Token::INT_(0)),
                                          }),
                                      }),
                                      new Terminal(Token::SEMICOLON),
                                      new ParsedTerm("exp", Symbols{
                                          new ParsedTerm("lvalue", Symbols{
                                              new Terminal(Token::ID_("diag1")),
                                              new Terminal(Token::LBRACK),
                                              new ParsedTerm("exp", Symbols{
                                                  new ParsedTerm("exp", Symbols{
                                                      new ParsedTerm("lvalue", Symbols{
                                                          new Terminal(Token::ID_("r")),
                                                      }),
                                                  }),
                                                  new Terminal(Token::PLUS),
                                                  new ParsedTerm("exp", Symbols{
                                                      new ParsedTerm("lvalue", Symbols{
                                                          new Terminal(Token::ID_("c")),
                                                      }),
                                                  }),
                                              }),
                                              new Terminal(Token::RBRACK),
                                          }),
                                          new Terminal(Token::ASSIGN),
                                          new ParsedTerm("exp", Symbols{
                                              new Terminal(Token::INT_(0)),
                                          }),
                                      }),
                                      new Terminal(Token::SEMICOLON),
                                      new ParsedTerm("exp", Symbols{
                                          new ParsedTerm("lvalue", Symbols{
                                              new Terminal(Token::ID_("diag2")),
                                              new Terminal(Token::LBRACK),
                                              new ParsedTerm("exp", Symbols{
                                                  new ParsedTerm("exp", Symbols{
                                                      new ParsedTerm("exp", Symbols{
                                                          new ParsedTerm("lvalue", Symbols{
                                                              new Terminal(Token::ID_("r")),
                                                          }),
                                                      }),
                                                      new Terminal(Token::PLUS),
                                                      new ParsedTerm("exp", Symbols{
                                                          new Terminal(Token::INT_(7)),
                                                      }),
                                                  }),
                                                  new Terminal(Token::MINUS),
                                                  new ParsedTerm("exp", Symbols{
                                                      new ParsedTerm("lvalue", Symbols{
                                                          new Terminal(Token::ID_("c")),
                                                      }),
                                                  }),
                                              }),
                                              new Terminal(Token::RBRACK),
                                          }),
                                          new Terminal(Token::ASSIGN),
                                          new ParsedTerm("exp", Symbols{
                                              new Terminal(Token::INT_(0)),
                                          }),
                                      }),
                                  }),
                                  new Terminal(Token::RPAREN),
                              }),
                          }),
                      }),
                  }),
              }),
          }),
          new Terminal(Token::IN),
          new ParsedTerm("exps", Symbols{
              new ParsedTerm("exp", Symbols{
                  new Terminal(Token::ID_("try")),
                  new Terminal(Token::LPAREN),
                  new ParsedTerm("exp", Symbols{
                      new Terminal(Token::INT_(0)),
                  }),
                  new Terminal(Token::RPAREN),
              }),
          }),
          new Terminal(Token::END),
      }),
  });
  EXPECT_EQ(*parseTree, *expectedParseTree);
}

TEST(parser_test, test_delanging_else) {
  using namespace tiger::lex;
  using namespace tiger::syntax;

  string program(R"(
    if 10 > 5 then
      if 2 > 1 then
        3
      else
        2
  )");
  Lexer lexer(program);
  auto parser = SLRParser::newInstance(Grammar::tigerGrammar());
  unique_ptr<ParsedTerm> parseTree = parser->parse(lexer);
#ifndef NDEBUG
  std::clog << *parseTree;
#endif
  using Symbols = deque<Symbol*>;
  auto expectedParseTree = std::make_unique<ParsedTerm>("program", Symbols{
      new ParsedTerm("exp", Symbols{
          new Terminal(Token::IF),
          new ParsedTerm("exp", Symbols{
              new ParsedTerm("exp", Symbols{
                  new Terminal(Token::INT_(10)),
              }),
              new Terminal(Token::GT),
              new ParsedTerm("exp", Symbols{
                  new Terminal(Token::INT_(5)),
              }),
          }),
          new Terminal(Token::THEN),
          new ParsedTerm("exp", Symbols{
              new Terminal(Token::IF),
              new ParsedTerm("exp", Symbols{
                  new ParsedTerm("exp", Symbols{
                      new Terminal(Token::INT_(2)),
                  }),
                  new Terminal(Token::GT),
                  new ParsedTerm("exp", Symbols{
                      new Terminal(Token::INT_(1)),
                  }),
              }),
              new Terminal(Token::THEN),
              new ParsedTerm("exp", Symbols{
                  new Terminal(Token::INT_(3)),
              }),
              new Terminal(Token::ELSE),
              new ParsedTerm("exp", Symbols{
                  new Terminal(Token::INT_(2)),
              }),
          }),
      }),
  });
  EXPECT_EQ(*parseTree, *expectedParseTree);
}
