//
// Created by whjpji on 18-7-23.
//

#include <boost/log/trivial.hpp>
#include "gtest/gtest.h"
#include "parser.h"

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

  vector<vector<bool>> firstsOf(nonterms.size());
  vector<vector<bool>> followsOf(nonterms.size());
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
    EXPECT_EQ(grammar->firstsOf(i), firstsOf[i]);
    EXPECT_EQ(grammar->followsOf(i), followsOf[i]);
  }

  string program("a * b + c");
  Lexer lexer(program);
  auto parser = SLRParser::newInstance(std::move(grammar));


  auto parseTree = parser->parse(lexer);
  auto expectedParseTree = std::make_unique<ParsedTerm>("E", deque<Symbol*>{
    new ParsedTerm("E", deque<Symbol*>{
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
  nonterms.emplace_back(new NonTerminal("type_id"));
  nonterms.emplace_back(new NonTerminal("closure1"));
  nonterms.emplace_back(new NonTerminal("lvalue"));
  nonterms.emplace_back(new NonTerminal("closure2"));
  // nonterms.emplace_back(new NonTerminal("opexp"));
  nonterms.emplace_back(new NonTerminal("exps"));
  nonterms.emplace_back(new NonTerminal("closure3"));
  nonterms.emplace_back(new NonTerminal("dec"));
  nonterms.emplace_back(new NonTerminal("ty"));
  // nonterms.emplace_back(new NonTerminal("classfields"));
  nonterms.emplace_back(new NonTerminal("vardec"));
  nonterms.emplace_back(new NonTerminal("tyfields"));
  // nonterms.emplace_back(new NonTerminal("classfield"));
  nonterms.emplace_back(new NonTerminal("closure4"));
  nonterms.emplace_back(new NonTerminal(" aug"));

  terms.emplace_back(new Terminal(Token::NIL));
  terms.emplace_back(new Terminal(Token::INT));
  terms.emplace_back(new Terminal(Token::STRING));
  terms.emplace_back(new Terminal(Token::LBRACK));
  terms.emplace_back(new Terminal(Token::RBRACK));
  terms.emplace_back(new Terminal(Token::OF));
  terms.emplace_back(new Terminal(Token::LBRACE));
  terms.emplace_back(new Terminal(Token::RBRACE));
  terms.emplace_back(new Terminal(Token::ID));
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
  BOOST_LOG_TRIVIAL(debug) << *parseTree;
  /*
  using Symbols = deque<unique_ptr<Symbol>>;
  auto expectedParseTree = std::make_unique<ParsedTerm>(
      "_aug", Symbols{
        new ParsedTerm("program", Symbols{
            new ParsedTerm("exp", Symbols{
                new Terminal(Token::LET),
                new ParsedTerm("decs"),
                new Terminal(Token::IN),
                new ParsedTerm("exps"),
                new Terminal(Token::END),
            }),
        }),
      });
   */
}
