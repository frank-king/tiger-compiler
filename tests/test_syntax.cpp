//
// Created by whjpji on 18-7-21.
//

#include <parser.h>
#include "gtest/gtest.h"
#include "syntax.h"

TEST(tiger_test, test_print_syntax) {
  using namespace tiger::syntax;
  using tiger::lex::Position;
  AbstractSyntax syntax;
  Program *program = syntax.elem<SeqExpr>(
      Position{1, 1}, syntax.elem<ExprList>(
          Position{1, 1}, vector<Expr*>{
              syntax.elem<AssignExpr>(
                  Position{1, 1},
                  syntax.elem<SimpleVar>(Position{1, 1}, string("a")),
                  syntax.elem<OpExpr>(
                      Position{1, 5}, OpExpr::PLUS,
                      syntax.elem<IntExpr>(Position{1, 5}, 5),
                      syntax.elem<IntExpr>(Position{1, 9}, 3))),
              syntax.elem<AssignExpr>(
                  Position{2, 1},
                  syntax.elem<SimpleVar>(Position{2, 1}, string("b")),
                  syntax.elem<SeqExpr>(
                      Position{2, 5},
                      syntax.elem<ExprList>(
                          Position{2, 5}, vector<Expr*>{
                              syntax.elem<CallExpr>(
                                  Position{2, 6}, string("print"), vector<Expr*>{
                                      syntax.elem<SimpleVar>(Position{2, 13}, string("a")),
                                      syntax.elem<OpExpr>(
                                          Position{2, 13}, OpExpr::MINUS,
                                          syntax.elem<SimpleVar>(Position{2, 15}, string("a")),
                                          syntax.elem<IntExpr>(Position{2, 17}, 1)),
                                  }),
                              syntax.elem<OpExpr>(
                                  Position{2, 21}, OpExpr::TIMES,
                                  syntax.elem<IntExpr>(Position{2, 21}, 10),
                                  syntax.elem<SimpleVar>(Position{2, 26}, string("a"))),
                          }))),
              syntax.elem<CallExpr>(
                  Position{3, 1}, string("print"), vector<Expr*>{
                      syntax.elem<SimpleVar>(Position{3, 7}, string("b")),
                  }),
          }));
  std::stringstream ss;
  ss << *program;
  std::string program_str = std::move(ss.str());
  string expected_program_str = R"((
a := 5 + 3;
b := (
print(a, a - 1);
10 * a
);
print(b)
))";
  EXPECT_EQ(program_str, expected_program_str);
}

TEST(tiger_test, test_abstract_syntax) {
  using namespace tiger::syntax;
  using tiger::lex::Position;
  using namespace tiger::lex;
  using namespace tiger::grammar;

  string programStr(R"(
    /* A program to solve the 8-queens problem */
    let
      var N := 8

      kind intArray = array of int

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
  Lexer lexer(programStr);
  auto parser = SLRParser::newInstance(Grammar::tigerGrammar());
  unique_ptr<ParsedTerm> parseTree = parser->parse(lexer);
  AbstractSyntax syntax;
  Program *program = syntax.abstract(parseTree.get());
  std::clog << *program;
  /*
  Program *expectedProgram = syntax.elem<SeqExpr>(
      Position{1, 1}, syntax.elem<ExprList>(
          Position{1, 1}, vector<Expr*>{
              syntax.elem<AssignExpr>(
                  Position{1, 1},
                  syntax.elem<SimpleVar>(Position{1, 1}, string("a")),
                  syntax.elem<OpExpr>(
                      Position{1, 5}, OpExpr::PLUS,
                      syntax.elem<IntExpr>(Position{1, 5}, 5),
                      syntax.elem<IntExpr>(Position{1, 9}, 3))),
              syntax.elem<AssignExpr>(
                  Position{2, 1},
                  syntax.elem<SimpleVar>(Position{2, 1}, string("b")),
                  syntax.elem<SeqExpr>(
                      Position{2, 5},
                      syntax.elem<ExprList>(
                          Position{2, 5}, vector<Expr*>{
                              syntax.elem<CallExpr>(
                                  Position{2, 6}, string("print"), vector<Expr*>{
                                      syntax.elem<SimpleVar>(Position{2, 13}, string("a")),
                                      syntax.elem<OpExpr>(
                                          Position{2, 13}, OpExpr::MINUS,
                                          syntax.elem<SimpleVar>(Position{2, 15}, string("a")),
                                          syntax.elem<IntExpr>(Position{2, 17}, 1)),
                                  }),
                              syntax.elem<OpExpr>(
                                  Position{2, 21}, OpExpr::TIMES,
                                  syntax.elem<IntExpr>(Position{2, 21}, 10),
                                  syntax.elem<SimpleVar>(Position{2, 26}, string("a"))),
                          }))),
              syntax.elem<CallExpr>(
                  Position{3, 1}, string("print"), vector<Expr*>{
                      syntax.elem<SimpleVar>(Position{3, 7}, string("b")),
                  }),
          }));
  */
}
