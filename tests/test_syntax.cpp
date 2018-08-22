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
  Lexer lexer(programStr);
  auto parser = SLRParser::newInstance(Grammar::tigerGrammar());
  unique_ptr<ParsedTerm> parseTree = parser->parse(lexer);
  AbstractSyntax syntax;
  Program *program = syntax.abstract(parseTree.get());
#ifndef NDEBUG
  std::clog << *program;
#endif
  Program *expectedProgram = syntax.elem<LetExpr>(
      Position{3, 5},
      syntax.elem<DeclList>(
          Position{4, 7}, vector<Declaration*>{
              syntax.elem<VarDecl>(
                  Position{4, 7}, "N", "",
                  syntax.elem<IntExpr>(Position{4, 16}, 8)),
              syntax.elem<TypeDecl>(
                  Position{6, 7}, "intArray",
                  syntax.elem<ArrayType>(Position{6, 23}, "int")),
              syntax.elem<VarDecl>(
                  Position{8, 7}, "row", "",
                  syntax.elem<ArrayExpr>(
                      Position{8, 18}, "intArray",
                      syntax.elem<SimpleVar>(Position{8, 29}, "N"),
                      syntax.elem<IntExpr>(Position{8, 36}, 0))),
              syntax.elem<VarDecl>(
                  Position{9, 7}, "col", "",
                  syntax.elem<ArrayExpr>(
                      Position{9, 18}, "intArray",
                      syntax.elem<SimpleVar>(Position{9, 29}, "N"),
                      syntax.elem<IntExpr>(Position{9, 36}, 0))),
              syntax.elem<VarDecl>(
                  Position{10, 7}, "diag1", "",
                  syntax.elem<ArrayExpr>(
                      Position{10, 20}, "intArray",
                      syntax.elem<OpExpr>(
                          Position{10, 30}, OpExpr::MINUS,
                          syntax.elem<OpExpr>(
                              Position{10, 30}, OpExpr::PLUS,
                              syntax.elem<SimpleVar>(Position{10, 30}, "N"),
                              syntax.elem<SimpleVar>(Position{10, 32}, "N")),
                          syntax.elem<IntExpr>(Position{10, 34}, 1)),
                      syntax.elem<IntExpr>(Position{10, 40}, 0))),
              syntax.elem<VarDecl>(
                  Position{11, 7}, "diag2", "",
                  syntax.elem<ArrayExpr>(
                      Position{11, 20}, "intArray",
                      syntax.elem<OpExpr>(
                          Position{11, 30}, OpExpr::MINUS,
                          syntax.elem<OpExpr>(
                              Position{11, 30}, OpExpr::PLUS,
                              syntax.elem<SimpleVar>(Position{11, 30}, "N"),
                              syntax.elem<SimpleVar>(Position{11, 32}, "N")),
                          syntax.elem<IntExpr>(Position{11, 34}, 1)),
                      syntax.elem<IntExpr>(Position{11, 40}, 0))),
              syntax.elem<FuncDecl>(
                  Position{13, 7}, "printboard",
                  syntax.elem<FieldList>(Position{}, vector<Field*>{}), "",
                  syntax.elem<SeqExpr>(
                      Position{14, 11},
                      syntax.elem<ExprList>(
                          Position{14, 12}, vector<Expr*>{
                              syntax.elem<ForExpr>(
                                  Position{14, 12}, "i",
                                  syntax.elem<IntExpr>(Position{14, 21}, 0),
                                  syntax.elem<OpExpr>(
                                      Position{14, 26}, OpExpr::MINUS,
                                      syntax.elem<SimpleVar>(Position{14, 26}, "N"),
                                      syntax.elem<IntExpr>(Position{14, 28}, 1)),
                                  syntax.elem<SeqExpr>(
                                      Position{15, 16},
                                      syntax.elem<ExprList>(
                                          Position{15, 17}, vector<Expr*>{
                                              syntax.elem<ForExpr>(
                                                  Position{15, 17}, "j",
                                                  syntax.elem<IntExpr>(Position{15, 26}, 0),
                                                  syntax.elem<OpExpr>(
                                                      Position{15, 31}, OpExpr::MINUS,
                                                      syntax.elem<SimpleVar>(Position{15, 31}, "N"),
                                                      syntax.elem<IntExpr>(Position{15, 33}, 1)),
                                                  syntax.elem<CallExpr>(
                                                      Position{16, 21}, "print", vector<Expr*>{
                                                          syntax.elem<IfExpr>(
                                                              Position{16, 27},
                                                              syntax.elem<OpExpr>(
                                                                  Position{16, 30}, OpExpr::EQ,
                                                                  syntax.elem<SubscriptVar>(
                                                                      Position{16, 30},
                                                                      syntax.elem<SimpleVar>(Position{16, 30}, "col"),
                                                                      syntax.elem<SimpleVar>(Position{16, 34}, "i")),
                                                                  syntax.elem<SimpleVar>(Position{16, 37}, "j")),
                                                              syntax.elem<StrExpr>(Position{16, 44}, " O"),
                                                              syntax.elem<StrExpr>(Position{16, 54}, " .")),
                                                      })),
                                              syntax.elem<CallExpr>(
                                                  Position{17, 17}, "print", vector<Expr*>{
                                                      syntax.elem<StrExpr>(Position{17, 23}, "\n"),
                                                  }),
                                          }))),
                              syntax.elem<CallExpr>(
                                  Position{18, 13}, "print", vector<Expr*>{
                                      syntax.elem<StrExpr>(Position{18, 19}, "\n"),
                                  }),
                          }))),
              syntax.elem<FuncDecl>(
                  Position{19, 7}, "try",
                  syntax.elem<FieldList>(
                      Position{19, 20}, vector<Field*>{
                          syntax.elem<Field>(Position{19, 20}, "c", "int"),
                      }), "",
                  syntax.elem<IfExpr>(
                      Position{20, 9},
                      syntax.elem<OpExpr>(
                          Position{20, 12}, OpExpr::EQ,
                          syntax.elem<SimpleVar>(Position{20, 12}, "c"),
                          syntax.elem<SimpleVar>(Position{20, 14}, "N")),
                      syntax.elem<CallExpr>(
                          Position{21, 14}, "printboard", vector<Expr*>{}),
                      syntax.elem<ForExpr>(
                          Position{22, 14}, "r",
                          syntax.elem<IntExpr>(Position{22, 23}, 0),
                          syntax.elem<OpExpr>(
                              Position{22, 28}, OpExpr::MINUS,
                              syntax.elem<SimpleVar>(Position{22, 28}, "N"),
                              syntax.elem<IntExpr>(Position{22, 30}, 1)),
                          syntax.elem<IfExpr>(
                              Position{23, 18},
                              syntax.elem<IfExpr>(
                                  Position{23, 21},
                                  syntax.elem<IfExpr>(
                                      Position{23, 21},
                                      syntax.elem<OpExpr>(
                                          Position{23, 21}, OpExpr::EQ,
                                          syntax.elem<SubscriptVar>(
                                              Position{23, 21},
                                              syntax.elem<SimpleVar>(Position{23, 21}, "row"),
                                              syntax.elem<SimpleVar>(Position{23, 25}, "r")),
                                          syntax.elem<IntExpr>(Position{23, 28}, 0)),
                                      syntax.elem<OpExpr>(
                                          Position{23, 32}, OpExpr::EQ,
                                          syntax.elem<SubscriptVar>(
                                              Position{23, 32},
                                              syntax.elem<SimpleVar>(Position{23, 32}, "diag1"),
                                              syntax.elem<OpExpr>(
                                                  Position{23, 38}, OpExpr::PLUS,
                                                  syntax.elem<SimpleVar>(Position{23, 38}, "r"),
                                                  syntax.elem<SimpleVar>(Position{23, 40}, "c"))),
                                          syntax.elem<IntExpr>(Position{23, 43}, 0)),
                                      syntax.elem<IntExpr>(Position{}, 0)),
                                  syntax.elem<OpExpr>(
                                      Position{23, 47}, OpExpr::EQ,
                                      syntax.elem<SubscriptVar>(
                                          Position{23, 47},
                                          syntax.elem<SimpleVar>(Position{23, 47}, "diag2"),
                                          syntax.elem<OpExpr>(
                                              Position{23, 53}, OpExpr::MINUS,
                                              syntax.elem<OpExpr>(
                                                  Position{23, 53}, OpExpr::PLUS,
                                                  syntax.elem<SimpleVar>(Position{23, 53}, "r"),
                                                  syntax.elem<IntExpr>(Position{23, 55}, 7)),
                                              syntax.elem<SimpleVar>(Position{23, 57}, "c"))),
                                      syntax.elem<IntExpr>(Position{23, 60}, 0)),
                                  syntax.elem<IntExpr>(Position{}, 0)),
                              syntax.elem<SeqExpr>(
                                  Position{24, 23},
                                  syntax.elem<ExprList>(
                                      Position{24, 24}, vector<Expr*>{
                                          syntax.elem<AssignExpr>(
                                              Position{24, 24},
                                              syntax.elem<SubscriptVar>(
                                                  Position{24, 24},
                                                  syntax.elem<SimpleVar>(Position{24, 24}, "row"),
                                                  syntax.elem<SimpleVar>(Position{24, 28}, "r")),
                                              syntax.elem<IntExpr>(Position{24, 32}, 1)),
                                          syntax.elem<AssignExpr>(
                                              Position{24, 35},
                                              syntax.elem<SubscriptVar>(
                                                  Position{24, 35},
                                                  syntax.elem<SimpleVar>(Position{24, 35}, "diag1"),
                                                  syntax.elem<OpExpr>(
                                                      Position{24, 41}, OpExpr::PLUS,
                                                      syntax.elem<SimpleVar>(Position{24, 41}, "r"),
                                                      syntax.elem<SimpleVar>(Position{24, 43}, "c"))),
                                              syntax.elem<IntExpr>(Position{24, 47}, 1)),
                                          syntax.elem<AssignExpr>(
                                              Position{24, 50},
                                              syntax.elem<SubscriptVar>(
                                                  Position{24, 50},
                                                  syntax.elem<SimpleVar>(Position{24, 50}, "diag2"),
                                                  syntax.elem<OpExpr>(
                                                      Position{24, 56}, OpExpr::MINUS,
                                                      syntax.elem<OpExpr>(
                                                          Position{24, 56}, OpExpr::PLUS,
                                                          syntax.elem<SimpleVar>(Position{24, 56}, "r"),
                                                          syntax.elem<IntExpr>(Position{24, 58}, 7)),
                                                      syntax.elem<SimpleVar>(Position{24, 60}, "c"))),
                                              syntax.elem<IntExpr>(Position{24, 64}, 1)),
                                          syntax.elem<AssignExpr>(
                                              Position{25, 24},
                                              syntax.elem<SubscriptVar>(
                                                  Position{25, 24},
                                                  syntax.elem<SimpleVar>(Position{25, 24}, "col"),
                                                  syntax.elem<SimpleVar>(Position{25, 28}, "c")),
                                              syntax.elem<SimpleVar>(Position{25, 32}, "r")),
                                          syntax.elem<CallExpr>(
                                              Position{26, 24}, "try", vector<Expr*>{
                                                  syntax.elem<OpExpr>(
                                                      Position{26, 28}, OpExpr::PLUS,
                                                      syntax.elem<SimpleVar>(Position{26, 28}, "c"),
                                                      syntax.elem<IntExpr>(Position{26, 30}, 1)),
                                              }),
                                          syntax.elem<AssignExpr>(
                                              Position{27, 24},
                                              syntax.elem<SubscriptVar>(
                                                  Position{27, 24},
                                                  syntax.elem<SimpleVar>(Position{27, 24}, "row"),
                                                  syntax.elem<SimpleVar>(Position{27, 28}, "r")),
                                              syntax.elem<IntExpr>(Position{27, 32}, 0)),
                                          syntax.elem<AssignExpr>(
                                              Position{27, 35},
                                              syntax.elem<SubscriptVar>(
                                                  Position{27, 35},
                                                  syntax.elem<SimpleVar>(Position{27, 35}, "diag1"),
                                                  syntax.elem<OpExpr>(
                                                      Position{27, 41}, OpExpr::PLUS,
                                                      syntax.elem<SimpleVar>(Position{27, 41}, "r"),
                                                      syntax.elem<SimpleVar>(Position{27, 43}, "c"))),
                                              syntax.elem<IntExpr>(Position{27, 47}, 0)),
                                          syntax.elem<AssignExpr>(
                                              Position{27, 50},
                                              syntax.elem<SubscriptVar>(
                                                  Position{27, 50},
                                                  syntax.elem<SimpleVar>(Position{27, 50}, "diag2"),
                                                  syntax.elem<OpExpr>(
                                                      Position{27, 56}, OpExpr::MINUS,
                                                      syntax.elem<OpExpr>(
                                                          Position{27, 56}, OpExpr::PLUS,
                                                          syntax.elem<SimpleVar>(Position{27, 56}, "r"),
                                                          syntax.elem<IntExpr>(Position{27, 58}, 7)),
                                                      syntax.elem<SimpleVar>(Position{27, 60}, "c"))),
                                              syntax.elem<IntExpr>(Position{27, 64}, 0)),
                                      })),
                              nullptr)))),
          }),
      syntax.elem<ExprList>(
          Position{28, 10}, vector<Expr*>{
            syntax.elem<CallExpr>(
                Position{28, 10}, "try", vector<Expr*>{
                    syntax.elem<IntExpr>(Position{28, 14}, 0),
                }),
          }));
  EXPECT_EQ(*program, *expectedProgram);
}
