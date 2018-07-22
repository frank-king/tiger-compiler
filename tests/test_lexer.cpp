//
// Created by whjpji on 18-7-22.
//

#include "gtest/gtest.h"
#include "lexer.h"

TEST(lexer_test, lexer_test) {
  using namespace tiger::lex;
  string program = R"(
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
  )";
  Lexer lexer(program);
  std::vector<Token> tokens;
  while (!lexer.eof())
    tokens.push_back(lexer.nextToken());
  std::vector<Token> expected_tokens = {
      Token::LET,
      Token::VAR, Token::ID_("N"), Token::ASSIGN, Token::INT_(8),
      Token::TYPE, Token::ID_("intArray"), Token::EQ, Token::ARRAY, Token::OF, Token::ID_("int"),

      Token::VAR, Token::ID_("row"), Token::ASSIGN, Token::ID_("intArray"),
      Token::LBRACK, Token::ID_("N"), Token::RBRACK, Token::OF, Token::INT_(0),

      Token::VAR, Token::ID_("col"), Token::ASSIGN, Token::ID_("intArray"),
      Token::LBRACK, Token::ID_("N"), Token::RBRACK, Token::OF, Token::INT_(0),

      Token::VAR, Token::ID_("diag1"), Token::ASSIGN, Token::ID_("intArray"), Token::LBRACK,
      Token::ID_("N"), Token::PLUS, Token::ID_("N"), Token::MINUS, Token::INT_(1),
      Token::RBRACK, Token::OF, Token::INT_(0),

      Token::VAR, Token::ID_("diag2"), Token::ASSIGN, Token::ID_("intArray"), Token::LBRACK,
      Token::ID_("N"), Token::PLUS, Token::ID_("N"), Token::MINUS, Token::INT_(1),
      Token::RBRACK, Token::OF, Token::INT_(0),

      Token::FUNCTION, Token::ID_("printboard"), Token::LPAREN, Token::RPAREN, Token::EQ,
      Token::LPAREN,
        Token::FOR, Token::ID_("i"), Token::ASSIGN, Token::INT_(0), Token::TO, Token::ID_("N"), Token::MINUS, Token::INT_(1),
        Token::DO, Token::LPAREN,
          Token::FOR, Token::ID_("j"), Token::ASSIGN, Token::INT_(0), Token::TO, Token::ID_("N"), Token::MINUS, Token::INT_(1),
            Token::DO, Token::ID_("print"), Token::LPAREN, Token::IF,
              Token::ID_("col"), Token::LBRACK, Token::ID_("i"), Token::RBRACK, Token::EQ, Token::ID_("j"),
            Token::THEN, Token::STRING_(" O"), Token::ELSE, Token::STRING_(" ."), Token::RPAREN, Token::SEMICOLON,
          Token::ID_("print"), Token::LPAREN, Token::STRING_("\n"), Token::RPAREN, Token::RPAREN, Token::SEMICOLON,
        Token::ID_("print"), Token::LPAREN, Token::STRING_("\n"), Token::RPAREN, Token::RPAREN,

      Token::FUNCTION, Token::ID_("try"), Token::LPAREN, Token::ID_("c"), Token::COLON, Token::ID_("int"), Token::RPAREN, Token::EQ,
        Token::IF, Token::ID_("c"), Token::EQ, Token::ID_("N"),
        Token::THEN, Token::ID_("printboard"), Token::LPAREN, Token::RPAREN,
        Token::ELSE, Token::FOR, Token::ID_("r"), Token::ASSIGN, Token::INT_(0), Token::TO, Token::ID_("N"), Token::MINUS, Token::INT_(1),
          Token::DO, Token::IF, Token::ID_("row"), Token::LBRACK, Token::ID_("r"), Token::RBRACK, Token::EQ, Token::INT_(0),
              Token::AND, Token::ID_("diag1"), Token::LBRACK, Token::ID_("r"), Token::PLUS, Token::ID_("c"), Token::RBRACK, Token::EQ, Token::INT_(0),
              Token::AND, Token::ID_("diag2"), Token::LBRACK, Token::ID_("r"), Token::PLUS, Token::INT_(7), Token::MINUS, Token::ID_("c"), Token::RBRACK, Token::EQ, Token::INT_(0),
            Token::THEN, Token::LPAREN,
              Token::ID_("row"), Token::LBRACK, Token::ID_("r"), Token::RBRACK, Token::ASSIGN, Token::INT_(1), Token::SEMICOLON,
              Token::ID_("diag1"), Token::LBRACK, Token::ID_("r"), Token::PLUS, Token::ID_("c"), Token::RBRACK, Token::ASSIGN, Token::INT_(1), Token::SEMICOLON,
              Token::ID_("diag2"), Token::LBRACK, Token::ID_("r"), Token::PLUS, Token::INT_(7), Token::MINUS, Token::ID_("c"), Token::RBRACK, Token::ASSIGN, Token::INT_(1), Token::SEMICOLON,
              Token::ID_("col"), Token::LBRACK, Token::ID_("c"), Token::RBRACK, Token::ASSIGN, Token::ID_("r"), Token::SEMICOLON,
              Token::ID_("try"), Token::LPAREN, Token::ID_("c"), Token::PLUS, Token::INT_(1), Token::RPAREN, Token::SEMICOLON,
              Token::ID_("row"), Token::LBRACK, Token::ID_("r"), Token::RBRACK, Token::ASSIGN, Token::INT_(0), Token::SEMICOLON,
              Token::ID_("diag1"), Token::LBRACK, Token::ID_("r"), Token::PLUS, Token::ID_("c"), Token::RBRACK, Token::ASSIGN, Token::INT_(0), Token::SEMICOLON,
              Token::ID_("diag2"), Token::LBRACK, Token::ID_("r"), Token::PLUS, Token::INT_(7), Token::MINUS, Token::ID_("c"), Token::RBRACK, Token::ASSIGN, Token::INT_(0), Token::RPAREN,

      Token::IN, Token::ID_("try"), Token::LPAREN, Token::INT_(1), Token::RPAREN,
      Token::END
  };
  EXPECT_EQ(tokens.size(), expected_tokens.size());
  for (auto t1 = tokens.cbegin(), t2 = expected_tokens.cbegin();
      t1 != tokens.cend() && t2 != expected_tokens.cend(); ++t1, ++t2)
    EXPECT_EQ(*t1, *t2);
}
