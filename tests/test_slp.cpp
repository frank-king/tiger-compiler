//
// Created by whjpji on 18-7-21.
//

#include "gtest/gtest.h"
#include "slp.h"

TEST(slp_test, stmt_test) {
  using namespace tiger::slp;
  using std::make_unique;
  unique_ptr<Stmt> program(make_unique<CompoundStmt>(
      make_unique<AssignStmt>("a", make_unique<OpExpr>(
          make_unique<NumExpr>(5),
          make_unique<BinOp>(BinOp::OpType::kAdd),
          make_unique<NumExpr>(3))),
      make_unique<CompoundStmt>(
          make_unique<AssignStmt>("b", make_unique<EseqExpr>(
              make_unique<PrintStmt>(make_unique<PairExprList>(
                  make_unique<IdExpr>("a"),
                  make_unique<LastExprList>(make_unique<OpExpr>(
                      make_unique<IdExpr>("a"),
                      make_unique<BinOp>(BinOp::OpType::kSub),
                      make_unique<NumExpr>(1))))),
              make_unique<OpExpr>(make_unique<NumExpr>(10),
                                  make_unique<BinOp>(BinOp::OpType::kMul),
                                  make_unique<IdExpr>("a")))),
          make_unique<PrintStmt>(make_unique<LastExprList>(make_unique<IdExpr>("b")))))
  );
  std::stringstream ss;
  ss << *program;
  std::string program_str = std::move(ss.str());
  const char *expected_program_str =
      "a := 5 + 3;\n"
      "b := (print(a, a - 1), 10 * a);\n"
      "print(b)";
  EXPECT_EQ(program_str, expected_program_str);
}

