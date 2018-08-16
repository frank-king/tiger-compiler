//
// Created by whjpji on 18-7-22.
//

#ifndef TIGER_COMPILER_LEXER_H
#define TIGER_COMPILER_LEXER_H

#include <fstream>
#include <istream>
#include <string>
#include <memory>
#include <filesystem>
#include <ostream>
#include <utility>
namespace fs = std::filesystem;
using std::string;
using std::unique_ptr;
using char_t = char;

namespace tiger::lex {

struct Position { long line, col; };

class Token {
public:
  enum Kind {
    ID, STRING, INT, COMMA, COLON, SEMICOLON,
    LPAREN, RPAREN, LBRACK, RBRACK, LBRACE, RBRACE,
    DOT, PLUS, MINUS, TIMES, DIVIDE, EQ, NEQ,
    LT, LE, GT, GE, AND, OR, ASSIGN,
    ARRAY, IF, THEN, ELSE, WHILE, FOR, TO, DO, LET,
    IN, END, OF, BREAK, NIL, FUNCTION, VAR, TYPE,
    // Special tokens used for the parser
    EOF_TOK, EMPTY
  };

  Token(Kind kind) noexcept : kind_(kind) {
    switch (kind_) {
    case ID: case STRING: new (&strValue_) string(); break;
    case INT: intValue_ = 0; break;
    default: break;
    }
  }
  Token(const Token& other) { *this = other; }
  Token(Token&& other) noexcept { *this = std::move(other); }
  Token(const Token& other, const Position& pos) {
    *this = other;
    pos_ = pos;
  }
  Token(Token&& other, Position pos) noexcept {
    *this = std::move(other);
    pos_ = pos;
  }
  ~Token() noexcept {
    switch (kind_) {
    case ID: case STRING: strValue_.~string(); break;
    default: break;
    }
  }

  Token& operator=(const Token& other) {
    switch (kind_ = other.kind_) {
    case ID: case STRING: new (&strValue_) string(other.strValue_); break;
    case INT: intValue_ = other.intValue_; break;
    default: break;
    }
    return *this;
  }
  Token& operator=(Token&& other) noexcept {
    switch (kind_ = other.kind_) {
    case ID: case STRING: new (&strValue_) string(std::move(other.strValue_)); break;
    case INT: intValue_ = other.intValue_; break;
    default: break;
    }
    return *this;
  }

  const Position& position() const noexcept { return pos_; }

  constexpr int intValue() const noexcept { return intValue_; }
  const string& strValue() const noexcept { return strValue_; }

  static Token INT_(int value) noexcept { return Token(INT, value); }
  static Token ID_(string value) { return Token(ID, std::move(value)); }
  static Token ID_(const char_t *value) { return Token(ID, value); }
  static Token STRING_(string value) { return Token(STRING, std::move(value)); }
  static Token STRING_(const char_t *value) { return Token(STRING, value); }

  constexpr const char_t *name() const noexcept { return NAMES[kind_]; }
  Kind kind() const noexcept { return kind_; }
  bool is(Kind kind) const noexcept { return kind_ == kind; }
  bool operator==(const Token &rhs) const noexcept {
    if (kind_ != rhs.kind_)
      return false;
    switch (kind_) {
    case ID: case STRING: return strValue_ == rhs.strValue_;
    case INT: return intValue_ == rhs.intValue_;
    default: return true;
    }
  }

  friend std::ostream& operator<<(std::ostream& os, const Token& token) {
    os << token.name();
    switch (token.kind_) {
    case ID: os << "(\"" << token.strValue_ << "\")"; break;
    case INT: os << "(" << token.intValue_ << ")"; break;
    case STRING:
      os << "(\"";
      for (auto ch : token.strValue_) {
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
      os << "\")";
      break;
    default: break;
    }
    return os;
  }

  static constexpr std::pair<const char_t*, Token::Kind> KEYWORDS[] = {
      {"array", ARRAY}, {"if", IF}, {"then", THEN}, {"else", ELSE}, {"while", WHILE},
      {"for", FOR}, {"to", TO}, {"do", DO}, {"let", LET}, {"in", IN}, {"end", END},
      {"of", OF}, {"break", BREAK}, {"nil", NIL}, {"function", FUNCTION}, {"var", VAR},
      {"kind", TYPE},
  };

protected:
  static constexpr const char_t *NAMES[] = {
      "ID", "STRING", "INT", "COMMA", "COLON", "SEMICOLON", "LPAREN",
      "RPAREN", "LBRACK", "RBRACK", "LBRACE", "RBRACE", "DOT", "PLUS",
      "MINUS", "TIMES", "DIVIDE", "EQ", "NEQ", "LT", "LE", "GT", "GE",
      "AND", "OR", "ASSIGN", "ARRAY", "IF", "THEN", "ELSE", "WHILE", "FOR",
      "TO", "DO", "LET", "IN", "END", "OF", "BREAK", "NIL", "FUNCTION",
      "VAR", "TYPE",
      // Special tokens used for the parser
      "EOF", "EMPTY",
  };
  explicit constexpr Token(Kind kind, int value) noexcept
      : kind_(kind), intValue_(value), pos_() {}
  explicit Token(Kind kind, string value) : kind_(kind), strValue_(std::move(value)) {}
  explicit Token(Kind kind, const char_t *value) : kind_(kind), strValue_(value) {}

  Kind kind_;
  union {
    int intValue_;
    string strValue_;
  };
  Position pos_;
};

class Lexer {
public:
  explicit Lexer(string str)
      : input_(std::make_unique<std::istringstream>(std::move(str))) {}
  explicit Lexer(fs::path path)
      : input_(std::make_unique<std::ifstream>(std::move(path))) {}

  bool eof() noexcept;
  Token nextToken();

protected:
  unique_ptr<std::istream> input_;
  void eatNonTokens();
  int get() noexcept;
  int peek() const noexcept;
  void unget() noexcept;
  void eatIfNewline(int &cur);
  void error();
  Token processInt();
  Token processKeywordOrId();
  Token processString();
  Token processSymbol();

  mutable Position cur_;

};
} // namespace tiger::lex

#endif //TIGER_COMPILER_LEXER_H
