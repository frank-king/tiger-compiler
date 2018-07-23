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
class Token {
public:
  enum Type {
    ID, STRING, INT, COMMA, COLON, SEMICOLON,
    LPAREN, RPAREN, LBRACK, RBRACK, LBRACE, RBRACE,
    DOT, PLUS, MINUS, TIMES, DIVIDE, EQ, NEQ,
    LT, LE, GT, GE, AND, OR, ASSIGN,
    ARRAY, IF, THEN, ELSE, WHILE, FOR, TO, DO, LET,
    IN, END, OF, BREAK, NIL, FUNCTION, VAR, TYPE,
    // Special tokens used for the parser
    EOF_TOK, EMPTY
  };

  Token(Type type) noexcept : type_(type) {
    switch (type_) {
    case ID: case STRING: new (&strValue_) string(); break;
    case INT: intValue_ = 0; break;
    default: break;
    }
  }
  Token(const Token& other) : type_(other.type_) {
    switch (type_) {
    case ID: case STRING: new (&strValue_) string(other.strValue_); break;
    case INT: intValue_ = other.intValue_; break;
    default: break;
    }
  }
  Token(Token&& other) : type_(other.type_) {
    switch (type_) {
    case ID: case STRING: new (&strValue_) string(std::move(other.strValue_)); break;
    case INT: intValue_ = other.intValue_; break;
    default: break;
    }
  }
  ~Token() noexcept {
    switch (type_) {
    case ID: case STRING: strValue_.~string(); break;
    default: break;
    }
  }

  static Token INT_(int value) noexcept { return Token(INT, value); }
  static Token ID_(string value) { return Token(ID, std::move(value)); }
  static Token ID_(const char_t *value) { return Token(ID, value); }
  static Token STRING_(string value) { return Token(STRING, std::move(value)); }
  static Token STRING_(const char_t *value) { return Token(STRING, value); }

  constexpr const char_t *name() const noexcept { return NAMES[type_]; }
  Type type() const noexcept { return type_; }
  bool is(Type type) const noexcept { return type_ == type; }
  bool operator==(const Token &rhs) const noexcept {
    if (type_ != rhs.type_)
      return false;
    switch (type_) {
    case ID: case STRING: return strValue_ == rhs.strValue_;
    case INT: return intValue_ == rhs.intValue_;
    default: return true;
    }
  }

  friend std::ostream& operator<<(std::ostream& os, const Token& token) {
    os << token.name();
    switch (token.type_) {
    case ID: os << "(" << token.strValue_ << ")"; break;
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

  static constexpr std::pair<const char_t*, Token::Type> KEYWORDS[] = {
      {"array", ARRAY}, {"if", IF}, {"then", THEN}, {"else", ELSE}, {"while", WHILE},
      {"for", FOR}, {"to", TO}, {"do", DO}, {"let", LET}, {"in", IN}, {"end", END},
      {"of", OF}, {"break", BREAK}, {"nil", NIL}, {"function", FUNCTION}, {"var", VAR},
      {"type", TYPE},
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
  explicit constexpr Token(Type type, int value) noexcept : type_(type), intValue_(value) {}
  explicit Token(Type type, string value) : type_(type), strValue_(std::move(value)) {}
  explicit Token(Type type, const char_t *value) : type_(type), strValue_(value) {}

  Type type_;
  union {
    int intValue_;
    string strValue_;
  };
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
  // using std::istream::pos_type;
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

  // pos_type tok_beg_ = 0, tok_end_ = 0, cur_ = 0;
  mutable long line_ = 1, col_ = 1;

};
} // namespace tiger::lex

#endif //TIGER_COMPILER_LEXER_H
