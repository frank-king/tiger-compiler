//
// Created by whjpji on 18-7-22.
//

#include "lexer.h"

namespace tiger::lex {
bool Lexer::eof() noexcept { return input_->eof(); }
Token Lexer::nextToken() {
  // First, process the non-tokens, spaces or comments.
  eatNonTokens();
  if (eof())
    return Token::EOF_TOK;

  // tok_beg_ = input_->tellg();
  if (auto ch = peek(); isalpha(ch)) {
    // Process the keywords or identifiers.
    return processKeywordOrId();
  } else if (isdigit(ch)) {
    // Process the integer literals.
    return processInt();
  } else if (ch == '\"') {
    // Process the string literals;
    return processString();
  } else {
    // Process the symbols.
    return processSymbol();
  }
}
int Lexer::get() noexcept { ++col_; return input_->get(); }
int Lexer::peek() const noexcept { return input_->peek(); }
void Lexer::unget() noexcept { --col_; input_->unget(); }

void Lexer::eatNonTokens() {
  while (!input_->eof()) {
    if (auto ch = get(); isspace(ch)) {
      // Process the spaces.
      do {
        eatIfNewline(ch);
        ch = get();
        if (input_->eof())
          return;
      } while (isspace(ch));
      unget();
    } else if (ch == '/' && peek() == '*') {
      // Process the comments.
      get();
      for (int count = 1; count > 0;) {
        eatIfNewline(ch);
        ch = get();
        if (input_->eof())
          error();
        if (ch == '/' && peek() == '*') { // Nested comment
          ++count;
          get();
        } else if (ch == '*' && peek() == '/') { // Comment ends
          --count;
          get();
        }
      }
    } else {
      if (!eof())
        unget();
      break;
    }
  }
}
void Lexer::eatIfNewline(int &cur) {
  if (cur == '\n' || cur == '\r') {
    if (auto ch = peek(); ch != cur && (ch == '\n' || ch == '\r'))
      get();
    cur = peek();
    ++line_;
    col_ = 1;
  }
}

Token Lexer::processInt() {
  int value = 0;
  auto ch = get();
  do {
    value = value * 10 + ch - '0';
    ch = get();
  } while (isdigit(ch));
  if (!eof())
    unget();
  return Token::INT_(value);
}

Token Lexer::processKeywordOrId() {
  std::ostringstream buf;
  auto ch = get();
  do {
    buf << static_cast<char_t>(ch);
    ch = get();
  } while (isalnum(ch) || ch == '_');
  if (!eof())
    unget();
  string&& value = buf.str();
  for (const auto& [name, type] : Token::KEYWORDS)
    if (value == name)
      return Token(type);
  return Token::ID_(std::move(value));
}

Token Lexer::processString() {
  std::ostringstream buf;
  auto ch = get();
  while (true) {
    if (ch = get(); ch == '\"')
      // End of string.
      break;
    else if (ch == '\\') {
      // Escape sequences.
      if (ch = get(); isspace(ch)) {
        // Ignored sequence.
        do {
          eatIfNewline(ch);
          if (ch == '\\')
            break;
          else if (!isspace(ch))
            error();
          ch = get();
        } while (isspace(ch));
      } else if (ch >= '0' && ch < '4') {
        // Octal ASCII code (0-255 in decimal, 0-377 in octal).
        int ord = ch - '0';
        for (int i = 0; i < 2; ++i) {
          ch = get();
          if (ch < '0' || ch >= '8')
            error();
          ord = ord * 8 + ch - '0';
        }
        buf << static_cast<char_t>(ord);
      } else if (ch == 'x') {
        // Hexadecimal ASCII code.
        int ord = 0;
        for (int i = 0; i < 2; ++i) {
          ch = get();
          ord *= 16;
          if (ch >= '0' && ch <= '9') {
            ord += ch - '0';
          } else if (ch >= 'A' && ch <= 'F') {
            ord += ch - 'A' + 10;
          } else if (ch >= 'a' && ch <= 'f') {
            ord += ch - 'a' + 10;
          } else {
            error();
          }
        }
        buf << static_cast<char_t>(ord);
      } /*else if (ch == '^') {
        // Control characters.
        switch (get()) {
        case '@': buf << '\0'; break; // null
        case 'G': buf << '\a'; break; // bell
        case 'H': buf << '\b'; break; // backspace
        case 'I': buf << '\t'; break; // horizontal
        case 'J': buf << '\n'; break; // line feed
        case 'K': buf << '\v'; break; // vertical tab
        case 'L': buf << '\f'; break; // form feed
        case 'M': buf << '\r'; break; // carriage return
        // case 'Z': buf << EOF; break; // EOF
        default: // TODO: error in string literal
          break;
        }
      } else if (ch == 'n') { buf << '\n'; }
      else if (ch == 't') { buf << '\t'; }
      else if (ch == 't') { buf << '\t'; }
      else if (ch == '\"') { buf << '\"'; }
      else if (ch == '\\') { buf << '\\'; }
      */
      else {
        switch (ch) {
        case 'a': buf << '\a'; break;
        case 'b': buf << '\b'; break;
        case 'f': buf << '\f'; break;
        case 'n': buf << '\n'; break;
        case 'r': buf << '\r'; break;
        case 't': buf << '\t'; break;
        case 'v': buf << '\v'; break;
        case '\"': buf << '\"'; break;
        case '\\': buf << '\\'; break;
        default:
          error();
        }
      }
    } else {
      // Literal characters.
      buf << static_cast<char_t>(ch);
    }
  }
  /*
  if (string&& value = buf.str(); value.empty())
    return Token::STRING_("");
  else
    return Token::STRING_(std::move(value));
  */
  return Token::STRING_(buf.str());
}
Token Lexer::processSymbol() {
  auto ch = get();
  switch (ch) {
  case ',': return Token::COMMA;
  case ';': return Token::SEMICOLON;
  case '(': return Token::LPAREN;
  case ')': return Token::RPAREN;
  case '[': return Token::LBRACK;
  case ']': return Token::RBRACK;
  case '{': return Token::LBRACE;
  case '}': return Token::RBRACE;
  case '.': return Token::DOT;
  case '+': return Token::PLUS;
  case '-': return Token::MINUS;
  case '*': return Token::TIMES;
  case '/': return Token::DIVIDE;
  case '=': return Token::EQ;
  case '&': return Token::AND;
  case '|': return Token::OR;
  case ':':
    switch (get()) {
    case '=': return Token::ASSIGN;
    default: unget(); return Token::COLON;
    }
  case '<':
    switch (get()) {
    case '>': return Token::NEQ;
    case '=': return Token::LE;
    default: unget(); return Token::LT;
    }
  case '>':
    switch (get()) {
    case '=': return Token::GE;
    default: unget(); return Token::GT;
    }
  default:
    // TODO: error symbol
    break;
  }
}
void Lexer::error() {
  // TODO: process lexical error.
}

} // namespace tiger::lex
