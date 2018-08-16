//
// Created by whjpji on 18-7-21.
//

#ifndef TIGER_COMPILER_UTIL_H
#define TIGER_COMPILER_UTIL_H

#include <iostream>
#include <memory>

namespace tiger {
using std::unique_ptr;

template <typename Container, typename Delimiter>
void printWithDelimiter(std::ostream& os,
                        const Container& container,
                        Delimiter&& delimiter) {
  auto iter = std::cbegin(container), cend = std::cend(container);
  while (iter != cend && std::next(iter) != cend) {
    os << *iter++ << delimiter;
  }
  if (iter != cend) {
    os << *iter++;
  }
}

template <typename Container, typename Delimiter, typename DoPrintFunc>
void printWithDelimiter(std::ostream& os,
                        const Container& container,
                        Delimiter&& delimiter,
                        DoPrintFunc&& doPrintFunc) {
  auto iter = std::cbegin(container), cend = std::cend(container);
  while (iter != cend && std::next(iter) != cend) {
    doPrintFunc(*iter++);
    os << delimiter;
  }
  if (iter != cend) {
    doPrintFunc(*iter++);
  }
}

template <typename U, typename T>
unique_ptr<U> dynamic_unique_cast(unique_ptr<T>&& base) {
  if (!base)
    return nullptr;
  else {
    auto derived = dynamic_cast<U *>(base.release());
    return unique_ptr<U>(derived);
  }
};


}; // namespace tiger

#endif //TIGER_COMPILER_UTIL_H
