add_library(libtiger OBJECT slp.cpp lexer.cpp)
add_executable(tiger_compiler main.cpp $<TARGET_OBJECTS:libtiger>)
