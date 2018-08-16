set(CMAKE_CXX_FLAGS -DBOOST_LOG_DYN_LINK)
add_library(libtiger OBJECT syntax.cpp lexer.cpp parser.cpp)
add_executable(tiger_compiler main.cpp $<TARGET_OBJECTS:libtiger> ${Boost_LIBRARIES})
