add_library(libslp OBJECT slp.cpp)
add_executable(tiger_compiler main.cpp $<TARGET_OBJECTS:libslp>)
