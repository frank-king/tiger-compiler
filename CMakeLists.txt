cmake_minimum_required(VERSION 3.10)
project(tiger_compiler)

set(CMAKE_CXX_STANDARD 17)

set(${CMAKE_CXX_FLAGS} -Wall)

add_subdirectory(src)
add_subdirectory(tests)
