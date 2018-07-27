cmake_minimum_required(VERSION 3.10)
project(tiger_compiler)

set(CMAKE_CXX_STANDARD 17)

set(${CMAKE_CXX_FLAGS} -Wall)

find_package(Boost 1.65.1 COMPONENTS log log_setup thread date_time filesystem system REQUIRED)

add_subdirectory(src)
add_subdirectory(tests)
