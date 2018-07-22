include_directories(${PROJECT_SOURCE_DIR}/src)

find_package(GTest REQUIRED)
include_directories(${GTEST_INCLUDE_DIRS})

find_package(Threads REQUIRED)

enable_testing()
set(TEST_SOURCES
        test_slp.cpp
        test_lexer.cpp
        )
add_executable(tiger_tests ${TEST_SOURCES} $<TARGET_OBJECTS:libtiger>)
target_link_libraries(tiger_tests ${GTEST_BOTH_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
