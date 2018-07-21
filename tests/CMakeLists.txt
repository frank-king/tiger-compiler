include_directories(${PROJECT_SOURCE_DIR}/src)

find_package(GTest REQUIRED)
include_directories(${GTEST_INCLUDE_DIRS})

find_package(Threads REQUIRED)

enable_testing()
add_executable(slp_tests test_slp.cpp $<TARGET_OBJECTS:libslp>)
target_link_libraries(slp_tests ${GTEST_BOTH_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
