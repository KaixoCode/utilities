cmake_minimum_required (VERSION 3.0)
project (utilities)

set (CMAKE_CXX_STANDARD 20)

include_directories(
  /
)

add_library(utilities STATIC
  "${utilities_SOURCE_DIR}/utils.hpp"
  "${utilities_SOURCE_DIR}/smart_tuple.hpp"
  "${utilities_SOURCE_DIR}/pa_function.hpp"
  "${utilities_SOURCE_DIR}/function.hpp" 
  "${utilities_SOURCE_DIR}/lambda.hpp"
  "${utilities_SOURCE_DIR}/struct_tuple.hpp"
)

set_target_properties(utilities PROPERTIES LINKER_LANGUAGE CXX)


add_executable(utilities_example
  "${utilities_SOURCE_DIR}/example.cpp"
)
