cmake_minimum_required (VERSION 3.0)
project (utilities)

set (CMAKE_CXX_STANDARD 23)

include_directories(
  "${utilities_SOURCE_DIR}/libs"
)

add_library(utilities STATIC
  "${utilities_SOURCE_DIR}/utils.hpp"
  "${utilities_SOURCE_DIR}/smart_tuple.hpp"
  "${utilities_SOURCE_DIR}/pa_function.hpp"
  "${utilities_SOURCE_DIR}/function.hpp" 
  "${utilities_SOURCE_DIR}/lambda.hpp"
  "${utilities_SOURCE_DIR}/struct_tuple.hpp"
  "${utilities_SOURCE_DIR}/type_linker.hpp"
  "${utilities_SOURCE_DIR}/constexpr_counter.hpp"
  "${utilities_SOURCE_DIR}/dynamic_array.hpp"
  "${utilities_SOURCE_DIR}/image_buffer.hpp"
  "${utilities_SOURCE_DIR}/vec.hpp"
  "${utilities_SOURCE_DIR}/axial_array.hpp"
  "${utilities_SOURCE_DIR}/list_comprehension.hpp"
  "${utilities_SOURCE_DIR}/trash.hpp"
  "${utilities_SOURCE_DIR}/Parser.hpp"
  "${utilities_SOURCE_DIR}/meta_struct.hpp"
  "${utilities_SOURCE_DIR}/typed_any.hpp"
  "${utilities_SOURCE_DIR}/constexpr_parser.hpp"
  "${utilities_SOURCE_DIR}/instances.hpp"
  "${utilities_SOURCE_DIR}/pack_utils.hpp"
  "${utilities_SOURCE_DIR}/string_literal.hpp"
  "${utilities_SOURCE_DIR}/callable.hpp"
  "${utilities_SOURCE_DIR}/delegate.hpp"
  "${utilities_SOURCE_DIR}/match.hpp"
  "${utilities_SOURCE_DIR}/function_utils.hpp"
)

set_target_properties(utilities PROPERTIES LINKER_LANGUAGE CXX)


add_executable(utilities_example
  "${utilities_SOURCE_DIR}/example.cpp"
)

target_link_libraries(utilities_example
  comctl32
  propsys
  dwmapi
  gdiplus
  user32
)