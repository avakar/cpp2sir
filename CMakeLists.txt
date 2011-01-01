cmake_minimum_required(VERSION 2.6)
project(cpp2sir)

set(LLVM_INCLUDE_DIR "" CACHE PATH "The location of LLVM and clang include files.")
set(LLVM_LIB_DIR "" CACHE PATH "The location of LLVM and clang library files.")

include_directories(${LLVM_INCLUDE_DIR})
link_directories(${LLVM_LIB_DIR})

set(CLANG_LIBS
    LLVMCore
    LLVMSupport
    LLVMMC

    clangAST
    clangAnalysis
    clangBasic
    clangDriver
    clangFrontend
    clangIndex
    clangLex
    clangParse
    clangSema
    clangSerialization
    )

add_executable(cpp2sir ast_pretty_printer.cpp main.cpp cfg.cpp
    cfg_builder.cpp cfg_json_writer.cpp unit_navigator.cpp util.cpp json/json_reader.cpp
    mangle.cpp
    json/json_value.cpp json/json_writer.cpp)
target_link_libraries(cpp2sir ${CLANG_LIBS})
set_property(
    TARGET cpp2sir
    PROPERTY COMPILE_DEFINITIONS __STDC_LIMIT_MACROS __STDC_CONSTANT_MACROS _SCL_SECURE_NO_WARNINGS
    )
