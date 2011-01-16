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

set(CPP2SIR_HEADERS
	ast_pretty_printer.hpp
	cfg.hpp
	cfg_builder.hpp
	cfg_json_writer.hpp
	reg.hpp
	unit_navigator.hpp
	util.hpp
	)

set(CPP2SIR_SOURCES
	ast_pretty_printer.cpp
	cfg.cpp
	cfg_builder.cpp
	cfg_json_writer.cpp
	main.cpp
	unit_navigator.cpp
	util.cpp
	)

add_subdirectory(json)

add_executable(cpp2sir ${CPP2SIR_SOURCES} ${CPP2SIR_HEADERS})
if (MSVC)
    add_definitions("/wd4146 /wd4800 /wd4355 /wd4345 /wd4244 /wd4624 /wd4291")
endif (MSVC)
target_link_libraries(cpp2sir libjson ${CLANG_LIBS})
set_property(
    TARGET cpp2sir
    PROPERTY COMPILE_DEFINITIONS __STDC_LIMIT_MACROS __STDC_CONSTANT_MACROS _SCL_SECURE_NO_WARNINGS
    )
