cmake_minimum_required(VERSION 2.6)
project(libjson)

set(JSON_SOURCES
	json_value.cpp
	json_reader.cpp
	json_writer.cpp
	)

add_library(libjson STATIC ${JSON_SOURCES})
