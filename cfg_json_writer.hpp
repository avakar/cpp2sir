#ifndef CPPPARSER_CFG_JSON_WRITER_HPP
#define CPPPARSER_CFG_JSON_WRITER_HPP

#include "cfg.hpp"
#include "util.hpp"
#include <iostream>

void cfg_json_write(std::ostream & out, program const & prog, name_mangler const & nm, bool readable);

#endif
