#ifndef CPPPARSER_UTIL_HPP
#define CPPPARSER_UTIL_HPP

#include <clang/AST/Decl.h>
#include <string>
#include <map>

class name_mangler
{
public:
	explicit name_mangler(clang::ASTContext & ctx)
		: m_ctx(ctx)
	{
	}

	std::string make_decl_name(clang::NamedDecl const * decl, std::string const & static_prefix = "__unique");
	std::string make_rtti_name(clang::QualType type, std::string const & static_prefix = "__unique");

	typedef std::map<std::string, std::string> alias_map_t;
	alias_map_t const & aliases() const { return m_aliases; }

private:
	clang::ASTContext & m_ctx;
	alias_map_t m_aliases;
};

#endif
