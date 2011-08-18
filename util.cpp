#include "util.hpp"
#include <clang/AST/DeclCXX.h>
#include <clang/AST/DeclTemplate.h>
#include <boost/assert.hpp>
#include <boost/lexical_cast.hpp>

namespace {

clang::LinkageSpecDecl::LanguageIDs get_linkage_specifier(clang::NamedDecl const * decl)
{
	clang::DeclContext const * ctx = llvm::dyn_cast<clang::DeclContext>(decl);
	if (!ctx)
		return clang::LinkageSpecDecl::lang_cxx;

	ctx = ctx->getParent();
	while (ctx)
	{
		if (clang::LinkageSpecDecl const * lsdecl = llvm::dyn_cast<clang::LinkageSpecDecl>(ctx))
			return lsdecl->getLanguage();

		ctx = ctx->getParent();
	}

	return clang::LinkageSpecDecl::lang_cxx;
}

}

std::string name_mangler::make_decl_name(clang::NamedDecl const * decl, std::string const & static_prefix)
{
	std::string name_str;
	llvm::raw_string_ostream name(name_str);
	if (decl->getLinkage() == clang::UniqueExternalLinkage || decl->getLinkage() == clang::InternalLinkage)
	{
		name << "_S" << static_prefix.size() << static_prefix;
	}

	if (get_linkage_specifier(decl) == clang::LinkageSpecDecl::lang_c || (!llvm::isa<clang::VarDecl>(decl) && !llvm::isa<clang::FunctionDecl>(decl)))
	{
		name << decl->getName();
	}
	else
	{
		if (clang::CXXConstructorDecl const * ctor = llvm::dyn_cast<clang::CXXConstructorDecl>(decl))
			m_mangler->mangleCXXCtor(ctor, clang::Ctor_Complete, name);
		else if (clang::CXXDestructorDecl const * dtor = llvm::dyn_cast<clang::CXXDestructorDecl>(decl))
			m_mangler->mangleCXXDtor(dtor, clang::Dtor_Complete, name);
		else
			m_mangler->mangleName(decl, name);
	}

	m_aliases.insert(std::make_pair(name_str, decl->getQualifiedNameAsString()));
	return name_str;
}

std::string name_mangler::make_rtti_name(clang::QualType type, std::string const & static_prefix)
{
	std::string res_str;
	llvm::raw_string_ostream res(res_str);
	m_mangler->mangleCXXRTTI(type, res);

	return res_str;
}
