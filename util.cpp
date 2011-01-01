#include "util.hpp"
#include <clang/AST/DeclCXX.h>
#include <clang/AST/DeclTemplate.h>
#include <boost/assert.hpp>
#include <boost/lexical_cast.hpp>

#include "mangle.h"

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

std::string make_decl_name(clang::NamedDecl const * decl, std::string const & static_prefix)
{
	std::string name;

	if (decl->getLinkage() == clang::UniqueExternalLinkage || decl->getLinkage() == clang::InternalLinkage)
	{
		name = "_S";
		name.append(boost::lexical_cast<std::string>(static_prefix.size()));
		name.append(static_prefix);
	}

	if (get_linkage_specifier(decl) == clang::LinkageSpecDecl::lang_c || (!llvm::isa<clang::VarDecl>(decl) && !llvm::isa<clang::FunctionDecl>(decl)))
		return name + decl->getNameAsString();

	clang::CodeGen::MangleContext ctx(decl->getASTContext());

	llvm::SmallVector<char, 64> res;
	if (clang::CXXConstructorDecl const * ctor = llvm::dyn_cast<clang::CXXConstructorDecl>(decl))
		ctx.mangleCXXCtor(ctor, clang::Ctor_Complete, res);
	else if (clang::CXXDestructorDecl const * dtor = llvm::dyn_cast<clang::CXXDestructorDecl>(decl))
		ctx.mangleCXXDtor(dtor, clang::Dtor_Complete, res);
	else
		ctx.mangleName(decl, res);

	return name + std::string(res.begin(), res.end());
}
