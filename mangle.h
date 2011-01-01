//===--- Mangle.h - Mangle C++ Names ----------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Implements C++ name mangling according to the Itanium C++ ABI,
// which is used in GCC 3.2 and newer (and many compilers that are
// ABI-compatible with GCC):
//
//   http://www.codesourcery.com/public/cxx-abi/abi.html
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_CODEGEN_MANGLE_H
#define LLVM_CLANG_CODEGEN_MANGLE_H

#include <clang/AST/Type.h>
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/SmallString.h>
#include <llvm/Support/raw_ostream.h>

#include <clang/AST/DeclCXX.h>
#include <clang/AST/DeclObjC.h>

namespace clang {

	/// CXXCtorType - C++ constructor types
	enum CXXCtorType {
		Ctor_Complete,          // Complete object ctor
		Ctor_Base,              // Base object ctor
		Ctor_CompleteAllocating // Complete object allocating ctor
	};

	/// CXXDtorType - C++ destructor types
	enum CXXDtorType {
		Dtor_Deleting, // Deleting dtor
		Dtor_Complete, // Complete object dtor
		Dtor_Base      // Base object dtor
	};

} // end namespace clang

namespace clang {

	namespace CodeGen {

		/// GlobalDecl - represents a global declaration. This can either be a
		/// CXXConstructorDecl and the constructor type (Base, Complete).
		/// a CXXDestructorDecl and the destructor type (Base, Complete) or
		/// a VarDecl, a FunctionDecl or a BlockDecl.
		class GlobalDecl {
			llvm::PointerIntPair<const Decl*, 2> Value;

			void Init(const Decl *D) {
				assert(!isa<CXXConstructorDecl>(D) && "Use other ctor with ctor decls!");
				assert(!isa<CXXDestructorDecl>(D) && "Use other ctor with dtor decls!");

				Value.setPointer(D);
			}

		public:
			GlobalDecl() {}

			GlobalDecl(const VarDecl *D) { Init(D);}
			GlobalDecl(const FunctionDecl *D) { Init(D); }
			GlobalDecl(const BlockDecl *D) { Init(D); }
			GlobalDecl(const ObjCMethodDecl *D) { Init(D); }

			GlobalDecl(const CXXConstructorDecl *D, CXXCtorType Type)
				: Value(D, Type) {}
			GlobalDecl(const CXXDestructorDecl *D, CXXDtorType Type)
				: Value(D, Type) {}

			GlobalDecl getCanonicalDecl() const {
				GlobalDecl CanonGD;
				CanonGD.Value.setPointer(Value.getPointer()->getCanonicalDecl());
				CanonGD.Value.setInt(Value.getInt());

				return CanonGD;
			}

			const Decl *getDecl() const { return Value.getPointer(); }

			CXXCtorType getCtorType() const {
				assert(isa<CXXConstructorDecl>(getDecl()) && "Decl is not a ctor!");
				return static_cast<CXXCtorType>(Value.getInt());
			}

			CXXDtorType getDtorType() const {
				assert(isa<CXXDestructorDecl>(getDecl()) && "Decl is not a dtor!");
				return static_cast<CXXDtorType>(Value.getInt());
			}

			friend bool operator==(const GlobalDecl &LHS, const GlobalDecl &RHS) {
				return LHS.Value == RHS.Value;
			}

			void *getAsOpaquePtr() const { return Value.getOpaqueValue(); }

			static GlobalDecl getFromOpaquePtr(void *P) {
				GlobalDecl GD;
				GD.Value.setFromOpaqueValue(P);
				return GD;
			}
		};

	} // end namespace CodeGen
} // end namespace clang

namespace llvm {
	template<class> struct DenseMapInfo;

	template<> struct DenseMapInfo<clang::CodeGen::GlobalDecl> {
		static inline clang::CodeGen::GlobalDecl getEmptyKey() {
			return clang::CodeGen::GlobalDecl();
		}

		static inline clang::CodeGen::GlobalDecl getTombstoneKey() {
			return clang::CodeGen::GlobalDecl::
				getFromOpaquePtr(reinterpret_cast<void*>(-1));
		}

		static unsigned getHashValue(clang::CodeGen::GlobalDecl GD) {
			return DenseMapInfo<void*>::getHashValue(GD.getAsOpaquePtr());
		}

		static bool isEqual(clang::CodeGen::GlobalDecl LHS, 
			clang::CodeGen::GlobalDecl RHS) {
				return LHS == RHS;
		}

	};

	// GlobalDecl isn't *technically* a POD type. However, its copy constructor,
	// copy assignment operator, and destructor are all trivial.
	template <>
	struct isPodLike<clang::CodeGen::GlobalDecl> {
		static const bool value = true;
	};
} // end namespace llvm

namespace clang {
  class ASTContext;
  class BlockDecl;
  class CXXConstructorDecl;
  class CXXDestructorDecl;
  class CXXMethodDecl;
  class FunctionDecl;
  class NamedDecl;
  class ObjCMethodDecl;
  class VarDecl;

namespace CodeGen {
  struct ThisAdjustment;
  struct ThunkInfo;

/// MangleBuffer - a convenient class for storing a name which is
/// either the result of a mangling or is a constant string with
/// external memory ownership.
class MangleBuffer {
public:
  void setString(llvm::StringRef Ref) {
    String = Ref;
  }

  llvm::SmallVectorImpl<char> &getBuffer() {
    return Buffer;
  }

  llvm::StringRef getString() const {
    if (!String.empty()) return String;
    return Buffer.str();
  }

  operator llvm::StringRef() const {
    return getString();
  }

private:
  llvm::StringRef String;
  llvm::SmallString<256> Buffer;
};

/// MangleContext - Context for tracking state which persists across multiple
/// calls to the C++ name mangler.
class MangleContext {
  ASTContext &Context;

  llvm::DenseMap<const TagDecl *, uint64_t> AnonStructIds;
  unsigned Discriminator;
  llvm::DenseMap<const NamedDecl*, unsigned> Uniquifier;
  llvm::DenseMap<const BlockDecl*, unsigned> GlobalBlockIds;
  llvm::DenseMap<const BlockDecl*, unsigned> LocalBlockIds;
  
public:
  explicit MangleContext(ASTContext &Context)
    : Context(Context) { }

  virtual ~MangleContext() { }

  ASTContext &getASTContext() const { return Context; }

  void startNewFunction() { LocalBlockIds.clear(); }
  
  uint64_t getAnonymousStructId(const TagDecl *TD) {
    std::pair<llvm::DenseMap<const TagDecl *,
      uint64_t>::iterator, bool> Result =
      AnonStructIds.insert(std::make_pair(TD, AnonStructIds.size()));
    return Result.first->second;
  }

  unsigned getBlockId(const BlockDecl *BD, bool Local) {
    llvm::DenseMap<const BlockDecl *, unsigned> &BlockIds
      = Local? LocalBlockIds : GlobalBlockIds;
    std::pair<llvm::DenseMap<const BlockDecl *, unsigned>::iterator, bool>
      Result = BlockIds.insert(std::make_pair(BD, BlockIds.size()));
    return Result.first->second;
  }
  
  /// @name Mangler Entry Points
  /// @{

  virtual bool shouldMangleDeclName(const NamedDecl *D);
  virtual void mangleName(const NamedDecl *D, llvm::SmallVectorImpl<char> &);
  virtual void mangleReferenceTemporary(const VarDecl *D,
                                        llvm::SmallVectorImpl<char> &);
  virtual void mangleCXXVTable(const CXXRecordDecl *RD,
                               llvm::SmallVectorImpl<char> &);
  virtual void mangleCXXVTT(const CXXRecordDecl *RD,
                            llvm::SmallVectorImpl<char> &);
  virtual void mangleCXXCtorVTable(const CXXRecordDecl *RD, int64_t Offset,
                                   const CXXRecordDecl *Type,
                                   llvm::SmallVectorImpl<char> &);
  virtual void mangleCXXRTTI(QualType T, llvm::SmallVectorImpl<char> &);
  virtual void mangleCXXRTTIName(QualType T, llvm::SmallVectorImpl<char> &);
  virtual void mangleCXXCtor(const CXXConstructorDecl *D, CXXCtorType Type,
                             llvm::SmallVectorImpl<char> &);
  virtual void mangleCXXDtor(const CXXDestructorDecl *D, CXXDtorType Type,
                             llvm::SmallVectorImpl<char> &);
  void mangleBlock(GlobalDecl GD,
                   const BlockDecl *BD, llvm::SmallVectorImpl<char> &);

  // This is pretty lame.
  void mangleItaniumGuardVariable(const VarDecl *D,
                                  llvm::SmallVectorImpl<char> &);

  void mangleInitDiscriminator() {
    Discriminator = 0;
  }

  bool getNextDiscriminator(const NamedDecl *ND, unsigned &disc) {
    unsigned &discriminator = Uniquifier[ND];
    if (!discriminator)
      discriminator = ++Discriminator;
    if (discriminator == 1)
      return false;
    disc = discriminator-2;
    return true;
  }
  /// @}
};

/// MiscNameMangler - Mangles Objective-C method names and blocks.
class MiscNameMangler {
  MangleContext &Context;
  llvm::raw_svector_ostream Out;
  
  ASTContext &getASTContext() const { return Context.getASTContext(); }

public:
  MiscNameMangler(MangleContext &C, llvm::SmallVectorImpl<char> &Res);

  llvm::raw_svector_ostream &getStream() { return Out; }
  
  void mangleBlock(GlobalDecl GD, const BlockDecl *BD);
  void mangleObjCMethodName(const ObjCMethodDecl *MD);
};

}
}

#endif
