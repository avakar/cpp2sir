## Installation

First, install LLVM and clang (LLVM's C/C++/ObjC frontend).
The clang libraries are used to generate C++ AST and to
perform template instantiation.
I recommend always using the trunk version of both LLVM and clang
(they are kept in sync and I'll try to keep cpp2sir in sync as well).
I will also keep this file updated with the revisions of LLVM and clang
that are known to work.

    $ svn co -r 123537 http://llvm.org/svn/llvm-project/llvm/trunk llvm
    $ svn co -r 123539 http://llvm.org/svn/llvm-project/cfe/trunk llvm/tools/clang

Use `cmake` to generate whatever makefiles you use on your platform.
I recommend performing the build in a nested directory so as to keep the source
directories clean.

    $ mkdir build
    $ cd build
    $ cmake .. -DCMAKE_INSTALL_PREFIX=<prefix>

After you run `cmake`, use the generated makefiles to build and install the libraries.
Once the installation is complete, you can configure and build cpp2sir.

    $ cmake .. -DLLVM_INCLUDE_DIR=<llvm-include> -DLLVM_LIB_DIR=<llvm-lib>

If you can't get the sources to compile, please, [let me know][1].

  [1]: https://github.com/avakar/cpp2sir/issues
