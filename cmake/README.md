# CMake additional modules

This directory contains the modules to be loaded by `include()` or `find_package()` before checking the default CMake modules:

- `cmake_uninstall.cmake.in`: generic code from https://gitlab.kitware.com/cmake/community/-/wikis/FAQ#can-i-do-make-uninstall-with-cmake
- `DefaultFlags.cmake`: defines default Release and Debug flags for compilers.
