#!/bin/sh
# Script to build gtk-fortran using CMake
# Vincent MAGNIN, 2016-06-19, modified 2020-02-14

# For a safer script:
set -eu

# Go to the top of the project:
cd ..

# Does the build directory already exists ?
if [ ! -d build ]; then
    mkdir build
    cd build
else
    cd build
    make clean
fi

echo "Building gtk-fortran"
cmake -D CMAKE_BUILD_TYPE=debug ..
# Parallel building:
make -j

echo
echo "TO INSTALL gtk-fortran LIBRARIES ON YOUR SYSTEM, type in the build/ directory:"
echo "  sudo make install"
