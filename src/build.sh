#!/bin/bash
# Script to build gtk-fortran using CMake
# Vincent MAGNIN, 2016-06-19, modified 2019-03-26

echo "Building gtk-fortran..."

cd ..

# If the build directory already exists, this command will print an error but
# the script execution will continue:
mkdir build
cd build

make clean
cmake -D CMAKE_BUILD_TYPE=debug ..

# Parallel building:
make -j

echo
echo "TO INSTALL gtk-fortran LIBRARIES ON YOUR SYSTEM, TYPE :"
echo "cd ../build/ ; sudo make install"
