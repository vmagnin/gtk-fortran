#!/bin/bash
# Script to build gtk-fortran using CMake
# Vincent MAGNIN, 2016-06-19, modified 2019-04-09

echo "Building gtk-fortran..."
cd ..
mkdir build
cd build
cmake ..
make -i
echo
echo "TO INSTALL gtk-fortran LIBRARIES ON YOUR SYSTEM, type in the build/ directory:"
echo "  sudo make install"
