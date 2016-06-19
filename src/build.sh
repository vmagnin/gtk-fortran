#!/bin/bash
# Script to build gtk-fortran using CMake
# Vincent MAGNIN, 06-19-2016

echo "Building gtk-fortran..."
cd ..
mkdir build
cd build
cmake ..
make -i
echo
echo "TO INSTALL gtk-fortran LIBRARIES ON YOUR SYSTEM, TYPE : cd ../build/ ; sudo make install"
