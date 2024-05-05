#!/bin/sh
# This file is part of gtk-fortran, a GTK / Fortran interface library.
# Copyright (C) 2016 The gtk-fortran team
#
# Interactive script to build and test gtk-fortran with GFortran and Intel ifx
# before release
# Vincent MAGNIN, 2016-06-19
# Last modification: 2023-03-20

# For a safer script:
set -u

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

echo '-------------------------------------------------------------------------'
echo 'Do you want to build gtk-fortran with the default Fortran compiler? (y/N)'
read -r answer
case ${answer} in
'Y' | 'y')
    cmake -D CMAKE_BUILD_TYPE=debug .. && make -j
    ;;
*)
    echo 'No building'
    ;;
esac

echo '-----------------------------------------------'
echo 'Do you want to launch the tests examples? (y/N)'
read -r answer
case ${answer} in
'Y' | 'y')
    # Launch all the examples, four at at time:
    ctest --timeout 5 -j 4 -VV
    ;;
*)
    echo 'Not testing'
    ;;
esac

echo '--------------------------------------------------------------------'
echo 'Do you want to install the gtk-fortran library on your system? (y/N)'
read -r answer
case ${answer} in
'Y' | 'y')
    sudo make install
    ;;
*)
    echo 'No installation'
    ;;
esac

echo '--------------------------------------------------------------------'
# Is Intel ifx installed on the system?
if ifx --version; then
    echo 'Do you want to build and test gtk-fortran with Intel ifx? (y/N)'
    read -r answer
    case ${answer} in
    'Y' | 'y')
        # Does that directory already exist?
        if [ ! -d /tmp/gtk-fortran/ ]; then
            mkdir /tmp/gtk-fortran/
        fi
        # Copy the gtk-fortran directory in /tmp/
        cp -r ../* /tmp/gtk-fortran/
        cd /tmp/gtk-fortran/build
        rm CMakeCache.txt
        make clean
        cmake -D CMAKE_BUILD_TYPE=debug -D EXCLUDE_PLPLOT=true -D CMAKE_Fortran_COMPILER:FILEPATH="$(which ifx)" .. && make -j && ctest --timeout 5 -j 4 -VV
        ;;
    *)
        echo 'No ifx build'
        ;;
    esac
fi
