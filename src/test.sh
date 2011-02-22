#! /bin/sh
# Compilation of the examples, for testing gtk-fortran

echo "gtk"
gfortran -c gtk.f90 `pkg-config --cflags --libs gtk+-2.0`

for i in `ls -tr ../examples/*.f90` ; do echo $i ; gfortran gtk.o $i  `pkg-config --cflags --libs gtk+-2.0` -o $i.out ; done

for i in `ls -tr ../examples/*.out` ; do $i ; done
