#! /bin/sh
# Compilation of the examples, for testing gtk-fortran

echo "gtk"
gfortran -c gdk-auto.f90 `pkg-config --cflags --libs gtk+-2.0`
gfortran -c glib-auto.f90 `pkg-config --cflags --libs gtk+-2.0`
gfortran -c gtk.f90 `pkg-config --cflags --libs gtk+-2.0`
gfortran -c atk-auto.f90 `pkg-config --cflags --libs gtk+-2.0`
gfortran -c cairo-auto.f90 `pkg-config --cflags --libs gtk+-2.0`
gfortran -c gdk-pixbuf-auto.f90 `pkg-config --cflags --libs gtk+-2.0`
gfortran -c pango-auto.f90 `pkg-config --cflags --libs gtk+-2.0`

for i in `ls -tr ../examples/*.f90` ; do echo $i ; gfortran *.o $i  `pkg-config --cflags --libs gtk+-2.0` -o $i.out ; done

for i in `ls -tr ../examples/*.out` ; do $i ; done
