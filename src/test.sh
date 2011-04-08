#! /bin/sh
# Compilation of gtk-fortran and its examples, using GTK+ 3.0
# April 8th 2011

rm *.o
rm *.mod
rm *.out
rm ../examples/*.mod
rm ../examples/*.out

echo "gtk..."
gfortran -c gdk-auto.f90 `pkg-config --cflags --libs gtk+-3.0`
gfortran -c glib-auto.f90 `pkg-config --cflags --libs gtk+-3.0`
gfortran -c gtk.f90 `pkg-config --cflags --libs gtk+-3.0`
gfortran -c atk-auto.f90 `pkg-config --cflags --libs gtk+-3.0`
gfortran -c cairo-auto.f90 `pkg-config --cflags --libs gtk+-3.0`
gfortran -c gdk-pixbuf-auto.f90 `pkg-config --cflags --libs gtk+-3.0`
gfortran -c pango-auto.f90 `pkg-config --cflags --libs gtk+-3.0`
gfortran -c gtk-sup.f90 `pkg-config --cflags --libs gtk+-3.0`
gfortran -c gtk-hl.f90 `pkg-config --cflags --libs gtk+-3.0`

echo "examples..."
#for i in `ls -tr ../examples/*.f90` ; do echo $i ; gfortran gtk.o gtk-sup.o gtk-hl.o $i  `pkg-config --cflags --libs gtk+-3.0` -o $i.out ; done
for i in `ls -tr ../examples/*.f90` ; do echo $i ; gfortran gtk.o gtk-sup.o $i  `pkg-config --cflags --libs gtk+-3.0` -o $i.out ; done
gfortran gtk.o ../examples/gtkbuilder2.f90 -o ../examples/gtkbuilder2.f90.out `pkg-config --cflags --libs gtk+-3.0` `pkg-config --cflags --libs gmodule-2.0`

cd ../examples/

for i in `ls -tr *.out` ; do ./$i ; done
