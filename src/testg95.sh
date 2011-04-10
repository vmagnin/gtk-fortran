#! /bin/sh
# Compilation of gtk-fortran and its examples
# April 7th 2011

rm *.o
rm *.mod
rm *.out
rm ../examples/*.mod
rm ../examples/*.out

echo "gtk..."
g95 -c gdk-auto.f90 `pkg-config --cflags --libs gtk+-2.0`
g95 -c glib-auto.f90 `pkg-config --cflags --libs gtk+-2.0`
g95 -c gtk.f90 `pkg-config --cflags --libs gtk+-2.0`
g95 -c atk-auto.f90 `pkg-config --cflags --libs gtk+-2.0`
g95 -c cairo-auto.f90 `pkg-config --cflags --libs gtk+-2.0`
g95 -c gdk-pixbuf-auto.f90 `pkg-config --cflags --libs gtk+-2.0`
g95 -c pango-auto.f90 `pkg-config --cflags --libs gtk+-2.0`
g95 -c gtk-sup.f90 `pkg-config --cflags --libs gtk+-2.0`
g95 -c gtk-hl.f90 `pkg-config --cflags --libs gtk+-2.0`

echo "examples..."
for i in `ls -tr ../examples/*.f90` ; do echo $i ; g95 gtk.o gtk-sup.o gtk-hl.o $i  `pkg-config --cflags --libs gtk+-2.0` -o $i.out ; done
g95 gtk.o ../examples/gtkbuilder2.f90 -o ../examples/gtkbuilder2.f90.out `pkg-config --cflags --libs gtk+-2.0` `pkg-config --cflags --libs gmodule-2.0`

cd ../examples/

for i in `ls -tr *.out` ; do ./$i ; done
