#! /bin/sh
# Compilation of gtk-fortran and its examples, using GTK+ 2.0
# GNU GPL v3
# Contributed by Vincent MAGNIN
# April 8th 2011, updated May 22th

echo "Removing old files..."
rm *.o
rm *.mod
rm *.out
rm ../examples/*.mod
rm ../examples/*.out

gtkversion="`pkg-config --cflags --libs gtk+-2.0`"

echo "gtk..."
# gtk3 only: "unix-print-auto.f90"
for file in "gdk-auto.f90" "glib-auto.f90" "gtk.f90" "atk-auto.f90" "cairo-auto.f90" "gdk-pixbuf-auto.f90" "pango-auto.f90" "gtk-sup.f90" "gtk-hl.f90" "gdkevents-auto2.f90"; do 
  gfortran -c $file $gtkversion
done

echo "examples..."
for i in ../examples/*.f90 ; do 
  echo $i
  gfortran gtk.o gtk-sup.o gtk-hl.o $i $gtkversion -o $i.out
done
gfortran gtk.o ../examples/gtkbuilder2.f90 -o ../examples/gtkbuilder2.f90.out $gtkversion `pkg-config --cflags --libs gmodule-2.0`

echo "running the examples..."
cd ../examples/
for i in *.out ; do 
  echo $i
  ./$i
done
