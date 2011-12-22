#! /bin/sh
# Compilation of gtk-fortran and its examples, using GTK+ 3.0
# April 8th 2011, last modification: May 22th, 2011

echo "Removing old files..."
rm *.o
rm *.mod
rm *.out
rm ../examples/*.mod
rm ../examples/*.out

gtkversion="`pkg-config --cflags --libs gtk+-3.0`"

gtk_hl_obj="gtk-hl-misc.o gtk-hl-accelerator.o gtk-hl-button.o gtk-hl-combobox.o gtk-hl-container.o gtk-hl-entry.o gtk-hl-menu.o gtk-hl-progress.o gtk-hl-spin-slider.o gtk-hl-tree.o  gtk-hl-chooser.o gtk-hl-dialog.o"

echo "gtk..."
for file in "gdk-auto.f90" "glib-auto.f90" "gtk.f90" "unix-print-auto.f90" "atk-auto.f90" "cairo-auto.f90" "gdk-pixbuf-auto.f90" "pango-auto.f90" "gtk-sup.f90" "gtk-hl-misc.f90" "gtk-hl-accelerator.f90" "gtk-hl-button.f90" "gtk-hl-combobox.f90" "gtk-hl-container.f90" "gtk-hl-entry.f90" "gtk-hl-menu.f90" "gtk-hl-progress.f90" "gtk-hl-spin-slider.f90" "gtk-hl-tree.f90"  "gtk-hl-chooser.f90" "gtk-hl-dialog.f90" "gtk-hl.f90" "gdkevents-auto3.f90" "gtk-draw-hl.f90"; do 
  gfortran -c $file $gtkversion
done

echo "examples..."
for i in ../examples/*.f90 ; do 
  echo $i
  gfortran gtk.o gtk-sup.o gtk-hl.o ${gtk_hl_obj} gtk-draw-hl.o $i $gtkversion -o $i.out
done
gfortran gtk.o ../examples/gtkbuilder2.f90 -o ../examples/gtkbuilder2.f90.out $gtkversion `pkg-config --cflags --libs gmodule-2.0`
gfortran gtk.o gtk-sup.o gtk-hl.o ../sketcher/gtkf-sketcher.f90 -o ../sketcher/gtkf-sketcher.f90.out $gtkversion `pkg-config --cflags --libs gmodule-2.0`

echo "running the examples..."
cd ../examples/
for i in *.out ; do 
  echo $i
  ./$i
done
