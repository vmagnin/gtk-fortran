#! /bin/sh
# An alternative simple build system, using the directory ../build/byscript
# and finally launching one by one the examples for testing.
# It does not substitutes the @..@ strings in gtkf-sketcher.f90,
# and gtk-fortran.f90, like CMake.
# GNU GPL v3
# Contributed by Vincent MAGNIN
# 2011-04-08, last updated 2021-06-11

# For a safer script:
set -eu

# Allow override of default compiler. For example:
#  FC='ifort' ./alt_build_test.sh
# The default compiler is:
: ${FC="gfortran"}

# Major version of GTK for the current branch (from VERSIONS file):
readonly GTKv=$(sed -n -E 's/gtk-fortran;([0-9]+).*/\1/p' ../VERSIONS)
echo "Building gtk-${GTKv}-fortran"

# Compiler and linker options:
readonly gtkoptions="$(pkg-config --cflags --libs gtk"${GTKv}")"

# Go to the top of the project:
cd ..

# Create (if needed) the build/byscript directory and go there:
if [ ! -d build ]; then
    mkdir build
fi
cd build

if [ ! -d byscript ]; then
    mkdir byscript
fi
cd byscript
if [ $? = 0 ]; then
    echo ">>> Removing old files..."
    rm -f ./*.o ./*.mod ./*.out
fi

# Needed to compile High-Level examples:
readonly gtk_hl_obj="gtk-hl-misc.o gtk-hl-button.o gtk-hl-combobox.o gtk-hl-container.o gtk-hl-entry.o gtk-hl-progress.o gtk-hl-spin-slider.o gtk-hl-tree.o  gtk-hl-chooser.o gtk-hl-dialog.o gtk-hl-infobar.o gtk-hl-assistant.o gdk-pixbuf-hl.o"

echo
echo ">>> Compiling the GTK libraries and gtk_hl using ${FC}"
for file in "unixonly-auto.f90" "gdk-auto.f90" "glib-auto.f90" "gtk.f90" "unix-print-auto.f90" "cairo-auto.f90" "gdk-pixbuf-auto.f90" "pango-auto.f90" "gsk-auto.f90" "graphene-auto.f90" "gtk-sup.f90" "gtk-hl-misc.f90" "gtk-hl-button.f90" "gtk-hl-combobox.f90" "gtk-hl-container.f90" "gtk-hl-entry.f90" "gtk-hl-infobar.f90" "gtk-hl-assistant.f90" "gtk-hl-progress.f90" "gtk-hl-spin-slider.f90" "gtk-hl-tree.f90" "gtk-hl-chooser.f90" "gtk-hl-dialog.f90" "gtk-hl.f90" "gdkevents-auto.f90" "gtk-draw-hl.f90" "gdk-pixbuf-hl.f90"; do
  echo "${file}"
  #compile that file:
  "${FC}" -c ../../src/${file} ${gtkoptions}
done

echo
echo ">>> Compiling the examples..."
for i in ../../examples/*.f90 ; do
  #remove the 15th first characters '../../examples/':
  f=$(echo "${i}"|sed 's/^.\{15\}//')
  #remove the .f90 extension:
  e=$(echo "${f}"|sed 's/\.f90//')
  echo "${e}"
  #compile that file:
  "${FC}" gtk.o gtk-sup.o gtk-hl.o unixonly-auto.o ${gtk_hl_obj} gtk-draw-hl.o "${i}" ${gtkoptions} -o "${e}.out"
done
# Other examples:
"${FC}" gtk.o ../../examples/gtkbuilder2.f90 -o gtkbuilder2.out ${gtkoptions} $(pkg-config --cflags --libs gmodule-2.0)

"${FC}" gtk.o gtk-sup.o gtk-hl.o ${gtk_hl_obj} ../../sketcher/gtkf-sketcher.f90 ${gtkoptions} $(pkg-config --cflags --libs gmodule-2.0) -o gtkf-sketcher.out

# List the executables:
echo
echo ">>> Executables in ../build/byscript"
ls ./*.out
echo

echo ">>> Running each example (CTRL+C to exit)..."
for i in *.out ; do
  if [ ! "${i}" = "gio_demo.out" ]; then
    echo "${i}"
    ./"${i}"
  fi
done
