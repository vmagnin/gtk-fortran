# Gtk-fortran Example Programs

This directory contains example programs for the GTK 4 version of gtk-fortran.
They are automatically built by CMake but any example may also be individually 
built on a system with `gtk-4-fortran` installed with the command:

```bash
$ gfortran <name>.f90 -o <name> $(pkg-config --cflags --libs gtk-4-fortran)
```

Among those examples:

- gtkzero_gapp.f90 just opens an empty GTK window. Based on GtkApplication and GApplication. 
- gtkhello.f90 opens a window with two buttons.
- The \*pixbuf\*.f90 examples demonstrate pixel drawing (bitmap drawing). The pixbuf\_without\_gui.f90 example draws a Sierpinski triangle in a PNG file, without using a GUI.
- The cairo*.f90 examples desmonstrate vectorial drawing using Cairo.
- bazaar.f90 is used for testing various widgets and functions.
- gio_demo.f90: this is a very basic demo to get started with GIO. It uses 
Fortran I/O to read text from the keyboard & writes it to the file `gio_demo.dat`
in the current directory.
- gtkbuilder*.f90 examples demonstrates how you can use a Glade3 XML file to
create your graphical user interface.
- list_demo.f90 demonstrates how to use GtkTreeView for displaying trees and 
lists.
- notebooks.f90 demonstrates how to use GtkNotebook, a tabbed notebook container.
- hl_* examples uses the "high-level" interface.
    - hl_pbar.f90:  a progress bar.
    - hl_pbar_p.f90: several progress bar in parallel (if you compile with `-openmp`)
- tests.f90 is testing things about ISO_C_BINDING and the relations between 
Fortran types and GLib types.
