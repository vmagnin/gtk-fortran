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
- The *pixbuf*.f90 examples demonstrate pixel drawing (bitmap drawing).
- The cairo*.f90 examples desmonstrate vectorial drawing using Cairo.
- bazaar.f90 is used for testing various widgets and functions.
- gio_demo.f90: this is a very basic demo to get started with GIO. It uses 
Fortran I/O to read text from the keyboard & writes it to the file `gio_demo.dat`
in the current directory.
- gtkbuilder*.f90 examples demonstrates how you can use a Glade3 XML file to
create your graphical user interface.
- list_demo.f90 demonstrates how to use GtkTreeView for displaying trees and 
lists.
- menu.f90 demonstrates how to use GtkActionGroup to create a menu. But gtk_action_new gtk_ui_manager_new are deprecated since version 3.10 and should not be used in newly-written code.
- menu2.f90 demonstrates how to create a menu.
- notebooks.f90 demonstrates how to use GtkNotebook, a tabbed notebook container.
- hl_* examples uses the "high-level" interface.
- tests.f90 is testing things about ISO_C_BINDING and the relations between 
Fortran types and GLib types.
