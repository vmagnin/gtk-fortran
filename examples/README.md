# Gtk-fortran Example Programs

This directory contains example programs for the GTK 4 version of gtk-fortran. They are automatically built by CMake but any example may also be individually built on a system with `gtk-4-fortran` installed with the command:

```bash
$ gfortran <name>.f90 -o <name> $(pkg-config --cflags --libs gtk-4-fortran)
```

Among those examples:

- `gtkzero_gapp.f90` just opens an empty GTK window. Based on GtkApplication and GApplication.
- `gtkhello.f90` opens a window with two buttons.
- The `*pixbuf*.f90` examples demonstrate pixel drawing (bitmap drawing). The `pixbuf_without_gui.f9`0 example draws a Sierpinski triangle in a PNG file, without using a GUI.
- The `cairo*.f90` examples demonstrates vectorial drawing using Cairo.
- `bazaar.f90` is used for testing various widgets and functions.
- `gio_demo.f90`: this is a very basic demo to get started with GIO. It uses
Fortran I/O to read text from the keyboard & writes it to the file `gio_demo.dat` in the current directory.
- `gtkbuilder2.f90` demonstrates how you can use an UI XML file to create your graphical user interface.
- `list_demo.f90` demonstrates how to use GtkTreeView for displaying trees and lists.
- `menubar.f90` demonstrates a menubar based on GMenu and GAction. It also uses CSS styles.
- `notebooks.f90` demonstrates how to use GtkNotebook, a tabbed notebook container.
- `regex.f90` (no GUI) demonstrates how to use GLib regular expressions functions.
- `hl_*` examples use the "high-level" interface.
    - `hl_pbar.f90`: a progress bar.
    - ...
- `tests.f90` (no GUI) is testing things about ISO_C_BINDING and the relations between Fortran types and GLib types.
- `tests_gtk_sup.f90` (no GUI) is testing functions of the `gtk_sup` module.
