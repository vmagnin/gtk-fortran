echo OFF
echo Please be patient...
echo The hl_ examples need GTK+ 2.18 or higher
echo
echo Compiling gtk-fortran
echo ON

gfortran -c glib-auto.f90 gtk.f90 gtk-sup.f90 gtk-hl-misc.f90 gtk-hl-accelerator.f90 gtk-hl-button.f90 gtk-hl-combobox.f90 gtk-hl-container.f90 gtk-hl-entry.f90 gtk-hl-menu.f90 gtk-hl-progress.f90 gtk-hl-spin-slider.f90 gtk-hl-tree.f90  gtk-hl-chooser.f90 gtk-hl-dialog.f90 gtk-hl.f90 atk-auto.f90 cairo-auto.f90 gdk-auto.f90 gdk-pixbuf-auto.f90 pango-auto.f90 -L. -llibgtk-win32-2.0-0 -llibgdk-win32-2.0-0 -llibgobject-2.0-0 -llibgdk_pixbuf-2.0-0 -llibcairo-2

gfortran gtk.o gtk-sup.o gtk-hl-misc.o gtk-hl-accelerator.o gtk-hl-button.o gtk-hl-combobox.o gtk-hl-container.o gtk-hl-entry.o gtk-hl-menu.o gtk-hl-progress.o gtk-hl-spin-slider.o gtk-hl-tree.o  gtk-hl-chooser.o gtk-hl-dialog.o gtk-hl.o ../examples/hl_choosers.f90 -o hl_choosers.exe -L. -llibglib-2.0-0 -llibgtk-win32-2.0-0 -llibgdk-win32-2.0-0 -llibgobject-2.0-0 -llibgdk_pixbuf-2.0-0 -llibcairo-2
gfortran gtk.o gtk-sup.o gtk-hl.o ../examples/hl_radio.f90 -o hl_radio.exe -L. -llibglib-2.0-0 -llibgtk-win32-2.0-0 -llibgdk-win32-2.0-0 -llibgobject-2.0-0 -llibgdk_pixbuf-2.0-0 -llibcairo-2
gfortran gtk.o gtk-sup.o gtk-hl-misc.o gtk-hl-accelerator.o gtk-hl-button.o gtk-hl-combobox.o gtk-hl-container.o gtk-hl-entry.o gtk-hl-menu.o gtk-hl-progress.o gtk-hl-spin-slider.o gtk-hl-tree.o  gtk-hl-chooser.o gtk-hl-dialog.o gtk-hl.o ../examples/hl_pbar.f90 -o hl_pbar.exe -L. -llibglib-2.0-0 -llibgtk-win32-2.0-0 -llibgdk-win32-2.0-0 -llibgobject-2.0-0 -llibgdk_pixbuf-2.0-0 -llibcairo-2
gfortran gtk.o gtk-sup.o gtk-hl-misc.o gtk-hl-accelerator.o gtk-hl-button.o gtk-hl-combobox.o gtk-hl-container.o gtk-hl-entry.o gtk-hl-menu.o gtk-hl-progress.o gtk-hl-spin-slider.o gtk-hl-tree.o  gtk-hl-chooser.o gtk-hl-dialog.o gtk-hl.o ../examples/hl_pbar_p.f90 -o hl_pbar_p.exe -L. -llibglib-2.0-0 -llibgtk-win32-2.0-0 -llibgdk-win32-2.0-0 -llibgobject-2.0-0 -llibgdk_pixbuf-2.0-0 -llibcairo-2
gfortran gtk.o gtk-sup.o gtk-hl-misc.o gtk-hl-accelerator.o gtk-hl-button.o gtk-hl-combobox.o gtk-hl-container.o gtk-hl-entry.o gtk-hl-menu.o gtk-hl-progress.o gtk-hl-spin-slider.o gtk-hl-tree.o  gtk-hl-chooser.o gtk-hl-dialog.o gtk-hl.o ../examples/hl_menu.f90 -o hl_menu.exe -L. -llibglib-2.0-0 -llibgtk-win32-2.0-0 -llibgdk-win32-2.0-0 -llibgobject-2.0-0 -llibgdk_pixbuf-2.0-0 -llibcairo-2
gfortran gtk.o gtk-sup.o gtk-hl-misc.o gtk-hl-accelerator.o gtk-hl-button.o gtk-hl-combobox.o gtk-hl-container.o gtk-hl-entry.o gtk-hl-menu.o gtk-hl-progress.o gtk-hl-spin-slider.o gtk-hl-tree.o  gtk-hl-chooser.o gtk-hl-dialog.o gtk-hl.o ../examples/hl_list1.f90 -o hl_list1.exe -L. -llibglib-2.0-0 -llibgtk-win32-2.0-0 -llibgdk-win32-2.0-0 -llibgobject-2.0-0 -llibgdk_pixbuf-2.0-0 -llibcairo-2
gfortran gtk.o gtk-sup.o gtk-hl-misc.o gtk-hl-accelerator.o gtk-hl-button.o gtk-hl-combobox.o gtk-hl-container.o gtk-hl-entry.o gtk-hl-menu.o gtk-hl-progress.o gtk-hl-spin-slider.o gtk-hl-tree.o  gtk-hl-chooser.o gtk-hl-dialog.o gtk-hl.o ../examples/hl_list_n.f90 -o hl_list_n.exe -L. -llibglib-2.0-0 -llibgtk-win32-2.0-0 -llibgdk-win32-2.0-0 -llibgobject-2.0-0 -llibgdk_pixbuf-2.0-0 -llibcairo-2
gfortran gtk.o gtk-sup.o gtk-hl.o ../examples/hl_dialog.f90 -o hl_dialog.exe -L. -llibglib-2.0-0 -llibgtk-win32-2.0-0 -llibgdk-win32-2.0-0 -llibgobject-2.0-0 -llibgdk_pixbuf-2.0-0 -llibcairo-2
gfortran gtk.o gtk-sup.o gtk-hl-misc.o gtk-hl-accelerator.o gtk-hl-button.o gtk-hl-combobox.o gtk-hl-container.o gtk-hl-entry.o gtk-hl-menu.o gtk-hl-progress.o gtk-hl-spin-slider.o gtk-hl-tree.o  gtk-hl-chooser.o gtk-hl-dialog.o gtk-hl.o ../examples/hl_combo.f90 -o hl_combo.exe -L. -llibglib-2.0-0 -llibgtk-win32-2.0-0 -llibgdk-win32-2.0-0 -llibgobject-2.0-0 -llibgdk_pixbuf-2.0-0 -llibcairo-2
gfortran gtk.o gtk-sup.o gtk-hl.o ../examples/hl_sliders.f90 -o hl_sliders.exe -L. -llibglib-2.0-0 -llibgtk-win32-2.0-0 -llibgdk-win32-2.0-0 -llibgobject-2.0-0 -llibgdk_pixbuf-2.0-0 -llibcairo-2
gfortran gtk.o gtk-sup.o gtk-hl-misc.o gtk-hl-accelerator.o gtk-hl-button.o gtk-hl-combobox.o gtk-hl-container.o gtk-hl-entry.o gtk-hl-menu.o gtk-hl-progress.o gtk-hl-spin-slider.o gtk-hl-tree.o  gtk-hl-chooser.o gtk-hl-dialog.o gtk-hl.o ../examples/hl_textview.f90 -o hl_textview.exe -L. -llibglib-2.0-0 -llibgtk-win32-2.0-0 -llibgdk-win32-2.0-0 -llibgobject-2.0-0 -llibgdk_pixbuf-2.0-0 -llibcairo-2

gfortran gtk.o cairo.o ../examples/mandelbrot_pixbuf.f90 -o mandelbrot_pixbuf.exe -L. -llibgtk-win32-2.0-0 -llibgdk-win32-2.0-0 -llibgobject-2.0-0 -llibgdk_pixbuf-2.0-0 -llibcairo-2
gfortran gtk.o ../examples/notebooks.f90 -o notebooks.exe -L. -llibgtk-win32-2.0-0 -llibgdk-win32-2.0-0 -llibgobject-2.0-0 -llibgdk_pixbuf-2.0-0 -llibcairo-2
gfortran gtk.o glib-auto.o ../examples/tests.f90 -o tests.exe -L. -llibgtk-win32-2.0-0 -llibglib-2.0-0 -llibgdk-win32-2.0-0 -llibgobject-2.0-0 -llibgdk_pixbuf-2.0-0 -llibcairo-2
gfortran gtk.o ../examples/menu.f90 -o menu.exe -L. -llibgtk-win32-2.0-0 -llibgdk-win32-2.0-0 -llibgobject-2.0-0 -llibgdk_pixbuf-2.0-0 -llibcairo-2
gfortran gtk.o ../examples/mandelbrot.f90 -o mandelbrot.exe -L. -llibgtk-win32-2.0-0 -llibgdk-win32-2.0-0 -llibgobject-2.0-0 -llibgdk_pixbuf-2.0-0 -llibcairo-2
gfortran gtk.o ../examples/list_demo.f90 -o list_demo.exe -L. -llibgtk-win32-2.0-0 -llibgdk-win32-2.0-0 -llibgobject-2.0-0 -llibgdk_pixbuf-2.0-0 -llibcairo-2
gfortran gtk.o ../examples/julia_pixbuf.f90 -o julia_pixbuf.exe -L. -llibgtk-win32-2.0-0 -llibgdk-win32-2.0-0 -llibgobject-2.0-0 -llibgdk_pixbuf-2.0-0 -llibcairo-2
gfortran gtk.o ../examples/gtkhello2.f90 -o gtkhello2.exe -L. -llibgtk-win32-2.0-0 -llibgdk-win32-2.0-0 -llibgobject-2.0-0 -llibgdk_pixbuf-2.0-0 -llibcairo-2
gfortran gtk.o ../examples/cairo-basics.f90 -o cairo-basics.exe -L. -llibgtk-win32-2.0-0 -llibgdk-win32-2.0-0 -llibgobject-2.0-0 -llibgdk_pixbuf-2.0-0 -llibcairo-2
gfortran gtk.o ../examples/cairo-tests.f90 -o cairo-tests.exe -L. -llibgtk-win32-2.0-0 -llibgdk-win32-2.0-0 -llibgobject-2.0-0 -llibgdk_pixbuf-2.0-0 -llibcairo-2
gfortran gtk.o ../examples/bazaar.f90 -o bazaar.exe -L. -llibgtk-win32-2.0-0 -llibgdk-win32-2.0-0 -llibgobject-2.0-0 -llibgdk_pixbuf-2.0-0 -llibcairo-2
gfortran gtk.o ../examples/gtkbuilder.f90 -o gtkbuilder.exe -L. -llibgtk-win32-2.0-0 -llibgdk-win32-2.0-0 -llibgobject-2.0-0 -llibgdk_pixbuf-2.0-0 -llibcairo-2
gfortran gtk.o ../examples/gtkbuilder2.f90 -o gtkbuilder2.exe -L. -llibgmodule-2.0-0 -llibglib-2.0-0 -llibgtk-win32-2.0-0 -llibgdk-win32-2.0-0 -llibgobject-2.0-0 -llibgdk_pixbuf-2.0-0 -llibcairo-2
