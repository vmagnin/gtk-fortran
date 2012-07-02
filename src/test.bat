@echo OFF
echo Removing old files...
del *.o
del *.mod
del *.exe
del *.glade
echo.

rem set variables
set gtk-fortran-files=glib-auto.f90 gtk.f90 gtk-sup.f90 atk-auto.f90 cairo-auto.f90 gdk-auto.f90 gdk-pixbuf-auto.f90 pango-auto.f90 gtk-hl-misc.f90 gtk-hl-accelerator.f90 gtk-hl-button.f90 gtk-hl-combobox.f90 gtk-hl-container.f90 gtk-hl-entry.f90 gtk-hl-menu.f90 gtk-hl-progress.f90 gtk-hl-spin-slider.f90 gtk-hl-tree.f90 gtk-hl-chooser.f90 gtk-hl-dialog.f90 gtk-hl.f90 
set gtk-fortran-obj=gtk.o gtk-sup.o
set gtk-hl-obj=gtk-hl.o gtk-hl-misc.o gtk-hl-accelerator.o gtk-hl-button.o gtk-hl-combobox.o gtk-hl-container.o gtk-hl-entry.o gtk-hl-menu.o gtk-hl-progress.o gtk-hl-spin-slider.o gtk-hl-tree.o gtk-hl-chooser.o gtk-hl-dialog.o
for %%X in (pkg-config.exe) do (set FOUND=%%~$PATH:X)
if defined FOUND (
echo Found pkg-config
pkg-config --libs gtk+-2.0 > tmp.txt
set /p gtk-libraries= < tmp.txt
del tmp.txt
echo gtk-libraries=%gtk-libraries%
echo.
) else (
set gtk-libraries=-L. -llibgtk-win32-2.0-0 -llibgdk-win32-2.0-0 -llibgobject-2.0-0 -llibgdk_pixbuf-2.0-0 -llibcairo-2 -llibglib-2.0-0
)

echo Compiling gtk libraries...
gfortran -c %gtk-fortran-files% %gtk-libraries%
echo.

echo Compiling examples...
gfortran %gtk-fortran-obj% %gtk-hl-obj% ../examples/hl_choosers.f90 -o hl_choosers.exe %gtk-libraries%
gfortran %gtk-fortran-obj% %gtk-hl-obj% ../examples/hl_radio.f90 -o hl_radio.exe %gtk-libraries%
gfortran %gtk-fortran-obj% %gtk-hl-obj% ../examples/hl_pbar.f90 -o hl_pbar.exe %gtk-libraries%
gfortran %gtk-fortran-obj% %gtk-hl-obj% ../examples/hl_pbar_p.f90 -o hl_pbar_p.exe %gtk-libraries%
gfortran %gtk-fortran-obj% %gtk-hl-obj% ../examples/hl_menu.f90 -o hl_menu.exe %gtk-libraries%
gfortran %gtk-fortran-obj% %gtk-hl-obj% ../examples/hl_list1.f90 -o hl_list1.exe %gtk-libraries%
gfortran %gtk-fortran-obj% %gtk-hl-obj% ../examples/hl_list_n.f90 -o hl_list_n.exe %gtk-libraries%
gfortran %gtk-fortran-obj% %gtk-hl-obj% ../examples/hl_dialog.f90 -o hl_dialog.exe %gtk-libraries%
gfortran %gtk-fortran-obj% %gtk-hl-obj% ../examples/hl_combo.f90 -o hl_combo.exe %gtk-libraries%
gfortran %gtk-fortran-obj% %gtk-hl-obj% ../examples/hl_sliders.f90 -o hl_sliders.exe %gtk-libraries%
gfortran %gtk-fortran-obj% %gtk-hl-obj% ../examples/hl_textview.f90 -o hl_textview.exe %gtk-libraries%
gfortran %gtk-fortran-obj% ../examples/mandelbrot_pixbuf.f90 -o mandelbrot_pixbuf.exe %gtk-libraries%
gfortran %gtk-fortran-obj% ../examples/notebooks.f90 -o notebooks.exe %gtk-libraries%
gfortran %gtk-fortran-obj% glib-auto.o ../examples/tests.f90 -o tests.exe %gtk-libraries%
gfortran %gtk-fortran-obj% ../examples/menu.f90 -o menu.exe %gtk-libraries%
gfortran %gtk-fortran-obj% ../examples/mandelbrot.f90 -o mandelbrot.exe %gtk-libraries%
gfortran %gtk-fortran-obj% ../examples/list_demo.f90 -o list_demo.exe %gtk-libraries%
gfortran %gtk-fortran-obj% ../examples/julia_pixbuf.f90 -o julia_pixbuf.exe %gtk-libraries%
gfortran %gtk-fortran-obj% ../examples/gtkhello2.f90 -o gtkhello2.exe %gtk-libraries%
gfortran %gtk-fortran-obj% ../examples/cairo-basics.f90 -o cairo-basics.exe %gtk-libraries%
gfortran %gtk-fortran-obj% ../examples/cairo-tests.f90 -o cairo-tests.exe %gtk-libraries%
gfortran %gtk-fortran-obj% ../examples/bazaar.f90 -o bazaar.exe %gtk-libraries%
gfortran %gtk-fortran-obj% ../examples/gtkbuilder.f90 -o gtkbuilder.exe %gtk-libraries%
gfortran %gtk-fortran-obj% ../examples/gtkbuilder2.f90 -o gtkbuilder2.exe %gtk-libraries%
echo.

echo Running the examples...
copy ..\examples\gtkbuilder.glade gtkbuilder.glade
@for %%i in (*.exe) do (
"%%i"
)
