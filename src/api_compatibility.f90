! This gtk_os_dependent module is present only for API compatibility,
! as the mswindowsonly-auto.f90 and unixonly-auto.f90 files were
! removed in GTK 4.2.0 (in 2022). These functions are now declared
! in the gdk_pixbuf module (gdk-pixbuf-auto.f90 file).

! This file is part of the gtk-fortran library, distributed under
! GNU General Public License version 3.

module gtk_os_dependent
  use gdk_pixbuf, only: gdk_pixbuf_new_from_file, gdk_pixbuf_new_from_file_at_size, &
                        gdk_pixbuf_new_from_file_at_scale, gdk_pixbuf_savev
end module gtk_os_dependent

