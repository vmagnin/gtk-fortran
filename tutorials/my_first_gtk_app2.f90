! $ gfortran -Wall -Wextra -std=f2008 -pedantic -g my_first_gtk_app2.f90 $(pkg-config --cflags --libs gtk-3-fortran)

module handlers
  use, intrinsic :: iso_c_binding
  use gtk, only: gtk_application_new, G_APPLICATION_FLAGS_NONE
  use g, only: g_application_run, g_object_unref

  implicit none

  contains

end module handlers

program my_first_gtk_app
  use handlers

  implicit none
  type(c_ptr)    :: app
  integer(c_int) :: status

  app = gtk_application_new("gtk-fortran.my_first_gtk_app"//c_null_char, &
                          & G_APPLICATION_FLAGS_NONE)
  status = g_application_run(app, 0_c_int, [c_null_ptr])
  call g_object_unref(app)
end program
