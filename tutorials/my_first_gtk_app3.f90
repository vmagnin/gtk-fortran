! $ gfortran -Wall -Wextra -std=f2008 -pedantic -g my_first_gtk_app3.f90 $(pkg-config --cflags --libs gtk-3-fortran)

module handlers
  use, intrinsic :: iso_c_binding
  use gtk, only: gtk_application_new, G_APPLICATION_FLAGS_NONE, &
               & gtk_application_window_new, gtk_widget_show_all, &
               & gtk_window_set_title, g_signal_connect
  use g, only: g_application_run, g_object_unref

  implicit none

  contains

  subroutine activate(app, gdata) bind(c)
    type(c_ptr), value, intent(in) :: app, gdata
    type(c_ptr) :: window

    window = gtk_application_window_new(app)
    call gtk_window_set_title(window, "Hello world!"//c_null_char)
    call gtk_widget_show_all(window)
  end subroutine activate
end module handlers

program my_first_gtk_app
  use handlers

  implicit none
  type(c_ptr)    :: app
  integer(c_int) :: status

  app = gtk_application_new("gtk-fortran.my_first_gtk_app"//c_null_char, &
                          & G_APPLICATION_FLAGS_NONE)
  call g_signal_connect(app, "activate"//c_null_char, c_funloc(activate), c_null_ptr)
  status = g_application_run(app, 0_c_int, [c_null_ptr])
  call g_object_unref(app)
end program
