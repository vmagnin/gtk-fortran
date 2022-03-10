! $ gfortran -Wall -Wextra -std=f2008 -pedantic -g my_first_gtk_app4.f90 $(pkg-config --cflags --libs gtk-3-fortran)

module handlers
  use, intrinsic :: iso_c_binding
  use gtk, only: gtk_application_new, G_APPLICATION_FLAGS_NONE, &
               & gtk_application_window_new, gtk_widget_show_all, &
               & gtk_window_set_title, g_signal_connect, &
               & gtk_box_new, gtk_container_add, gtk_button_new_with_label, &
               & GTK_ORIENTATION_VERTICAL, FALSE
  use g, only: g_application_run, g_object_unref

  implicit none

  contains

  subroutine my_button_clicked(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata
    print *, "Button clicked!"
  end subroutine

  subroutine activate(app, gdata) bind(c)
    type(c_ptr), value, intent(in) :: app, gdata
    type(c_ptr) :: window
    type(c_ptr) :: box, my_button

    window = gtk_application_window_new(app)
    call gtk_window_set_title(window, "Hello world!"//c_null_char)

    box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10_c_int)
    call gtk_container_add(window, box)
    my_button = gtk_button_new_with_label("Compute"//c_null_char)
    call gtk_container_add(box, my_button)
    call g_signal_connect(my_button, "clicked"//c_null_char, c_funloc(my_button_clicked))

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
