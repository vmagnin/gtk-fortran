! $ gfortran -Wall -Wextra -std=f2008 -pedantic -g my_first_gtk_app5.f90 $(pkg-config --cflags --libs gtk-3-fortran)

module math
  use, intrinsic :: iso_c_binding, only: dp=>c_double
  implicit none

  contains

  pure real(dp) function iterate(x0, r) result(x)
    real(dp), intent(in) :: x0, r
    integer  :: i

    x = x0
    do i = 0, 20000
      x = r * x * (1_dp - x)
    end do
  end function iterate
end module math

module handlers
  use, intrinsic :: iso_c_binding
  use gtk, only: gtk_application_new, G_APPLICATION_FLAGS_NONE, &
               & gtk_application_window_new, gtk_widget_show_all, &
               & gtk_window_set_title, g_signal_connect, &
               & gtk_box_new, gtk_container_add, gtk_button_new_with_label, &
               & GTK_ORIENTATION_VERTICAL, FALSE,&
               & gtk_label_new, gtk_spin_button_new, gtk_adjustment_new, &
               & gtk_spin_button_get_value
  use g, only: g_application_run, g_object_unref
  use math

  implicit none
  type(c_ptr) r_spin_button

  contains

  subroutine my_button_clicked(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata
    real(dp) :: r, x0

    call random_number(x0)
    r = gtk_spin_button_get_value(r_spin_button)
    print *, r, x0, iterate(x0, r)
  end subroutine

  subroutine activate(app, gdata) bind(c)
    type(c_ptr), value, intent(in) :: app, gdata
    type(c_ptr) :: window
    type(c_ptr) :: box, my_button
    type(c_ptr) :: label_r

    window = gtk_application_window_new(app)
    call gtk_window_set_title(window, "Hello world!"//c_null_char)

    box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10_c_int)
    call gtk_container_add(window, box)

    label_r = gtk_label_new("r parameter"//c_null_char)
    call gtk_container_add(box, label_r)

    r_spin_button = gtk_spin_button_new(gtk_adjustment_new(3._dp, 0._dp, 4._dp, &
                                    & 0.1_dp, 0._dp, 0._dp), 0.0_dp, 15_c_int)
    call gtk_container_add(box, r_spin_button)

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
  status = g_application_run(app, 0_c_int, c_null_ptr)
  call g_object_unref(app)
end program
