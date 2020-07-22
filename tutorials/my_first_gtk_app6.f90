! $ gfortran -Wall -Wextra -std=f2008 -pedantic -g my_first_gtk_app6.f90 $(pkg-config --cflags --libs gtk-3-fortran)

module math
  use iso_c_binding, only: dp=>c_double
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
  use iso_c_binding
  use gtk, only: gtk_application_new, G_APPLICATION_FLAGS_NONE, &
               & gtk_application_window_new, gtk_widget_show_all, &
               & gtk_window_set_title, g_signal_connect, &
               & gtk_box_new, gtk_container_add, gtk_button_new_with_label, &
               & GTK_ORIENTATION_VERTICAL, FALSE,&
               & gtk_label_new, gtk_spin_button_new, gtk_adjustment_new, &
               & gtk_spin_button_get_value, gtk_drawing_area_new, &
               & gtk_widget_queue_draw, GDK_COLORSPACE_RGB, gtk_widget_set_vexpand, &
               & TRUE, gtk_widget_set_size_request
  use g, only: g_application_run, g_object_unref
  use cairo, only: cairo_paint
  use gdk, only: gdk_cairo_set_source_pixbuf
  use gdk_pixbuf, only: gdk_pixbuf_new, gdk_pixbuf_get_rowstride, &
                      & gdk_pixbuf_get_pixels, gdk_pixbuf_get_n_channels
  use math

  implicit none
  type(c_ptr) r_spin_button, my_drawing_area, my_pixbuf
  integer(kind=c_int) :: nch, rowstride, width, height, pixwidth, pixheight
  character(kind=c_char), dimension(:), pointer :: pixel

  contains

  subroutine my_button_clicked(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata
    real(dp) :: r, x0

    call random_number(x0)
    r = gtk_spin_button_get_value(r_spin_button)
    print *, r, x0, iterate(x0, r)
  end subroutine

  function draw(widget, my_cairo_context, gdata) result(ret) bind(c)
    type(c_ptr), value, intent(in) :: widget, my_cairo_context, gdata
    integer(c_int)                 :: ret

    call gdk_cairo_set_source_pixbuf(my_cairo_context, my_pixbuf, 0d0, 0d0)
    call cairo_paint(my_cairo_context)
    ret = FALSE
  end function draw

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

    my_drawing_area = gtk_drawing_area_new()
    pixwidth  = 1000
    pixheight = 600
    call gtk_widget_set_size_request(my_drawing_area, pixwidth, pixheight)
    call g_signal_connect (my_drawing_area, "draw"//c_null_char, c_funloc(draw))
    call gtk_container_add(box, my_drawing_area)

    my_pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8_c_int, &
              & pixwidth, pixheight)
    nch = gdk_pixbuf_get_n_channels(my_pixbuf)
    rowstride = gdk_pixbuf_get_rowstride(my_pixbuf)
    call c_f_pointer(gdk_pixbuf_get_pixels(my_pixbuf), pixel, &
                   & (/pixwidth*pixheight*nch/))
    pixel = char(0)

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
