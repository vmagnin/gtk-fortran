! This file is part of gtk-fortran, a GTK / Fortran interface library.
! Copyright (C) 2011 The gtk-fortran team
!
! This is free software; you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 3, or (at your option)
! any later version.
!
! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! Under Section 7 of GPL version 3, you are granted additional
! permissions described in the GCC Runtime Library Exception, version
! 3.1, as published by the Free Software Foundation.
!
! You should have received a copy of the GNU General Public License along with
! this program; see the files LICENSE and LICENSE_EXCEPTION respectively.
! If not, see <http://www.gnu.org/licenses/>.
!------------------------------------------------------------------------------
! Contributed by Jerry DeLisle and Vincent Magnin
! GTK 4 version: vmagnin 2020-05-19
! Last modifications: vmagnin 2023-04-25
!------------------------------------------------------------------------------

module handlers
  use gtk, only: gtk_application_window_new, gtk_window_set_child, &
  & gtk_drawing_area_set_content_width, gtk_drawing_area_set_content_height, &
  & gtk_drawing_area_set_draw_func, gtk_drawing_area_new, &
  & gtk_widget_queue_draw, gtk_widget_show, gtk_window_set_default_size, &
  & gtk_window_set_title, gtk_window_set_resizable, &
  & TRUE, FALSE, GDK_COLORSPACE_RGB, g_signal_connect, &
  & CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL

  use cairo, only: cairo_arc, cairo_create, cairo_curve_to, cairo_destroy, &
  & cairo_get_target, cairo_line_to, cairo_move_to, cairo_new_sub_path, &
  & cairo_paint, cairo_select_font_face, cairo_set_font_size, &
  & cairo_set_line_width, cairo_set_source, cairo_set_source_rgb, &
  & cairo_show_text, cairo_stroke, cairo_surface_write_to_png

  use gdk, only: gdk_cairo_set_source_pixbuf

  use gdk_pixbuf, only: gdk_pixbuf_get_n_channels, gdk_pixbuf_get_pixels, &
  & gdk_pixbuf_get_rowstride, gdk_pixbuf_new

  use g, only: g_main_context_iteration, g_main_context_pending

  use, intrinsic :: iso_fortran_env, only: wp=>real64, dp=>real64, int8
  use, intrinsic :: iso_c_binding

  implicit none
  type(c_ptr)    :: my_gmainloop
  integer(c_int) :: run_status = TRUE
  integer(c_int) :: boolresult
  type(c_ptr)    :: my_pixbuf
  character(kind=c_char), dimension(:), pointer :: pixel
  integer(c_int) :: nch, rowstride, width, height, pwidth, pheight
  logical :: write_png
  ! Mathematical window:
  real(wp), parameter :: xmin = -2.0_wp
  real(wp), parameter :: xmax = +1.0_wp
  real(wp), parameter :: ymin = -1.5_wp
  real(wp), parameter :: ymax = +1.5_wp

contains
  ! This function is needed to update the GUI during long computations.
  ! https://docs.gtk.org/glib/main-loop.html
  subroutine pending_events ()
    do while(IAND(g_main_context_pending(c_null_ptr), run_status) /= FALSE)
      ! FALSE for non-blocking:
      boolresult = g_main_context_iteration(c_null_ptr, FALSE)
    end do
  end subroutine pending_events

  ! "It is called whenever GTK needs to draw the contents of the drawing area
  ! to the screen."
  ! https://docs.gtk.org/gtk4/method.DrawingArea.set_draw_func.html
  subroutine my_draw_function(widget, my_cairo_context, width, height, gdata) bind(c)
    type(c_ptr), value, intent(in)    :: widget, my_cairo_context, gdata
    integer(c_int), value, intent(in) :: width, height
    integer :: cstatus, i
    real(dp), parameter :: pi = acos(-1._dp)
    real(dp) :: Ox, Oy, X_circle
    real(dp) :: X_cardio, teta, ro

    print *, "Entering my_draw_function()"

    ! Pixel coordinates of the mathematical origin:
    Ox = width  * (0._dp - xmin) / (xmax - xmin)
    Oy = height * (ymax - 0._dp) / (ymax - ymin)

    ! We draw the Mandelbrot set pixbuf in the Cairo context, starting from
    ! the top left corner:
    call gdk_cairo_set_source_pixbuf(my_cairo_context, my_pixbuf, 0._dp, 0._dp)
    call cairo_paint(my_cairo_context)

    ! Text:
    call cairo_select_font_face(my_cairo_context, "Arial"//c_null_char, &
                            & CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)

    ! Draw cartesian axes, in white:
    call cairo_set_source_rgb(my_cairo_context, 1._dp, 1._dp, 1._dp)
    call cairo_set_line_width(my_cairo_context, 2._dp)
    ! Horizontal axis:
    call cairo_move_to(my_cairo_context, 0._dp,                Oy)
    call cairo_line_to(my_cairo_context, real(width, KIND=dp), Oy)
    ! Vertical axis:
    call cairo_move_to(my_cairo_context, Ox, 0._dp)
    call cairo_line_to(my_cairo_context, Ox, real(height, KIND=dp))
    call cairo_stroke(my_cairo_context)
    ! Texts on the axes:
    call cairo_set_font_size (my_cairo_context, 20._dp)
    call cairo_move_to(my_cairo_context, width - 20._wp, Oy - 10._dp)
    call cairo_show_text (my_cairo_context, "x"//c_null_char)
    call cairo_move_to(my_cairo_context, Ox - 20._wp, +20._dp)
    call cairo_show_text (my_cairo_context, "y"//c_null_char)
    call cairo_move_to(my_cairo_context, Ox - 20._wp, Oy + 20._dp)
    call cairo_show_text (my_cairo_context, "O"//c_null_char)
    call cairo_set_font_size (my_cairo_context, 32._dp)
    call cairo_move_to(my_cairo_context, width / 10._dp, height / 10._dp)
    call cairo_show_text (my_cairo_context, "Mandelbrot set"//c_null_char)

    ! Circle of radius 1/4 centered around −1, in yellow:
    call cairo_set_source_rgb(my_cairo_context, 1._dp, 1._dp, 0._dp)
    call cairo_set_line_width(my_cairo_context, 3._dp)
    call cairo_new_sub_path(my_cairo_context)
    X_circle = width  * (-1._dp - xmin) / (xmax - xmin)
    call cairo_arc(my_cairo_context, X_circle, Oy, width / (xmax-xmin) / 4._dp, 0._dp, 2._dp*pi)
    call cairo_stroke(my_cairo_context)
    call cairo_set_font_size (my_cairo_context, 20._dp)
    call cairo_move_to(my_cairo_context, X_circle, Oy - 10._dp)
    call cairo_show_text (my_cairo_context, "circle"//c_null_char)

    ! Main cardioid centered on A(1/4, 0) with polar equation ro(teta) = 1/2*(1 − cos teta)
    X_cardio = width  * (1._dp/4._dp - xmin) / (xmax - xmin)
    call cairo_move_to(my_cairo_context, X_cardio, Oy)
    do i = 1, 100
      teta = (i * 2._dp * pi) / 100._dp
      ro = (1._dp - cos(teta)) / 2._dp
      ro = ro * width / (xmax - xmin)
      call cairo_line_to(my_cairo_context, X_cardio + ro*cos(teta), Oy + ro*sin(teta))
    end do
    call cairo_stroke(my_cairo_context)
    call cairo_move_to(my_cairo_context, Ox + 5._dp, Oy - 10._dp)
    call cairo_show_text (my_cairo_context, "cardioid"//c_null_char)

    ! The image is written to PNG only one time:
    if (write_png) then
      cstatus = cairo_surface_write_to_png(cairo_get_target(my_cairo_context),&
                                         & "cairo-tests.png"//c_null_char)
      print *, "Writing cairo-tests.png: ", cstatus
      write_png = .false.
    end if
  end subroutine my_draw_function

  ! The GUI is defined here:
  subroutine activate(app, gdata) bind(c)
    type(c_ptr), value, intent(in)  :: app, gdata
    type(c_ptr)     :: my_window
    type(c_ptr)     :: my_drawing_area
    integer(c_int)  :: width, height
    integer         :: bytes

    ! Properties of the main window:
    my_window = gtk_application_window_new(app)
    width  = 748
    height = 748
    call gtk_window_set_default_size(my_window, width, height)
    call gtk_window_set_resizable(my_window, FALSE)
    call gtk_window_set_title(my_window, "Cairo tests: mixing vector graphics with a pixbuf (gtk-fortran)"//c_null_char)

    my_drawing_area = gtk_drawing_area_new()
    call gtk_drawing_area_set_content_width(my_drawing_area, width)
    call gtk_drawing_area_set_content_height(my_drawing_area, height)
    call gtk_drawing_area_set_draw_func(my_drawing_area, &
                        & c_funloc(my_draw_function), c_null_ptr, c_null_funptr)

    ! Dimensions of the Mandelbrot set picture:
    pwidth  = width
    pheight = height
    ! "Creates a new GdkPixbuf structure and allocates a buffer for it":
    ! RGB, no alpha channel (FALSE), 8 bits per color sample, width, height
    my_pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8_c_int, pwidth, pheight)
    ! Queries the number of channels of a pixbuf:
    nch = gdk_pixbuf_get_n_channels(my_pixbuf)
    print *, "Number of channels of the pixbuf: ", nch
    ! "Queries the rowstride of a pixbuf, which is the number of bytes between
    ! the start of a row and the start of the next row":
    rowstride = gdk_pixbuf_get_rowstride(my_pixbuf)
    print *, "Rowstride of the pixbuf: ", rowstride
    bytes = pwidth * pheight * nch
    print *, "Size (bytes) of the pixbuf: ", bytes

    call c_f_pointer(gdk_pixbuf_get_pixels(my_pixbuf), pixel, [bytes])
    ! Drawing the whole set:
    call Mandelbrot_set(my_drawing_area, 1000_4)
    write_png = .true.

    call gtk_window_set_child(my_window, my_drawing_area)

    call gtk_widget_show(my_window)
  end subroutine activate

  !*********************************************
  ! A tribute to Benoit MANDELBROT (1924-2010)
  ! http://en.wikipedia.org/wiki/Mandelbrot_set
  !*********************************************
  subroutine Mandelbrot_set(my_drawing_area, itermax)
    type(c_ptr)   :: my_drawing_area
    integer       :: i, j, k, p, itermax
    real(wp)      :: x, y    ! coordinates in the complex plane
    complex(wp)   :: c, z
    real(wp)      :: scx, scy             ! scales
    integer(int8) :: red, green, blue     ! rgb color
    real(wp)      :: t0, t1

    print *, "Entering Mandelbrot_set() subroutine"

    call cpu_time(t0)
    scx = (xmax - xmin) / pwidth   ! x scale
    scy = (ymax - ymin) / pheight  ! y scale

    do i = 0, pwidth-1
      x = xmin + scx * i

      do j = 0, pheight-1
        y = ymin + scy * j

        c = cmplx(x, y, kind=wp)    ! Starting point
        z = (0.0_wp, 0.0_wp)        ! z0
        k = 1
        do while ((k <= itermax) .and. ((z%re**2 + z%im**2) < 4.0_wp))
          z = z*z + c
          k = k + 1
        end do

        if (k > itermax) then
          ! Black pixel:
          red   = 0
          green = 0
          blue  = 0
        else
          red   = int(min(255, k*2),  int8)
          green = int(min(255, k*5),  int8)
          blue  = int(min(255, k*10), int8)
        end if

        p = i * nch + j * rowstride + 1
        pixel(p)   = char(red)
        pixel(p+1) = char(green)
        pixel(p+2) = char(blue)
      end do
      ! This subroutine processes GTK events as needed during the computation
      ! (not really useful in that fast computation example)
      call pending_events()
      if (run_status == FALSE) return   ! Exit if we had a delete event
    end do

    ! We only draw the image at the end of that fast computation:
    call gtk_widget_queue_draw(my_drawing_area)

    call cpu_time(t1)
    print '(A, F6.2, A)', "System time = ", t1-t0, " s"
  end subroutine mandelbrot_set

end module handlers

!***********************************************************
! We create a GtkApplication:
!***********************************************************
program cairo_tests
  use, intrinsic :: iso_c_binding
  use gtk, only: gtk_application_new, g_signal_connect, G_APPLICATION_FLAGS_NONE
  use g, only: g_application_run, g_object_unref
  use handlers, only: activate

  implicit none
  integer(c_int) :: exit_status
  type(c_ptr)    :: app

  app = gtk_application_new("gtk-fortran.examples.cairo-tests"//c_null_char, &
                          & G_APPLICATION_FLAGS_NONE)
  call g_signal_connect(app, "activate"//c_null_char, c_funloc(activate), &
                      & c_null_ptr)
  exit_status = g_application_run(app, 0_c_int, [c_null_ptr])
  call g_object_unref(app)
end program cairo_tests
