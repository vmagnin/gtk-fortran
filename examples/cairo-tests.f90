! Copyright (C) 2011
! Free Software Foundation, Inc.
!
! This file is part of the gtk-fortran gtk+ Fortran Interface library.
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
! this program; see the files COPYING3 and COPYING.RUNTIME respectively.
! If not, see <http://www.gnu.org/licenses/>.
!------------------------------------------------------------------------------
! Contributed by Jerry DeLisle and Vincent Magnin
! GTK 4 version: vmagnin 2020-05-19
! Last modification: vmagnin 2020-05-28, 2022-04-05
!------------------------------------------------------------------------------

module handlers
  use gtk, only: gtk_application_window_new, gtk_window_set_child, &
  & gtk_drawing_area_set_content_width, gtk_drawing_area_set_content_height, &
  & gtk_drawing_area_set_draw_func, gtk_drawing_area_new, &
  & gtk_widget_queue_draw, gtk_widget_show, gtk_window_set_default_size, &
  & gtk_window_set_title, TRUE, FALSE, GDK_COLORSPACE_RGB, g_signal_connect, &
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

  use, intrinsic :: iso_c_binding

  implicit none
  type(c_ptr)    :: my_gmainloop
  integer(c_int) :: run_status = TRUE
  integer(c_int) :: boolresult
  type(c_ptr)    :: my_pixbuf
  character(kind=c_char), dimension(:), pointer :: pixel
  integer(kind=c_int) :: nch, rowstride, width, height, pwidth, pheight
  logical :: write_png

contains
  ! This function is needed to update the GUI during long computations.
  ! https://developer.gnome.org/glib/stable/glib-The-Main-Event-Loop.html
  subroutine pending_events ()
    do while(IAND(g_main_context_pending(c_null_ptr), run_status) /= FALSE)
      ! FALSE for non-blocking:
      boolresult = g_main_context_iteration(c_null_ptr, FALSE)
    end do
  end subroutine pending_events

  ! "It is called whenever GTK needs to draw the contents of the drawing area
  ! to the screen."
  ! https://developer.gnome.org/gtk4/stable/GtkDrawingArea.html#gtk-drawing-area-set-draw-func
  subroutine my_draw_function(widget, my_cairo_context, width, height, gdata) bind(c)
    type(c_ptr), value, intent(in)    :: widget, my_cairo_context, gdata
    integer(c_int), value, intent(in) :: width, height
    integer :: cstatus

    print *, "Entering my_draw_function()"

    ! We redraw the Mandelbrot set pixbuf:
    call gdk_cairo_set_source_pixbuf(my_cairo_context, my_pixbuf, &
                                   & 700d0/4d0, 700d0/4d0)
    call cairo_paint(my_cairo_context)

    ! And do some vectorial Cairo drawings above:
    call cairo_set_line_width(my_cairo_context, 1d0)
    call cairo_set_source_rgb(my_cairo_context, 0d0, 0d0, 1d0)
    call cairo_move_to(my_cairo_context, 100d0, 50d0)
    call cairo_line_to(my_cairo_context, 700d0, 700d0)
    call cairo_stroke(my_cairo_context)

    call cairo_set_source_rgb(my_cairo_context, 1d0, 0d0, 0d0)
    call cairo_set_line_width(my_cairo_context, 3d0)
    call cairo_move_to(my_cairo_context, 60d0, 0d0)
    call cairo_curve_to(my_cairo_context, 600d0, 50d0, 135d0, 45d0, 500d0, 500d0)
    call cairo_stroke(my_cairo_context)

    call cairo_set_source_rgb(my_cairo_context, 1d0, 1d0, 0d0)
    call cairo_set_line_width(my_cairo_context, 2d0)
    call cairo_move_to(my_cairo_context, 0d0, height/2d0)
    call cairo_line_to(my_cairo_context, 1d0*width, height/2d0)
    call cairo_move_to(my_cairo_context, width/2d0+width/12d0, 0d0)
    call cairo_line_to(my_cairo_context, width/2d0+width/12d0, height*1d0)
    call cairo_stroke(my_cairo_context)

    call cairo_select_font_face(my_cairo_context, "Times"//c_null_char, &
                   & CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
    call cairo_set_font_size (my_cairo_context, 16d0)
    call cairo_move_to(my_cairo_context, 200d0, 200d0)
    call cairo_show_text (my_cairo_context, "Mandelbrot set"//c_null_char)

    call cairo_new_sub_path(my_cairo_context)
    call cairo_arc(my_cairo_context, 300d0, 300d0, 100d0, 0d0, 3.14159d0)
    call cairo_stroke(my_cairo_context)

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
    integer :: bytes

    ! Properties of the main window:
    my_window = gtk_application_window_new(app)
    width  = 700
    height = 700
    call gtk_window_set_default_size(my_window, width, height)
    call gtk_window_set_title(my_window, "Cairo tests mixing vectorial drawing and pixbuf"//c_null_char)

    my_drawing_area = gtk_drawing_area_new()
    call gtk_drawing_area_set_content_width(my_drawing_area, width)
    call gtk_drawing_area_set_content_height(my_drawing_area, height)
    call gtk_drawing_area_set_draw_func(my_drawing_area, &
                   & c_funloc(my_draw_function), c_null_ptr, c_null_funptr)

    ! Dimensions of the Mandelbrot set picture:
    pwidth  = width  / 2_c_int
    pheight = height / 2_c_int
    ! "Creates a new GdkPixbuf structure and allocates a buffer for it":
    ! RGB, alpha channel (TRUE), 8 bits per color sample, width, height
    my_pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, TRUE, 8_c_int, pwidth, pheight)
    ! Queries the number of channels of a pixbuf:
    nch = gdk_pixbuf_get_n_channels(my_pixbuf)
    print *, "Number of channels of the pixbuf: ", nch
    ! "Queries the rowstride of a pixbuf, which is the number of bytes between
    ! the start of a row and the start of the next row":
    rowstride = gdk_pixbuf_get_rowstride(my_pixbuf)
    print *, "Rowstride of the pixbuf: ", rowstride
    bytes = pwidth*pheight*nch
    print *, "Size (bytes) of the pixbuf: ", bytes

    call c_f_pointer(gdk_pixbuf_get_pixels(my_pixbuf), pixel, [bytes])
    call Mandelbrot_set(my_drawing_area, -2d0, +1d0, -1.5d0, +1.5d0, 100_4)
    write_png = .true.

    call gtk_window_set_child(my_window, my_drawing_area)

    call gtk_widget_show(my_window)
  end subroutine activate

  !*********************************************
  ! A tribute to Benoit MANDELBROT (1924-2010)
  ! http://en.wikipedia.org/wiki/Mandelbrot_set
  !*********************************************
  subroutine Mandelbrot_set(my_drawing_area, xmin, xmax, ymin, ymax, itermax)
    ! Whole set: xmin=-2d0, xmax=+1d0, ymin=-1.5d0, ymax=+1.5d0, itermax=1000
    ! Seahorse valley:  around x=-0.743643887037151, y=+0.13182590420533, itermax=5000
    use, intrinsic :: iso_fortran_env, only: wp=>real64, int8

    type(c_ptr)   :: my_drawing_area
    integer       :: i, j, k, p, itermax
    real(wp)      :: x, y, xmin, xmax, ymin, ymax ! coordinates in the complex plane
    complex(8)    :: c, z
    real(wp)      :: scx, scy             ! scales
    integer(int8) :: red, green, blue     ! rgb color
    real(wp)      :: t0, t1

    print *, "Entering Mandelbrot_set() subroutine"

    call cpu_time(t0)
    scx = (xmax-xmin) / pwidth   ! x scale
    scy = (ymax-ymin) / pheight  ! y scale

    do i=0, pwidth-1
      ! We provoke an expose_event:
      !if (mod(i,10)==0) then
      if (mod(i,1_c_int)==0) then
        call gtk_widget_queue_draw(my_drawing_area)
      end if

      x = xmin + scx * i
      do j=0, pheight-1
        y = ymin + scy * j
        c = x + y*(0d0,1d0)   ! Starting point
        z = (0d0, 0d0)        ! z0
        k = 1
        do while ((k <= itermax) .and. (abs(z)<2d0))
          z = z*z+c
          k = k+1
        end do

        if (k>itermax) then
          ! Black pixel:
          red   = 0
          green = 0
          blue  = 0
        else
          red   = int(min(255, k*2),  KIND=1)
          green = int(min(255, k*5),  KIND=1)
          blue  = int(min(255, k*10), KIND=1)
        end if

        p = i * nch + j * rowstride + 1
        pixel(p)=char(red)
        pixel(p+1)=char(green)
        pixel(p+2)=char(blue)
        pixel(p+3)=char(255)  ! Opacity (alpha channel)

        ! This subrountine processes gtk events as needed during the computation.
        call pending_events()
        if (run_status == FALSE) return ! Exit if we had a delete event.
      end do
    end do

    call gtk_widget_queue_draw(my_drawing_area)

    call cpu_time(t1)
    print '(A, F6.2, A)', "System time = ", t1-t0, " s"
  end subroutine mandelbrot_set
end module handlers

!***********************************************************
! We create a GtkApplication:
!***********************************************************
program cairo_tests
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_funloc, c_null_char, c_null_ptr
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
