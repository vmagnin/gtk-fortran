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
!
! Contributed by Jerry DeLisle and Vincent Magnin
! Last modification: vmagnin 2022-04-05

module handlers
  use gtk, only: gtk_window_set_child, gtk_drawing_area_new, &
  & gtk_drawing_area_set_content_width, gtk_drawing_area_set_content_height, &
  & gtk_drawing_area_set_draw_func, &
  & gtk_widget_queue_draw, gtk_widget_show, gtk_window_new, &
  & gtk_window_set_default_size, gtk_window_set_title, &
  & GDK_COLORSPACE_RGB, gtk_init, g_signal_connect, FALSE, TRUE

  use cairo, only: cairo_create, cairo_destroy, cairo_paint, cairo_set_source

  use gdk, only: gdk_cairo_set_source_pixbuf

  use g, only: g_main_loop_new, g_main_loop_run, g_main_context_iteration, &
             & g_main_context_pending, g_main_loop_quit

  use gdk_pixbuf, only: gdk_pixbuf_get_n_channels, gdk_pixbuf_get_pixels, &
                      & gdk_pixbuf_get_rowstride, gdk_pixbuf_new
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_char, c_null_ptr, c_null_char

  implicit none
  type(c_ptr)    :: my_gmainloop
  ! run_status is TRUE until the user closes the top window:
  integer(c_int) :: run_status = TRUE
  integer(c_int) :: boolresult
  type(c_ptr)    :: my_pixbuf
  character(kind=c_char), dimension(:), pointer :: pixel
  integer(c_int) :: nch, rowstride, width, height
  logical :: computing = .false.

contains
  ! Our callback function before destroying the window:
  subroutine destroy_signal(widget, event, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, event, gdata

    print *, "Your destroy_signal() function has been invoked !"
    ! Some functions and subroutines need to know that it's finished:
    run_status = FALSE
    ! Makes the innermost invocation of the main loop return when it regains control:
    if (.not. computing)   call g_main_loop_quit(my_gmainloop)
  end subroutine destroy_signal

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

    ! We redraw the pixbuf:
    call gdk_cairo_set_source_pixbuf(my_cairo_context, my_pixbuf, 0d0, 0d0)
    call cairo_paint(my_cairo_context)
  end subroutine my_draw_function

end module handlers


module scientific_computation
  use, intrinsic :: iso_fortran_env, only: wp=>real64, int8

  implicit none

contains
  !*********************************************
  ! A tribute to Benoit MANDELBROT (1924-2010)
  ! http://en.wikipedia.org/wiki/Mandelbrot_set
  !*********************************************
  subroutine Mandelbrot_set(my_drawing_area, xmin, xmax, ymin, ymax, itermax)
    ! Whole set: xmin=-2.0_wp, xmax=+1.0_wp, ymin=-1.5_wp, ymax=+1.5_wp, itermax=1000
    ! Seahorse valley:  around x=-0.743643887037151, y=+0.13182590420533, itermax=5000
    use handlers

    type(c_ptr)   :: my_drawing_area
    integer       :: i, j, k, p, itermax
    real(wp)      :: x, y, xmin, xmax, ymin, ymax ! coordinates in the complex plane
    complex(wp)   :: c, z
    real(wp)      :: scx, scy             ! scales
    integer(int8) :: red, green, blue     ! rgb color
    real(wp)      :: t0, t1

    computing = .true.
    call cpu_time(t0)
    scx = (xmax-xmin) / width   ! x scale
    scy = (ymax-ymin) / height  ! y scale

    do i=0, width-1
      ! **************************************************************************
      ! Needed if you want to display progressively the result during computation.
      ! We provoke a draw event only once in a while to avoid degrading
      ! the performances:
      ! **************************************************************************
      if (mod(i, 10_c_int) == 0) then
        call gtk_widget_queue_draw(my_drawing_area)
      end if

      x = xmin + scx * i
      do j=0, height-1
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
          ! Colour palette:
          red   = int(min(255, k*2),  int8)
          green = int(min(255, k*5),  int8)
          blue  = int(min(255, k*10), int8)
        end if

        ! We write in the pixbuffer, using char() because we need unsigned integers:
        p = i * nch + j * rowstride + 1
        pixel(p)   = char(red)
        pixel(p+1) = char(green)
        pixel(p+2) = char(blue)

      end do
      ! **************************************************************************
      ! You need to manage the GTK events during computation:
      ! **************************************************************************
      call pending_events()
      if (run_status == FALSE) return ! Exit if we had a destroy signal.
    end do

    ! Final update of the display:
    call gtk_widget_queue_draw(my_drawing_area)

    call cpu_time(t1)
    print '(A, F6.2, A)', "CPU time = ", t1-t0, " s"

    computing = .false.
  end subroutine mandelbrot_set
end module scientific_computation


!***********************************************
! We define the GUI and then call the main loop:
!***********************************************
program mandelbrot
  use, intrinsic :: iso_c_binding, only: c_ptr, c_null_funptr, c_funloc, c_f_pointer
  use handlers
  use scientific_computation

  implicit none
  type(c_ptr) :: my_window
  type(c_ptr) :: my_drawing_area

  call gtk_init()

  ! Properties of the main window :
  width  = 700
  height = 700
  my_window = gtk_window_new()
  call gtk_window_set_default_size(my_window, width, height)
  call gtk_window_set_title(my_window, &
                    & "A tribute to Benoit MANDELBROT (1924-2010)"//c_null_char)
  call g_signal_connect(my_window, "destroy"//c_null_char, &
                      & c_funloc(destroy_signal))

  ! We need a widget where to draw our pixbuf:
  my_drawing_area = gtk_drawing_area_new()
  call gtk_drawing_area_set_content_width(my_drawing_area, width)
  call gtk_drawing_area_set_content_height(my_drawing_area, height)
  call gtk_drawing_area_set_draw_func(my_drawing_area, &
                   & c_funloc(my_draw_function), c_null_ptr, c_null_funptr)

  call gtk_window_set_child(my_window, my_drawing_area)

  call gtk_widget_show(my_window)

  ! We create a pixbuffer to store the pixels of the image:
  ! "Creates a new GdkPixbuf structure and allocates a buffer for it":
  ! RGB, no alpha channel (FALSE), 8 bits per color sample, width, height
  my_pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8_c_int, width, height)
  ! Queries the number of channels of a pixbuf:
  nch = gdk_pixbuf_get_n_channels(my_pixbuf)
  print *, "Number of channels of the pixbuf: ", nch
  ! "Queries the rowstride of a pixbuf, which is the number of bytes between
  ! the start of a row and the start of the next row":
  rowstride = gdk_pixbuf_get_rowstride(my_pixbuf)
  print *, "Rowstride of the pixbuf: ", rowstride

  ! We need a pointer toward the pixel buffer:
  call c_f_pointer(gdk_pixbuf_get_pixels(my_pixbuf), pixel, [width*height*nch])

  ! Scientific computing:
  call Mandelbrot_set(my_drawing_area, -2.0_wp, +1.0_wp, -1.5_wp, +1.5_wp, 10000_4)

  ! The window will stay opened after the computation, but we need to verify
  ! that the user has not closed the window during the computation.
  ! https://developer.gnome.org/glib/stable/glib-The-Main-Event-Loop.html
  if (run_status /= FALSE) then
    my_gmainloop = g_main_loop_new(c_null_ptr, FALSE)
    call g_main_loop_run(my_gmainloop)
  end if

  print *, "All done"
end program mandelbrot
