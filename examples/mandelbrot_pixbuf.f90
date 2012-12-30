! Copyright (C) 2011
! Free Software Foundation, Inc.

! This file is part of the gtk-fortran gtk+ Fortran Interface library.

! This is free software; you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 3, or (at your option)
! any later version.

! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! Under Section 7 of GPL version 3, you are granted additional
! permissions described in the GCC Runtime Library Exception, version
! 3.1, as published by the Free Software Foundation.

! You should have received a copy of the GNU General Public License along with
! this program; see the files COPYING3 and COPYING.RUNTIME respectively.
! If not, see <http://www.gnu.org/licenses/>.
!
! gfortran -g gtk.f90 mandelbrot_pixbuf.f90 `pkg-config --cflags --libs gtk+-2.0`
! Contributed by Jerry DeLisle and Vincent Magnin

module handlers
  use gtk, only: gtk_container_add, gtk_drawing_area_new, gtk_events_pending, gtk&
  &_main, gtk_main_iteration, gtk_main_iteration_do, gtk_widget_get_window, gtk_w&
  &idget_queue_draw, gtk_widget_show, gtk_window_new, gtk_window_set_default, gtk&
  &_window_set_default_size, gtk_window_set_title, GDK_COLORSPACE_RGB,&
  &gtk_init, g_signal_connect, FALSE, TRUE, c_null_ptr, c_null_char, GTK_WINDOW_TOPLEVEL
  
  use cairo, only: cairo_create, cairo_destroy, cairo_paint, cairo_set_source
  
  use gdk, only: gdk_cairo_create, gdk_cairo_set_source_pixbuf
  
  use gdk_pixbuf, only: gdk_pixbuf_get_n_channels, gdk_pixbuf_get_pixels, gdk_pix&
  &buf_get_rowstride, gdk_pixbuf_new
  
  use iso_c_binding, only: c_int, c_ptr, c_char
  
  implicit none
  integer(c_int) :: run_status = TRUE
  integer(c_int) :: boolresult
  type(c_ptr) :: my_pixbuf
  character(kind=c_char), dimension(:), pointer :: pixel
  integer(kind=c_int) :: nch, rowstride, width, height
  
contains
  ! User defined event handlers go here
  function delete_event (widget, event, gdata) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, event, gdata

    run_status = FALSE
    ret = FALSE
  end function delete_event


  subroutine pending_events ()
    do while(IAND(gtk_events_pending(), run_status) /= FALSE)
      boolresult = gtk_main_iteration_do(FALSE) ! False for non-blocking
    end do
  end subroutine pending_events


  function expose_event (widget, event, gdata) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    implicit none    
    integer(c_int)    :: ret
    type(c_ptr), value, intent(in) :: widget, event, gdata
    type(c_ptr) :: my_cairo_context
    
    my_cairo_context = gdk_cairo_create (gtk_widget_get_window(widget))
    call gdk_cairo_set_source_pixbuf(my_cairo_context, my_pixbuf, 0d0, 0d0) 
    call cairo_paint(my_cairo_context)
    call cairo_destroy(my_cairo_context)
    ret = FALSE
  end function expose_event
end module handlers


program mandelbrot
  use iso_c_binding, only: c_ptr, c_funloc, c_f_pointer
  use handlers
  implicit none
  type(c_ptr) :: my_window
  type(c_ptr) :: my_drawing_area
  integer :: i
  
  call gtk_init ()
  
  ! Properties of the main window :
  width = 700
  height = 700

  my_window = gtk_window_new (GTK_WINDOW_TOPLEVEL)
  call gtk_window_set_default_size(my_window, width, height)
  call gtk_window_set_title(my_window, "A tribute to Benoit MANDELBROT (1924-2010)"//c_null_char)
  call g_signal_connect (my_window, "delete-event"//c_null_char, c_funloc(delete_event))
      
  my_drawing_area = gtk_drawing_area_new()
  call g_signal_connect (my_drawing_area, "expose-event"//c_null_char, c_funloc(expose_event))
  call gtk_container_add(my_window, my_drawing_area)
  call gtk_widget_show (my_drawing_area)
  
  call gtk_widget_show (my_window)
  
  ! We create a pixbuffer to store the pixels of the image:
  my_pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, TRUE, 8_c_int, &
       & width, height)
  call c_f_pointer(gdk_pixbuf_get_pixels(my_pixbuf), pixel, (/0/))
  nch = gdk_pixbuf_get_n_channels(my_pixbuf)
  rowstride = gdk_pixbuf_get_rowstride(my_pixbuf)
  
  ! We use char() because we need unsigned integers.
  ! Our pixbuffer has an Alpha channel but is possible to create a pixbuffer
  ! with only Red, Green, Blue. 
  do i=1, width*height*nch, nch
    pixel(i)=char(0)      ! Red
    pixel(i+1)=char(0)    ! Green
    pixel(i+2)=char(0)    ! Blue
    pixel(i+3)=char(255)  ! Opacity (Alpha channel)
  end do

  call Mandelbrot_set(my_drawing_area, -2d0, +1d0, -1.5d0, +1.5d0, 1000_4)

  ! The window stays opened after the computation:
  do
    call pending_events()
    if (run_status == FALSE) exit
    call sleep(1) ! So we don't burn CPU cycles
  end do
  print *, "All done"

end program mandelbrot 


!*********************************************
! A tribute to Benoit MANDELBROT (1924-2010)
! http://en.wikipedia.org/wiki/Mandelbrot_set
!*********************************************
subroutine Mandelbrot_set(my_drawing_area, xmin, xmax, ymin, ymax, itermax)
  ! Whole set: xmin=-2d0, xmax=+1d0, ymin=-1.5d0, ymax=+1.5d0, itermax=1000
  ! Seahorse valley:  around x=-0.743643887037151, y=+0.13182590420533, itermax=5000
  use iso_c_binding
  use handlers
  implicit none

  type(c_ptr) :: my_drawing_area
  integer(4) :: i, j, k, p, itermax
  real(8)    :: x, y, xmin, xmax, ymin, ymax ! coordinates in the complex plane
  complex(8) :: c, z   
  real(8)    :: scx, scy             ! scales
  integer(1) :: red, green, blue     ! rgb color
  real(8) :: system_time, t0, t1
  
  t0=system_time()
  scx = ((xmax-xmin)/width)   ! x scale
  scy = ((ymax-ymin)/height)  ! y scale
  
  do i=0, width-1
    ! We provoke an expose_event once in a while to improve performances:
    if (mod(i,10_c_int)==0) then
      call gtk_widget_queue_draw(my_drawing_area)
    end if
    
    x = xmin + scx * i
    do j=0, height-1
      y = ymin + scy * j
      c = x + y*(0d0,1d0)   ! Starting point
      z = (0d0, 0d0)        ! z0
      k = 1
      do while ((k <= itermax) .and. ((real(z)**2+aimag(z)**2)<4d0)) 
        z = z*z+c
        k = k+1
      end do
      
      if (k>itermax) then
        ! Black pixel:
        red   = 0
        green = 0
        blue  = 0
      else
        red   = min(255, k*2)
        green = min(255, k*5)
        blue  = min(255, k*10)
      end if
      
      ! We write in the pixbuffer:
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

  t1=system_time()
  print *, "System time = ", t1-t0
end subroutine mandelbrot_set

!***********************************************************
!  system time since 00:00
!***********************************************************
real(8) function system_time()   
  implicit none
  integer, dimension(8) :: dt
  
  call date_and_time(values=dt)
  system_time=dt(5)*3600d0+dt(6)*60d0+dt(7)+dt(8)*0.001d0
end function system_time
