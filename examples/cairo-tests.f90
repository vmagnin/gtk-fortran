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
! gfortran -g mandelbrot.f90 `pkg-config --cflags --libs gtk+-2.0`
! Contributed by Jerry DeLisle and Vincent Magnin

module handlers
  use gtk, only: gtk_container_add, gtk_drawing_area_new, gtk_events_pending, gtk&
  &_main, gtk_main_iteration, gtk_main_iteration_do, gtk_widget_get_window, gtk_w&
  &idget_queue_draw, gtk_widget_show, gtk_window_new, gtk_window_set_default, gtk&
  &_window_set_default_size, gtk_window_set_title, TRUE, FALSE, NULL, CNULL, &
  & GTK_WINDOW_TOPLEVEL, GDK_COLORSPACE_RGB, gtk_init, g_signal_connect
  use cairo, only: cairo_arc, cairo_create, cairo_curve_to, cairo_destroy, cairo_&
  &get_target, cairo_line_to, cairo_move_to, cairo_new_sub_path, cairo_paint, cai&
  &ro_select_font_face, cairo_set_font_size, cairo_set_line_width, cairo_set_sour&
  &ce, cairo_set_source_rgb, cairo_show_text, cairo_stroke, cairo_surface_write_t&
  &o_png
  use gdk, only: gdk_cairo_create, gdk_cairo_set_source_pixbuf
  use gdk_pixbuf, only: gdk_pixbuf_get_n_channels, gdk_pixbuf_get_pixels, gdk_pix&
  &buf_get_rowstride, gdk_pixbuf_new
  use iso_c_binding

  implicit none

  integer(c_int) :: run_status = TRUE
  integer(c_int) :: boolresult
  logical :: boolevent
  
  type(c_ptr) :: my_pixbuf
  character(c_char), dimension(:), pointer :: pixel
  integer :: nch, rowstride, width, height
  logical :: finished
  
contains
  ! User defined event handlers go here
  function delete_event (widget, event, gdata) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, event, gdata
    !print *, "Delete_event"
    run_status = FALSE
    ret = FALSE
  end function delete_event

  subroutine pending_events ()
    do while(IAND(gtk_events_pending(), run_status) /= FALSE)
      boolresult = gtk_main_iteration_do(FALSE) ! False for non-blocking
    end do
  end subroutine pending_events

  function expose_event (widget, event, gdata) result(ret)  bind(c)
    use iso_c_binding
    use gtk
    implicit none
    integer(c_int)    :: ret
    type(c_ptr), value, intent(in) :: widget, event, gdata
    type(c_ptr) :: my_cairo_context
    integer :: cstatus
    
    my_cairo_context = gdk_cairo_create (gtk_widget_get_window(widget))
    
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

    call gdk_cairo_set_source_pixbuf(my_cairo_context, my_pixbuf, 700d0/4d0, 700d0/4d0) 
    call cairo_paint(my_cairo_context)

    call cairo_set_source_rgb(my_cairo_context, 1d0, 1d0, 0d0)
    call cairo_set_line_width(my_cairo_context, 2d0)
    call cairo_move_to(my_cairo_context, 0d0, height/2d0)  
    call cairo_line_to(my_cairo_context, 1d0*width, height/2d0)
    call cairo_move_to(my_cairo_context, width/2d0+width/12d0, 0d0)  
    call cairo_line_to(my_cairo_context, width/2d0+width/12d0, height*1d0)
    call cairo_stroke(my_cairo_context) 

    call cairo_select_font_face(my_cairo_context, "Times"//CNULL, CAIRO_FONT_SLANT_NORMAL, &
                                 &  CAIRO_FONT_WEIGHT_NORMAL)
    call cairo_set_font_size (my_cairo_context, 16d0)
    call cairo_move_to(my_cairo_context, 200d0, 200d0)
    call cairo_show_text (my_cairo_context, "Mandelbrot set"//CNULL)

    call cairo_new_sub_path(my_cairo_context)
    call cairo_arc(my_cairo_context, 300d0, 300d0, 100d0, 0d0, 3.14159d0) 
    call cairo_stroke(my_cairo_context) 
    
    if (finished) then
      cstatus = cairo_surface_write_to_png(cairo_get_target(my_cairo_context), "cairo.png"//CNULL)
      print *, cstatus
    end if
    
    call cairo_destroy(my_cairo_context)
    ret = FALSE
  end function expose_event
end module handlers


program mandelbrot
  use iso_c_binding
  use handlers
  implicit none
  type(c_ptr) :: my_window
  type(c_ptr) :: my_drawing_area
  integer :: i
  
  finished = .false.
  call gtk_init ()
  
  ! Properties of the main window :
  width = 700
  height = 700

  my_window = gtk_window_new (GTK_WINDOW_TOPLEVEL)
  call gtk_window_set_default_size(my_window, width, height)
  call gtk_window_set_title(my_window, "A tribute to Benoit MANDELBROT (1924-2010)"//CNULL)
  call g_signal_connect (my_window, "delete-event"//CNULL, c_funloc(delete_event))
      
  my_drawing_area = gtk_drawing_area_new()
  call g_signal_connect (my_drawing_area, "expose-event"//CNULL, c_funloc(expose_event))
  call gtk_container_add(my_window, my_drawing_area)
  call gtk_widget_show (my_drawing_area)

  
  call gtk_widget_show (my_window)
      
  my_pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, TRUE, 8, width/2, height/2)    
  call c_f_pointer(gdk_pixbuf_get_pixels(my_pixbuf), pixel, (/0/))
  nch = gdk_pixbuf_get_n_channels(my_pixbuf)
  rowstride = gdk_pixbuf_get_rowstride(my_pixbuf)
    
  do i=1, (width/2*height/2)*nch, nch
    pixel(i)=char(0)      ! Red
    pixel(i+1)=char(0)    ! Green
    pixel(i+2)=char(0)    ! Blue
    pixel(i+3)=char(255)  ! Opacity (Alpha channel)
  end do

  call Mandelbrot_set(my_drawing_area, -2d0, +1d0, -1.5d0, +1.5d0, 100)
  
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
    scx = ((xmax-xmin)/(width/2))   ! x scale
    scy = ((ymax-ymin)/(height/2))  ! y scale
    
    do i=0, width/2
      ! We provoke an expose_event:
      !if (mod(i,10)==0) then
      if (mod(i,1)==0) then
        call gtk_widget_queue_draw(my_drawing_area)
      end if
      
      x = xmin + scx * i
      do j=0, height/2
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
          red   = min(255, k*2)
          green = min(255, k*5)
          blue  = min(255, k*10)
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
    finished = .true.
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
