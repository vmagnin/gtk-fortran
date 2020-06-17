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
! Contributed by: James Tappin
! Last modifications: vmagnin 2020-06-10 (GTK 4)
!------------------------------------------------------------------------------

module cl_handlers

  !********************************
  ! Gtk modules for hl_cairo_clock.f90
  !********************************
  use cairo, only: cairo_arc, cairo_fill, cairo_fill_preserve, cairo_line_to, &
       & cairo_move_to, cairo_new_path, cairo_paint, cairo_rectangle, &
       & cairo_select_font_face, cairo_set_font_size, cairo_set_line_cap, &
       & cairo_set_line_width, cairo_set_source_rgb, cairo_set_source_rgba, &
       & cairo_show_text, cairo_stroke
  use g, only: g_timeout_add, g_main_loop_new, g_main_loop_run, g_main_loop_quit
  use gdk, only: gdk_keyval_from_name
  use gtk, only: gtk_window_set_child, &
       & gtk_window_destroy, gtk_widget_get_allocation, gtk_widget_queue_draw, &
       & gtk_widget_show, gtk_init, TRUE, FALSE, GDK_CONTROL_MASK, &
       & CAIRO_LINE_CAP_ROUND, CAIRO_FONT_SLANT_NORMAL, &
       & CAIRO_FONT_WEIGHT_BOLD
  use gdk_pixbuf_hl
  use gtk_draw_hl
  use gtk_hl_container
  use gdk_events
  use iso_c_binding

  implicit none
  integer(kind=c_int) :: height=250_c_int, width=250_c_int
  real(kind=c_double), parameter :: pi = 3.14159265358979323846_c_double
  integer, dimension(8) :: t0 = 0
  type(c_ptr) :: window
  type(c_ptr) :: my_gmainloop

contains
  function delete_cb (widget, event, gdata) result(ret)  bind(c)
    integer(c_int)    :: ret
    type(c_ptr), value, intent(in) :: widget, event, gdata

    call g_main_loop_quit(my_gmainloop)

    ret = FALSE
  end function delete_cb

  function show_time(area) bind(c)
    integer(kind=c_int) :: show_time
    type(c_ptr), value, intent(in) :: area

    integer, dimension(8) :: dat
    type(c_ptr) :: cr
    character(len=3) :: sdate

    character(len=4), parameter, dimension(12) :: mnames = &
         & (/'JAN'//c_null_char, 'FEB'//c_null_char, 'MAR'//c_null_char, &
         &   'APR'//c_null_char, 'MAY'//c_null_char, 'JUN'//c_null_char, &
         &   'JUL'//c_null_char, 'AUG'//c_null_char, 'SEP'//c_null_char, &
         &   'OCT'//c_null_char, 'NOV'//c_null_char, 'DEC'//c_null_char /)
    integer :: i
    real(kind=c_double) :: r0, r1, x0, x1, y0, y1, th, xc, yc, ycs
    real(kind=c_double) :: xb, xt, yb, yt, radius, scale_factor

    show_time = TRUE

    call date_and_time(values=dat)
    if (all(dat(5:7) == t0(5:7))) return

    t0 = dat

    cr = hl_gtk_drawing_area_cairo_new(area)

    xc = real(width, c_double) / 2._c_double
    yc = real(height, c_double) / 2._c_double
    radius = min(xc, yc)
    scale_factor = radius/125._c_double

    if (height > width) then
       xb = 0._c_double
       xt = real(width, c_double)
       yt = yc - xc
       yb = yc + xc
    else if (height < width) then
       xb = xc - yc
       xt = xc + yc
       yt = 0._c_double
       yb = real(height, c_double)
    else
       xb = 0._c_double
       xt = real(width, c_double)
       yt = 0._c_double
       yb = real(height, c_double)
    end if

    ! Background
    call cairo_set_source_rgb(cr, 0.3_c_double, 0.0_c_double, &
         & 0.0_c_double)
    call cairo_rectangle(cr, 0._c_double, 0._c_double,&
         & real(width, c_double), real(height, c_double))
    call cairo_paint(cr)

    ! Face
    r0 = radius * 0.85_c_double
    call cairo_set_source_rgb(cr, 0.3_c_double, 0.3_c_double, 0._c_double)
    call cairo_new_path(cr)
    call cairo_move_to(cr, xc+r0, yc)
    call cairo_arc(cr, xc, yc, r0, 0._c_double, 2.*pi)
    call cairo_fill(cr)

    ! Sub face
    r0 =  radius * 0.25_c_double
    call cairo_set_source_rgb(cr, 0.2_c_double, 0.7_c_double, 0.7_c_double)
    ycs = yc + 0.375_c_double*radius
    call cairo_new_path(cr)
    call cairo_move_to(cr, xc+r0, ycs)
    call cairo_arc(cr, xc, ycs, r0, 0._c_double, 2.*pi)
    call cairo_fill(cr)

    ! Clock dials
    ! Main
    call cairo_set_source_rgb(cr, 1._c_double, 1._c_double, &
         & 1._c_double)
    call cairo_set_line_cap (cr, CAIRO_LINE_CAP_ROUND)
    do i = 1, 60
       if (mod(i,15) == 0) then
          call cairo_set_line_width(cr, 5._c_double)
          r0 = radius * 0.75_c_double
          r1 = radius * 0.9_c_double
       else if (mod(i,5) == 0) then
          call cairo_set_line_width(cr, 4._c_double)
          r0 = radius * 0.8_c_double
          r1 = radius * 0.9_c_double
       else
          call cairo_set_line_width(cr, 2._c_double)
          r0 = radius * 0.8_c_double
          r1 = radius * 0.85_c_double
       end if
       th = real(i,c_double)*pi/30._c_double

       x0 = sin(th)*r0+xc
       x1 = sin(th)*r1+xc
       y0 = cos(th)*r0+yc
       y1 = cos(th)*r1+yc

       call cairo_move_to(cr, x0, y0)
       call cairo_line_to(cr, x1, y1)
       call cairo_stroke(cr)
    end do

    ! Seconds
    do i = 1, 60
       if (mod(i,15) == 0) then
          call cairo_set_line_width(cr, 2._c_double)
          r0 = radius * 0.2_c_double
          r1 = radius * 0.275_c_double
       else if (mod(i,5) == 0) then
          call cairo_set_line_width(cr, 1._c_double)
          r0 = radius * 0.225_c_double
          r1 = radius * 0.275_c_double
       else
          call cairo_set_line_width(cr, 1._c_double)
          r0 = radius * 0.225_c_double
          r1 = radius * 0.25_c_double
       end if
       th = real(i,c_double)*pi/30._c_double

       x0 = sin(th)*r0+xc
       x1 = sin(th)*r1+xc
       y0 = cos(th)*r0+ycs
       y1 = cos(th)*r1+ycs

       call cairo_move_to(cr, x0, y0)
       call cairo_line_to(cr, x1, y1)
       call cairo_stroke(cr)
    end do

    !  Date
    if (dat(5) >= 12) then
       call cairo_set_source_rgb(cr, 0._c_double, 0._c_double, 0._c_double)
    else
       call cairo_set_source_rgb(cr, 1._c_double, 1._c_double, 1._c_double)
    end if
    call cairo_select_font_face(cr, "sans-serif"//c_null_char, &
         & CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD)
    x0 = xc - 0.6_c_double*radius
    call cairo_set_line_width(cr, 1._c_double)
    call cairo_rectangle(cr, x0-2.*scale_factor, yc+10.*scale_factor, &
         & 42._c_double*scale_factor,-16._c_double*scale_factor)
    call cairo_fill_preserve(cr)
    if (dat(5) < 12) then
       call cairo_set_source_rgb(cr, 0._c_double, 0._c_double, 0._c_double)
    else
       call cairo_set_source_rgb(cr, 1._c_double, 1._c_double, 1._c_double)
    end if
    call cairo_stroke(cr)
    call cairo_set_font_size (cr, 12._c_double*scale_factor)
    write(sdate,"(I2.2,a1)") dat(3), char(0)
    call cairo_move_to(cr, x0,yc+6.*scale_factor)
    call cairo_show_text(cr, sdate)
    call cairo_set_font_size (cr, 9._c_double*scale_factor)
    call cairo_show_text(cr, ' '//mnames(dat(2)))

    ! Second hand
    ! Trail
    th = real(dat(7),c_double) * pi / 30._c_double - pi/2._c_double
    r0 = radius * 0.24_c_double
    do i = 1, 15
       call cairo_set_source_rgba(cr, 1._c_double, 0.1_c_double, &
            & 1._c_double, 1._c_double-real(i,c_double)/15._c_double)
       call cairo_new_path(cr)
       call cairo_move_to(cr, xc, ycs)
       call cairo_arc(cr, xc, ycs, r0, th-pi/30._c_double, th)
       call cairo_fill(cr)
       th = th-pi/30._c_double
    end do

    ! Hand
    call cairo_set_source_rgb(cr, .6_c_double, 0.1_c_double, &
         & .6_c_double)
    call cairo_set_line_width(cr, 2._c_double)

    th = real(dat(7),c_double) * pi / 30._c_double
    x1 = r0*sin(th) + xc
    x0 = xc
    y1 = -r0*cos(th) + ycs
    y0 = ycs

    call cairo_move_to(cr, x0, y0)
    call cairo_line_to(cr, x1, y1)
    call cairo_stroke(cr)

    ! Hour hand
    call cairo_set_source_rgb(cr, 0.1_c_double, 0.8_c_double, &
         & 1._c_double)
    call cairo_set_line_width(cr, 8._c_double)
    r0 = radius * 0.6_c_double
    th = (real(mod(dat(5),12),c_double) + &
         & real(dat(6),c_double)/60._c_double + &
         & real(dat(7), c_double)/3600._c_double) * pi / 6._c_double
    x1 = r0*sin(th) + xc
    x0 = -r0*sin(th)/10._c_double + xc
    y1 = -r0*cos(th) + yc
    y0 = r0*cos(th)/10._c_double + yc

    call cairo_move_to(cr, x0, y0)
    call cairo_line_to(cr, x1, y1)
    call cairo_stroke(cr)

    ! Minute hand
    call cairo_set_source_rgba(cr, 1.0_c_double, 1._c_double, &
         & 0.1_c_double, 0.9_c_double)
    call cairo_set_line_width(cr, 3._c_double)
    r0 = min(xc,yc) * 0.85_c_double

    th = (real(dat(6),c_double) + &
         & real(dat(7),c_double)/60._c_double) * pi / 30._c_double
    x1 = r0*sin(th) + xc
    x0 = -r0*sin(th)/10._c_double + xc
    y1 = -r0*cos(th) + yc
    y0 = r0*cos(th)/10._c_double + yc

    call cairo_move_to(cr, x0, y0)
    call cairo_line_to(cr, x1, y1)
    call cairo_stroke(cr)

    call hl_gtk_drawing_area_cairo_destroy(cr)
    call gtk_widget_queue_draw(area)
  end function show_time

  subroutine clock_resize(area, data) bind(c)
    type(c_ptr), value :: area, data

    type(gtkallocation), target:: alloc
    integer(kind=c_int) :: irv

    call gtk_widget_get_allocation(area,c_loc(alloc))
    width = alloc%width
    height = alloc%height

    call hl_gtk_drawing_area_resize(area)

    t0(:) = 0
    irv = show_time(area)

  end subroutine clock_resize

  function clock_key(widget, event, data) bind(c) result(rv)
    integer(kind=c_int) :: rv
    type(c_ptr), value :: widget, event, data

    integer(kind=c_int) :: key_q
    type(GdkEventKey), pointer :: fevent

    key_q = gdk_keyval_from_name("q"//c_null_char)
    call c_f_pointer(event, fevent)

    if (fevent%keyval == key_q .and. fevent%state == GDK_CONTROL_MASK) then
       call gtk_window_destroy(window)
       call g_main_loop_quit(my_gmainloop)
       rv = TRUE
    else
       rv = FALSE
    end if
  end function clock_key
end module cl_handlers

program cairo_clock
  use cl_handlers

  implicit none
  integer(kind=c_int) :: icont, timeid
  type(c_ptr) :: drawing

  call gtk_init()

  window = hl_gtk_window_new("Cairo Clock"//c_null_char, &
       & destroy = c_funloc(delete_cb), wsize=(/width, height/))

  drawing = hl_gtk_drawing_area_new(has_alpha = TRUE, &
       & size_allocate=c_funloc(clock_resize), &
       & key_press_event=c_funloc(clock_key))

  call gtk_window_set_child(window, drawing)
  call gtk_widget_show(window)

  icont =  show_time(drawing)

  timeid = g_timeout_add(300_c_int, c_funloc(show_time), drawing)

  my_gmainloop = g_main_loop_new(c_null_ptr, FALSE)
  call g_main_loop_run(my_gmainloop)

  print *, "All done"
end program cairo_clock
