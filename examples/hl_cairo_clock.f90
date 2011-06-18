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
! gfortran -g gtk.f90 hl_cairo_clock.f90 `pkg-config --cflags --libs gtk+-2.0`
! Contributed by: James Tappin

module cl_handlers

  use cairo, only: cairo_destroy, cairo_line_to, cairo_move_to, cairo_paint, cair&
       &o_rectangle, cairo_set_line_width, cairo_set_source, cairo_set_source_rgb, cai&
       &ro_stroke, cairo_set_line_cap, cairo_arc, cairo_new_sub_path, &
       & cairo_fill, cairo_close_path, cairo_fill_preserve, cairo_rectangle, &
       & cairo_new_path, cairo_set_source_rgba, cairo_select_font_face, &
       & cairo_show_text,  cairo_set_font_size

  use gtk, only: gtk_container_add, gtk_drawing_area_new, gtk_events_pending, gtk&
       &_main, gtk_main_iteration, gtk_main_iteration_do, gtk_widget_queue_draw, gtk_w&
       &idget_show, gtk_widget_show_all, gtk_window_new, &
       & CAIRO_LINE_CAP_ROUND, CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD, &
       &  TRUE, FALSE, CNULL, gtk_init, g_signal_connect

  use g, only: g_usleep

  use gtk_hl
  use gtk_draw_hl

  use iso_c_binding

  implicit none

  integer(kind=c_int) :: height, width
  integer(kind=c_int) :: run_status = TRUE

  real(kind=c_double), parameter :: pi = 3.14159265358979323846_c_double

contains
  function delete_cb (widget, event, gdata) result(ret)  bind(c)

    integer(c_int)    :: ret
    type(c_ptr), value :: widget, event, gdata

    run_status = FALSE
    ret = FALSE
  end function delete_cb


  subroutine pending_events ()
    integer(kind=c_int) :: boolresult
    do while(IAND(gtk_events_pending(), run_status) /= FALSE)
       boolresult = gtk_main_iteration_do(FALSE) ! False for non-blocking
    end do
  end subroutine pending_events

  subroutine show_time(area, dat)
    type(c_ptr), intent(in) :: area
    integer, intent(in), dimension(:) :: dat

    type(cairo_user_data_key_t) :: key
    type(c_ptr) :: cr, pixbuf
    character(len=3) :: sdate

    character(len=4), parameter, dimension(12) :: mnames = &
         & (/'JAN'//cnull, 'FEB'//cnull, 'MAR'//cnull, 'APR'//cnull, &
         &   'MAY'//cnull, 'JUN'//cnull, 'JUL'//cnull, 'AUG'//cnull, &
         &   'SEP'//cnull, 'OCT'//cnull, 'NOV'//cnull, 'DEC'//cnull /)
    integer :: i
    real(kind=c_double) :: r0, r1, x0, x1, y0, y1, th, xc, yc, ycs

    cr = hl_gtk_pixbuf_cairo_new(area, key)

    xc = real(width, c_double) / 2._c_double
    yc = real(height, c_double) / 2._c_double

    ! Background
    call cairo_set_source_rgb(cr, 0.3_c_double, 0.0_c_double, &
         & 0.0_c_double)
    call cairo_rectangle(cr, 0._c_double, 0._c_double,&
         & real(width, c_double), real(height, c_double))
    call cairo_paint(cr)

    ! Face
    r0 = min(xc,yc) * 0.85_c_double
    call cairo_set_source_rgb(cr, 0.3_c_double, 0.3_c_double, 0._c_double)
    call cairo_new_path(cr)
    call cairo_move_to(cr, xc+r0, yc)
    call cairo_arc(cr, xc, yc, r0, 0._c_double, 2.*pi)
    call cairo_fill(cr)

    ! Sub face
    r0 =  min(xc,yc) * 0.25_c_double
    call cairo_set_source_rgb(cr, 0.2_c_double, 0.7_c_double, 0.7_c_double)
    ycs = 1.375_c_double * yc
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
          r0 = min(xc,yc) * 0.75_c_double
          r1 = min(xc,yc) * 0.9_c_double
       else if (mod(i,5) == 0) then
          call cairo_set_line_width(cr, 4._c_double)
          r0 = min(xc,yc) * 0.8_c_double
          r1 = min(xc,yc) * 0.9_c_double
       else
          call cairo_set_line_width(cr, 2._c_double)
          r0 = min(xc,yc) * 0.8_c_double
          r1 = min(xc,yc) * 0.85_c_double
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
          r0 = min(xc,ycs) * 0.2_c_double
          r1 = min(xc,ycs) * 0.275_c_double
       else if (mod(i,5) == 0) then
          call cairo_set_line_width(cr, 1._c_double)
          r0 = min(xc,ycs) * 0.225_c_double
          r1 = min(xc,ycs) * 0.275_c_double
       else
          call cairo_set_line_width(cr, 1._c_double)
          r0 = min(xc,ycs) * 0.225_c_double
          r1 = min(xc,ycs) * 0.25_c_double
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
    call cairo_select_font_face(cr, "sans-serif"//cnull, &
         & CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD)
    x0 = xc*.4_c_double
    call cairo_set_line_width(cr, 1._c_double)
    call cairo_rectangle(cr, x0-2., yc+10., 42._c_double,-16._c_double)
    call cairo_fill_preserve(cr)
    if (dat(5) < 12) then
       call cairo_set_source_rgb(cr, 0._c_double, 0._c_double, 0._c_double)
    else
       call cairo_set_source_rgb(cr, 1._c_double, 1._c_double, 1._c_double)
    end if
    call cairo_stroke(cr)
    call cairo_set_font_size (cr, 12._c_double)
    write(sdate,"(I2.2,a1)") dat(3), char(0)
    call cairo_move_to(cr, x0,yc+6)
    call cairo_show_text(cr, sdate)
    call cairo_set_font_size (cr, 9._c_double)
    call cairo_show_text(cr, ' '//mnames(dat(2)))


    ! Second hand
    ! Trail
    th = real(dat(7),c_double) * pi / 30._c_double - pi/2._c_double
    r0 = min(xc,yc) * 0.24_c_double
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
    r0 = min(xc,yc) * 0.6_c_double
    th = (real(mod(dat(5),12),c_double) + &
         & real(dat(6),c_double)/60._c_double + &
         & real(dat(7), c_double)/3600._c_double) * pi / 6._c_double
    x1 = r0*sin(th) + xc
    x0 = -r0*sin(th)/10._c_double + xc
    y1 = -r0*cos(th) + xc
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
    y1 = -r0*cos(th) + xc
    y0 = r0*cos(th)/10._c_double + yc

    call cairo_move_to(cr, x0, y0)
    call cairo_line_to(cr, x1, y1)
    call cairo_stroke(cr)

    call hl_gtk_pixbuf_cairo_destroy(cr, key)
    call gtk_widget_queue_draw(area)
  end subroutine show_time

end module cl_handlers

program cairo_clock

  use cl_handlers
  implicit none

  type(c_ptr) :: window, drawing
  integer, dimension(8) :: t0, t1

  height = 250
  width = 250

  call gtk_init()

  window = hl_gtk_window_new("Cairo Clock"//cnull, &
       & delete_event = c_funloc(delete_cb))

  drawing = hl_gtk_drawing_area_new(size=(/width, height/), &
       & has_alpha = TRUE)

  call gtk_container_add(window, drawing)
  call gtk_widget_show_all (window)

  call date_and_time(values=t0)
  call show_time(drawing, t0)

  do
     call pending_events()
     if (run_status == FALSE) exit
     call g_usleep(1000_c_long) ! So we don't burn CPU cycles
     call date_and_time(values=t1)
     if (t1(7) /= t0(7) .or. t1(6) /= t0(6) .or. t1(5) /= t0(5)) then
        t0=t1
        call show_time(drawing, t0)
     end if
  end do
  print *, "All done"
end program cairo_clock
