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
! Contributed by James Tappin,
! originally derived from cairo_basics.f90 by Vincent Magnin & Jerry DeLisle

module handlers
  use, intrinsic :: iso_c_binding

  !********************************
  ! Gtk modules for hl_cairo1.f90
  use cairo, only: cairo_arc, cairo_curve_to, cairo_get_target, &
       & cairo_line_to, cairo_move_to, cairo_new_sub_path, cairo_paint, &
       & cairo_rectangle, cairo_select_font_face, cairo_set_font_size, &
       & cairo_set_line_width, cairo_set_source_rgb, cairo_show_text, &
       & cairo_stroke, cairo_surface_write_to_png
  use gdk, only: gdk_device_get_name, gdk_device_get_source, &
       & gdk_event_get_source_device, gdk_keyval_from_name, gdk_keyval_name
  use gtk, only: gtk_container_add, gtk_main, gtk_main_quit, &
       & gtk_widget_queue_draw, gtk_widget_show_all, gtk_init, TRUE, FALSE, &
       & GDK_BUTTON_PRESS, GDK_2BUTTON_PRESS, GDK_BUTTON_RELEASE, &
       & GDK_KEY_PRESS, GDK_ENTER_NOTIFY, GDK_LEAVE_NOTIFY, GDK_CONTROL_MASK, &
       & GDK_POINTER_MOTION_MASK, GDK_BUTTON_MOTION_MASK, &
       & CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL
  use gdk_events
  use gdk_pixbuf_hl
  use gtk_draw_hl
  use gtk_sup
  use gtk_hl

  implicit none
  !  integer(c_int) :: run_status = TRUE
  integer(c_int) :: boolresult
  logical :: boolevent
  integer(kind=c_int) :: width, height

  logical :: rflag = .false.
  integer(kind=c_int) :: xp0, yp0

contains
  ! User defined event handlers go here
  function delete_h (widget, event, gdata) result(ret)  bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, event, gdata

    call gtk_main_quit
    ret = FALSE
  end function delete_h

  function button_event_h(widget, event, gdata) result(rv) bind(c)
    integer(kind=c_int) :: rv
    type(c_ptr), value, intent(in) :: widget, event, gdata

    type(gdkeventbutton), pointer :: bevent
    type(c_ptr) :: hdevice, dcname, pixb
    character(len=64) :: dname, hdname
    integer(kind=c_int) :: xp1, yp1, xo, yo, xs, ys, ipick
    character(len=120), dimension(:), allocatable :: files

    rv = FALSE

    if (c_associated(event)) then
       call c_f_pointer(event,bevent)
    else
       return
    end if

    if (bevent%type == GDK_BUTTON_RELEASE) then
       print *, "Button release detected"
       if (rflag) then
          xp1 = nint(bevent%x)
          yp1 = nint(bevent%y)
          print *, "Corners: ", xp0, yp0, " and ", xp1, yp1

          xo = min(xp0, xp1)
          yo = min(yp0, yp1)
          xs = max(xp0, xp1) - xo + 1
          ys = max(yp0, yp1) - yo + 1
          print *, "Origin:", xo, yo, " Size:", xs, ys
          ipick = hl_gtk_file_chooser_show(files, &
               & filter=["image/png"], initial_file="cairo1.png"//c_null_char, &
               & current=TRUE)
          if (c_f_logical(ipick)) then
             pixb = hl_gtk_drawing_area_get_gdk_pixbuf(widget, &
                  & x0 = xo, y0=yo, xsize=xs, ysize=ys)
             call hl_gdk_pixbuf_save(pixb, trim(files(1)))
          end if
       end if
       rflag = .false.
    else
       print *, "Button press detected"
       print *, "Clicked at:", int(bevent%x), int(bevent%y)
       print *, "Type:", bevent%type
       print *, "State, Button:", bevent%state, bevent%button
       print *, "Root x,y:", int(bevent%x_root), int(bevent%y_root)
       dcname = gdk_device_get_name(bevent%device)
       call c_f_string(dcname, dname)
       hdevice = gdk_event_get_source_device(event)
       dcname = gdk_device_get_name(hdevice)
       call c_f_string(dcname, hdname)
       print *, "Device: ",trim(dname),' (',trim(hdname),') ', &
            & gdk_device_get_source(bevent%device)

       if (bevent%type == GDK_2BUTTON_PRESS .and. &
            & bevent%button == 3) call gtk_main_quit

       if (bevent%type == GDK_BUTTON_PRESS .and. &
            & bevent%button == 1 .and. bevent%state == GDK_CONTROL_MASK) then
          xp0 = nint(bevent%x)
          yp0 = nint(bevent%y)
          rflag = .true.
          print *, "Begin region define"
       end if
    end if
    print *

  end function button_event_h

  function motion_event_h(widget, event, gdata) bind(c) result(rv)
    integer(kind=c_int) :: rv
    type(c_ptr), value, intent(in) :: widget, event, gdata

    type(gdkeventscroll), pointer :: bevent

    if (c_associated(event)) then
       call c_f_pointer(event,bevent)

       write(*, "(2I5,A)", advance='no') int(bevent%x), &
            & int(bevent%y), c_carriage_return
    end if
    rv = FALSE
  end function motion_event_h

  function scroll_event_h(widget, event, gdata) bind(c) result(rv)
    integer(kind=c_int) :: rv
    type(c_ptr), value, intent(in) :: widget, event, gdata

    type(gdkeventscroll), pointer :: bevent
    type(c_ptr) :: hdevice, dcname
    character(len=64) :: dname, hdname

    print *, "Wheel event detected"
    if (c_associated(event)) then
       call c_f_pointer(event,bevent)
       print *, "Clicked at:", int(bevent%x), int(bevent%y)
       print *, "State, direction:", bevent%state, bevent%direction
       print *, "Root x,y:", int(bevent%x_root), int(bevent%y_root)
       dcname = gdk_device_get_name(bevent%device)
       call c_f_string(dcname, dname)
       hdevice = gdk_event_get_source_device(event)
       dcname = gdk_device_get_name(hdevice)
       call c_f_string(dcname, hdname)
       print *, "Device: ",trim(dname),' (',trim(hdname),') ', &
            & gdk_device_get_source(bevent%device)
    end if
    print *
    rv = FALSE
  end function scroll_event_h

  function cross_event_h(widget, event, gdata) bind(c) result(rv)
    integer(kind=c_int) :: rv
    type(c_ptr), value, intent(in) :: widget, event, gdata

    type(gdkeventcrossing), pointer :: bevent

    if (c_associated(event)) then
       call c_f_pointer(event,bevent)
       select case(bevent%type)
       case(GDK_ENTER_NOTIFY)
          print *, "Pointer entered at", int(bevent%x), int(bevent%y)
       case(GDK_LEAVE_NOTIFY)
          print *, "Pointer left at", int(bevent%x), int(bevent%y)
       case default
          print *, "Unknown type", bevent%type
       end select
    end if
    print *
    rv = FALSE
  end function cross_event_h

  function key_event_h(widget, event, gdata) bind(c) result(rv)
    integer(kind=c_int) :: rv
    type(c_ptr), value, intent(in) :: widget, event, gdata

    type(gdkeventkey), pointer :: bevent
    integer(kind=c_int) :: key_q
    character(len=20) :: keyname

    key_q = gdk_keyval_from_name("q"//c_null_char)
    print *, "Key event"
    if (c_associated(event)) then
       call c_f_pointer(event,bevent)
       call convert_c_string(gdk_keyval_name(bevent%keyval), keyname)
       print *, "Code: ",bevent%keyval," Name: ", trim(keyname), &
            & " Modifier: ", bevent%state
       if (bevent%type == GDK_KEY_PRESS .and. &
            & iand(bevent%state, GDK_CONTROL_MASK) /= 0 .and.&
            & bevent%keyval == key_q) call gtk_main_quit
    end if
    rv = FALSE
  end function key_event_h

  subroutine draw_pattern(widget)
    type(c_ptr) :: widget

    real(kind=c_double), parameter :: pi = 3.14159265358979323846_c_double

    type(c_ptr) :: my_cairo_context
    integer :: cstatus
    integer :: t

    my_cairo_context = hl_gtk_drawing_area_cairo_new(widget)
    if (.not. c_associated(my_cairo_context)) then
       print *, "ERROR failed to create cairo context"
       return
    end if

    ! Background
    call cairo_set_source_rgb(my_cairo_context, 0.6_c_double, 0.6_c_double, &
         & 0.6_c_double)
    call cairo_rectangle(my_cairo_context, 0._c_double, 0._c_double,&
         & real(width, c_double), real(height, c_double))
    call cairo_paint(my_cairo_context)

    ! Bezier curve:
    call cairo_set_source_rgb(my_cairo_context, 0.9_c_double, 0.8_c_double, &
         & 0.8_c_double)
    call cairo_set_line_width(my_cairo_context, 4._c_double)
    call cairo_move_to(my_cairo_context, 0._c_double, 0._c_double)  
    call cairo_curve_to(my_cairo_context, 600._c_double, 50._c_double, &
         & 115._c_double, 545._c_double, &
         & real(width, c_double), real(height, c_double))
    call cairo_stroke(my_cairo_context) 

    ! Lines:
    call cairo_set_source_rgb(my_cairo_context, 0._c_double, 0.5_c_double, &
         & 0.5_c_double)
    call cairo_set_line_width(my_cairo_context, 2._c_double)
    do t = 0, int(height), +20
       call cairo_move_to(my_cairo_context, 0._c_double, real(t, c_double))
       call cairo_line_to(my_cairo_context, real(t, c_double), &
            & real(height, c_double))
       call cairo_stroke(my_cairo_context) 
    end do

    ! Text:
    call cairo_set_source_rgb(my_cairo_context, 0._c_double, 0._c_double, &
         & 1._c_double)
    call cairo_select_font_face(my_cairo_context, "Times"//c_null_char, &
         & CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
    call cairo_set_font_size (my_cairo_context, 40._c_double)
    call cairo_move_to(my_cairo_context, 100._c_double, 30._c_double)
    call cairo_show_text (my_cairo_context, "gtk-fortran"//c_null_char)
    call cairo_move_to(my_cairo_context, 100._c_double, 75._c_double)
    call cairo_show_text (my_cairo_context, "Cairo & Fortran are good friends"//c_null_char)

    ! Circles:
    call cairo_new_sub_path(my_cairo_context)
    do t = 1, 50
       call cairo_set_source_rgb(my_cairo_context, t/50._c_double, &
            & 0._c_double, 0._c_double)
       call cairo_set_line_width(my_cairo_context, 5._c_double*t/50._c_double)
       call cairo_arc(my_cairo_context, 353._c_double+ &
            & 200._c_double*cos(t*2_c_double*pi/50), &
            & 350._c_double+200._c_double*sin(t*2._c_double*pi/50.), &
            & 50._c_double, 0._c_double, 2.*pi) 
       call cairo_stroke(my_cairo_context) 
    end do

    ! Save:
    cstatus = cairo_surface_write_to_png(cairo_get_target(my_cairo_context), &
         & "cairo.png"//c_null_char)

    call gtk_widget_queue_draw(widget)
    call hl_gtk_drawing_area_cairo_destroy(my_cairo_context)
  end subroutine draw_pattern

end module handlers


program cairo_basics_click
  use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc
  use handlers
  implicit none
  type(c_ptr) :: my_window
  type(c_ptr) :: my_drawing_area
  type(c_ptr) :: my_scroll_box

  call gtk_init ()
  
  ! Properties of the main window :
  width = 700
  height = 700
  my_window = hl_gtk_window_new("Cairo events demo"//c_null_char, &
       & delete_event = c_funloc(delete_h))
      
  my_drawing_area = hl_gtk_drawing_area_new(&
       & scroll=my_scroll_box, &
       & size = (/width, height /), &
       & ssize = (/ 400_c_int, 300_c_int /), &
       & button_press_event=c_funloc(button_event_h), &
       & button_release_event=c_funloc(button_event_h), &
       & scroll_event=c_funloc(scroll_event_h), &
       & enter_event=c_funloc(cross_event_h), &
       & leave_event=c_funloc(cross_event_h), &
       & key_press_event=c_funloc(key_event_h), &
       & motion_event=c_funloc(motion_event_h), &
       & event_exclude=GDK_POINTER_MOTION_MASK, &
       & event_mask=GDK_BUTTON_MOTION_MASK)

  call gtk_container_add(my_window, my_scroll_box)

  call gtk_widget_show_all (my_window)
  call draw_pattern(my_drawing_area)

  ! The window stays opened after the computation:
  call gtk_main()
  print *, "All done"

end program cairo_basics_click
