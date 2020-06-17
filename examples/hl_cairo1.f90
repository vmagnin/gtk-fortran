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
! Contributed by James Tappin,
! originally derived from cairo_basics.f90 by Vincent Magnin & Jerry DeLisle
! Last modifications: vmagnin 2020-06-16 (GTK 4)
!------------------------------------------------------------------------------

module handlers
  use iso_c_binding

  !********************************
  ! Gtk modules for hl_cairo1.f90
  !********************************
  use cairo, only: cairo_arc, cairo_curve_to, cairo_get_target, &
       & cairo_line_to, cairo_move_to, cairo_new_sub_path, cairo_paint, &
       & cairo_rectangle, cairo_select_font_face, cairo_set_font_size, &
       & cairo_set_line_width, cairo_set_source_rgb, cairo_show_text, &
       & cairo_stroke, cairo_surface_write_to_png
  use gdk, only: gdk_device_get_name, gdk_device_get_n_axes, &
       & gdk_keyval_from_name, gdk_keyval_name
  use gtk, only: gtk_window_set_child, &
       & gtk_widget_queue_draw, gtk_widget_show, gtk_init, TRUE, FALSE, &
       & GDK_BUTTON_PRESS, GDK_BUTTON_RELEASE, &
       & GDK_KEY_PRESS, GDK_ENTER_NOTIFY, GDK_LEAVE_NOTIFY, GDK_CONTROL_MASK, &
       & GDK_POINTER_MOTION_MASK, GDK_BUTTON_MOTION_MASK, &
       & CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL, &
       & gtk_event_controller_get_current_event_device, &
       & gtk_event_controller_get_name
  use g, only: g_main_loop_new, g_main_loop_run, g_main_loop_quit
  use gdk_events

  use gdk_pixbuf_hl
  use gtk_draw_hl
  use gtk_hl_container
  use gtk_sup

  implicit none
  type(c_ptr) :: my_gmainloop
  integer(c_int) :: boolresult
  logical :: boolevent
  integer(kind=c_int) :: width, height
  logical :: rflag = .false.
  integer(kind=c_int) :: xp0, yp0

contains
  ! User defined event handlers go here
  function delete_h (widget, event, gdata) result(ret)  bind(c)
    type(c_ptr), value, intent(in) :: widget, event, gdata
    integer(c_int)    :: ret

    call g_main_loop_quit(my_gmainloop)
    ret = FALSE
  end function delete_h

  ! GTK 4: Click callback function ("pressed" signal):
  subroutine button_event_h(gesture, n_press, x, y, gdata) bind(c)
    type(c_ptr), value, intent(in)    :: gesture, gdata
    integer(c_int), value, intent(in) :: n_press
    real(c_double), value, intent(in) :: x, y
    integer(kind=c_int) :: nb_axes, nb_buttons
    type(c_ptr) :: device, dcname
    character(len=80) :: dname

    print *, "Button ", gtk_gesture_single_get_current_button(gesture)
    print *, n_press, " click(s) at ", int(x), int(y)

    if (n_press > 1) then
      print *, "Multiple click"
    end if

    device = gtk_event_controller_get_current_event_device(gesture)
    dcname = gdk_device_get_name(device)
    call c_f_string(dcname, dname)
    print *, "Device: ",trim(dname)
    
    nb_axes = gdk_device_get_n_axes(device)
    print *, "Number of axes: ", nb_axes
  end subroutine button_event_h

  ! GTK 4: Click callback function ("released" signal):
  subroutine button_release_h(gesture, n_press, x, y, gdata) bind(c)
    type(c_ptr), value, intent(in)    :: gesture, gdata
    integer(c_int), value, intent(in) :: n_press
    real(c_double), value, intent(in) :: x, y

    print *, "Button released: ", gtk_gesture_single_get_current_button(gesture)
  end subroutine button_release_h

  ! GTK 4: Motion callback function ("motion" signal):
  function motion_event_h(controller, x, y, gdata) result(ret) bind(c)
    type(c_ptr), value, intent(in)    :: controller, gdata
    real(c_double), value, intent(in) :: x, y
    logical(c_bool) :: ret

    write(*, "(2I5,A)", advance='no') nint(x), nint(y), c_carriage_return
    ret = .true.
  end function motion_event_h

  ! GTK 4 : Scroll callback function ("scroll" signal):
  function scroll_event_h(controller, x, y, gdata) result(ret) bind(c)
    type(c_ptr), value, intent(in)    :: controller, gdata
    real(c_double), value, intent(in) :: x, y
    logical(c_bool) :: ret

    print *, "Scroll event detected : x,y= ", x, y
    ret = .true.
  end function scroll_event_h

  ! GTK 4 : motion callback function ("enter" signal):
  subroutine enter_event_h(controller, x, y, gdata) bind(c)
    type(c_ptr), value, intent(in)    :: controller, gdata
    real(c_double), value, intent(in) :: x, y

    print *, "Enter event detected : x,y= ", x, y
  end subroutine enter_event_h

  ! GTK 4 : motion callback function ("leave" signal):
  subroutine leave_event_h(controller, gdata) bind(c)
    type(c_ptr), value, intent(in)    :: controller, gdata
    print *, "Leave event detected"
  end subroutine leave_event_h


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
            & bevent%keyval == key_q)  call g_main_loop_quit(my_gmainloop)
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
  use iso_c_binding, only: c_ptr, c_funloc
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
       & button_release_event=c_funloc(button_release_h), &
       & scroll_event=c_funloc(scroll_event_h), &
       & enter_event=c_funloc(enter_event_h), &
       & leave_event=c_funloc(leave_event_h), &
       & key_press_event=c_funloc(key_event_h), &
       & motion_event=c_funloc(motion_event_h), &
       & event_exclude=GDK_POINTER_MOTION_MASK, &
       & event_mask=GDK_BUTTON_MOTION_MASK)

  call gtk_window_set_child(my_window, my_scroll_box)

  call gtk_widget_show(my_window)
  call draw_pattern(my_drawing_area)

  ! The window stays opened after the computation:
  my_gmainloop = g_main_loop_new(c_null_ptr, FALSE)
  call g_main_loop_run(my_gmainloop)
  print *, "All done"

end program cairo_basics_click
