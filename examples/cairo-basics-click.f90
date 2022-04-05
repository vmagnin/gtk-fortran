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
! GTK 4 version contributed by Vincent Magnin
! Last modification: vmagnin 2020-05-28, 2022-04-05

module handlers
  use, intrinsic :: iso_fortran_env, only: wp=>real64
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_null_ptr, c_null_funptr, &
                         & c_funloc, c_null_char, c_double, c_bool

  use gtk, only: gtk_application_window_new, gtk_drawing_area_new, &
  & gtk_drawing_area_set_content_width, gtk_drawing_area_set_content_height, &
  & gtk_drawing_area_set_draw_func, gtk_window_set_child, gtk_widget_show, &
  & gtk_window_set_default_size, gtk_window_set_title, FALSE, &
  & gtk_gesture_click_new, gtk_widget_add_controller, &
  & gtk_event_controller_get_widget, g_signal_connect, &
  & gtk_widget_get_width, gtk_widget_get_height, &
  & gtk_gesture_single_get_current_button, gtk_gesture_single_set_button, &
  & gtk_event_controller_scroll_new, GTK_EVENT_CONTROLLER_SCROLL_VERTICAL, &
  & gtk_widget_queue_draw

  use cairo, only: cairo_arc, cairo_line_to, cairo_move_to, &
  & cairo_set_line_width, cairo_set_source_rgb, cairo_stroke, &
  & cairo_get_target, cairo_surface_write_to_png

  implicit none
  ! Circle radius:
  real(wp) :: radius = 100.0_wp

contains
  ! The GUI is defined here:
  subroutine activate(app, gdata) bind(c)
    type(c_ptr), value, intent(in)  :: app, gdata
    type(c_ptr)     :: window
    type(c_ptr)     :: my_drawing_area, controller, controller2
    integer(c_int)  :: width, height

    window = gtk_application_window_new(app)
    width  = 700
    height = 700
    call gtk_window_set_default_size(window, width, height)
    call gtk_window_set_title(window, "Click or scroll and I will tell you..."//c_null_char)

    my_drawing_area = gtk_drawing_area_new()
    call gtk_drawing_area_set_content_width(my_drawing_area, width)
    call gtk_drawing_area_set_content_height(my_drawing_area, height)
    call gtk_drawing_area_set_draw_func(my_drawing_area, &
                   & c_funloc(my_draw_function), c_null_ptr, c_null_funptr)

    ! We need a gesture controller to detect mouse clicks:
    ! https://developer.gnome.org/gtk4/stable/GtkGestureClick.html
    ! https://developer.gnome.org/gtk4/stable/GtkWidget.html#gtk-widget-add-controller
    controller = gtk_gesture_click_new()
    ! 0 to listen to all buttons (button 1 by default):
    call gtk_gesture_single_set_button (controller, 0_c_int)
    call g_signal_connect(controller, "pressed"//c_null_char, &
                                        & c_funloc(click_cb))
    call gtk_widget_add_controller(my_drawing_area, controller)

    ! And a controller for scrolling (modifies the circle radius):
    ! https://developer.gnome.org/gtk4/stable/GtkEventControllerScroll.html
    controller2 = gtk_event_controller_scroll_new (GTK_EVENT_CONTROLLER_SCROLL_VERTICAL)
    call g_signal_connect(controller2, "scroll"//c_null_char, &
                                        & c_funloc(scroll_cb))
    call gtk_widget_add_controller(my_drawing_area, controller2)

    call gtk_window_set_child(window, my_drawing_area)
    call gtk_widget_show(window)
  end subroutine activate

  ! Click callback function ("pressed" signal):
  subroutine click_cb(gesture, n_press, x, y, gdata) bind(c)
    type(c_ptr), value, intent(in)    :: gesture, gdata
    integer(c_int), value, intent(in) :: n_press
    real(c_double), value, intent(in) :: x, y
    type(c_ptr) :: widget
    integer(c_int) :: width, height
    real(wp) :: d

    print *, "Button ", gtk_gesture_single_get_current_button(gesture)

    widget = gtk_event_controller_get_widget(gesture)
    print *, n_press, " click(s) at ", int(x), int(y)

    width  = gtk_widget_get_width(widget)
    height = gtk_widget_get_height(widget)
    d = sqrt((x - width/2.0_wp)**2 + (y - height/2.0_wp)**2)
    print *, "Distance from the centre of the circle: ", d
  end subroutine click_cb

  ! Scroll callback function ("scroll" signal):
  subroutine scroll_cb(controller, x, y, gdata) bind(c)
    type(c_ptr), value, intent(in)    :: controller, gdata
    real(c_double), value, intent(in) :: x, y
    type(c_ptr) :: widget

    radius = radius + y
    print *, "Scroll and new circle radius: ", x, y, radius
    ! We need to redraw the area:
    widget = gtk_event_controller_get_widget(controller)
    call gtk_widget_queue_draw(widget);
  end subroutine scroll_cb

  ! "It is called whenever GTK needs to draw the contents of the drawing area
  ! to the screen."
  ! https://developer.gnome.org/gtk4/stable/GtkDrawingArea.html#gtk-drawing-area-set-draw-func
  subroutine my_draw_function(widget, my_cairo_context, width, height, gdata) bind(c)
    type(c_ptr), value, intent(in)    :: widget, my_cairo_context, gdata
    integer(c_int), value, intent(in) :: width, height
    integer(c_int)                    :: cstatus
    integer                           :: t
    real(wp), parameter               :: pi = acos(-1.0_wp)

    print *, "draw"

    ! https://cairographics.org/manual/
    ! Note that Cairo numerical parameters are generally doubles.

    ! Horizontal and vertical thin blue lines:
    call cairo_set_source_rgb(my_cairo_context, 0d0, 0d0, 1d0)
    call cairo_set_line_width(my_cairo_context, 1d0)

    do t = 0, height, +100
      ! https://cairographics.org/manual/cairo-Paths.html#cairo-move-to
      call cairo_move_to(my_cairo_context, 0d0,       t*1d0)
      call cairo_line_to(my_cairo_context, width*1d0, t*1d0)
      call cairo_stroke(my_cairo_context)
    end do

    do t = 0, width, +100
      call cairo_move_to(my_cairo_context, t*1d0, 0d0)
      call cairo_line_to(my_cairo_context, t*1d0, height*1d0)
      call cairo_stroke(my_cairo_context)
    end do

    ! A thick red circle at the centre:
    call cairo_set_source_rgb(my_cairo_context, 1d0, 0d0, 0d0)
    call cairo_set_line_width(my_cairo_context, 5d0)
    call cairo_arc(my_cairo_context, width/2d0, height/2d0, radius, 0d0, 2*pi)
    call cairo_stroke(my_cairo_context)

    ! Save the image as a PNG
    ! https://cairographics.org/manual/cairo-PNG-Support.html#cairo-surface-write-to-png
    cstatus = cairo_surface_write_to_png(cairo_get_target(my_cairo_context), &
                                       & "cairo-basics-click.png"//c_null_char)
  end subroutine my_draw_function
end module handlers

! We create a GtkApplication:
program cairo_basics_click
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_funloc, c_null_char, c_null_ptr
  use gtk, only: gtk_application_new, g_signal_connect, G_APPLICATION_FLAGS_NONE
  use g, only: g_application_run, g_object_unref
  use handlers, only: activate

  implicit none
  integer(c_int) :: exit_status
  type(c_ptr)    :: app

  app = gtk_application_new("gtk-fortran.examples.cairo-basics-click"//c_null_char, &
                            & G_APPLICATION_FLAGS_NONE)
  call g_signal_connect(app, "activate"//c_null_char, c_funloc(activate), &
                      & c_null_ptr)
  exit_status = g_application_run(app, 0_c_int, [c_null_ptr])
  call g_object_unref(app)
end program cairo_basics_click

