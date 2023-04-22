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
!
! Contributed by Jerry DeLisle and Vincent Magnin
! Last modification: vmagnin 2023-04-23

module handlers
  use, intrinsic :: iso_c_binding

  use gtk, only: gtk_application_window_new, gtk_drawing_area_new, &
  & gtk_drawing_area_set_content_width, gtk_drawing_area_set_content_height, &
  & gtk_drawing_area_set_draw_func, gtk_window_set_child, gtk_widget_show, &
  & gtk_window_set_default_size, gtk_window_set_title,&
  & FALSE, CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL

  use cairo, only: cairo_arc, cairo_create, cairo_curve_to, cairo_destroy, &
  & cairo_get_target, cairo_line_to, cairo_move_to, cairo_new_sub_path, &
  & cairo_select_font_face, cairo_set_font_size, cairo_set_line_width, &
  & cairo_set_source, cairo_set_source_rgb, cairo_show_text, cairo_stroke, &
  & cairo_surface_write_to_png

  implicit none

contains
  ! The GUI is defined here:
  subroutine activate(app, gdata) bind(c)
    type(c_ptr), value, intent(in)  :: app, gdata
    type(c_ptr)     :: window
    type(c_ptr)     :: my_drawing_area
    integer(c_int)  :: width, height

    window = gtk_application_window_new(app)
    width  = 700
    height = 700
    call gtk_window_set_default_size(window, width, height)
    call gtk_window_set_title(window, "Cairo basics demo"//c_null_char)

    my_drawing_area = gtk_drawing_area_new()
    call gtk_drawing_area_set_content_width(my_drawing_area, width)
    call gtk_drawing_area_set_content_height(my_drawing_area, height)
    call gtk_drawing_area_set_draw_func(my_drawing_area, &
                   & c_funloc(my_draw_function), c_null_ptr, c_null_funptr)

    call gtk_window_set_child(window, my_drawing_area)
    call gtk_widget_show(window)
  end subroutine activate

  ! "It is called whenever GTK needs to draw the contents of the drawing area
  ! to the screen."
  ! https://developer.gnome.org/gtk4/stable/GtkDrawingArea.html#gtk-drawing-area-set-draw-func
  subroutine my_draw_function(widget, my_cairo_context, width, height, gdata) bind(c)
    use, intrinsic :: iso_fortran_env, only: wp=>real64

    type(c_ptr), value, intent(in)    :: widget, my_cairo_context, gdata
    integer(c_int), value, intent(in) :: width, height
    integer                           :: cstatus
    integer                           :: t
    real(wp), parameter               :: pi = acos(-1.0_wp)

    ! Bezier curve:
    call cairo_set_source_rgb(my_cairo_context, 0.9d0, 0.8d0, 0.8d0)
    call cairo_set_line_width(my_cairo_context, 4d0)
    call cairo_move_to(my_cairo_context, 0d0, 0d0)
    call cairo_curve_to(my_cairo_context, 600d0, 50d0, 115d0, 545d0, &
                      & width*1d0, height*1d0)
    call cairo_stroke(my_cairo_context)

    ! Lines:
    call cairo_set_source_rgb(my_cairo_context, 0d0, 0.5d0, 0.5d0)
    call cairo_set_line_width(my_cairo_context, 2d0)
    do t = 0, int(height), +20
      call cairo_move_to(my_cairo_context, 0d0, t*1d0)
      call cairo_line_to(my_cairo_context, t*1d0, height*1d0)
      call cairo_stroke(my_cairo_context)
    end do

    ! Text:
    call cairo_set_source_rgb(my_cairo_context, 0d0, 0d0, 1d0)
    call cairo_select_font_face(my_cairo_context, "Times"//c_null_char, &
                            & CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
    call cairo_set_font_size (my_cairo_context, 40d0)
    call cairo_move_to(my_cairo_context, 100d0, 30d0)
    call cairo_show_text (my_cairo_context, "gtk-fortran"//c_null_char)
    call cairo_move_to(my_cairo_context, 100d0, 75d0)
    call cairo_show_text (my_cairo_context, &
                        & "Cairo & Fortran are good friends"//c_null_char)

    ! Circles:
    call cairo_new_sub_path(my_cairo_context)
    do t = 1, 50
        call cairo_set_source_rgb(my_cairo_context, t/50d0, 0d0, 0d0)
        call cairo_set_line_width(my_cairo_context, 5d0*t/50d0)
        call cairo_arc(my_cairo_context, 353d0 + 200d0*cos(t*2d0*pi/50), &
                     & 350d0 + 200d0*sin(t*2d0*pi/50), 50d0, 0d0, 2*pi)
        call cairo_stroke(my_cairo_context)
    end do

    ! Save the image as a PNG:
    cstatus = cairo_surface_write_to_png(cairo_get_target(my_cairo_context), &
                                       & "cairo.png"//c_null_char)
  end subroutine my_draw_function
end module handlers

! We create a GtkApplication:
program cairo_basics
  use, intrinsic :: iso_c_binding
  use gtk, only: gtk_application_new, g_signal_connect, G_APPLICATION_FLAGS_NONE
  use g, only: g_application_run, g_object_unref
  use handlers, only: activate

  implicit none
  integer(c_int) :: exit_status
  type(c_ptr)    :: app

  app = gtk_application_new("gtk-fortran.examples.cairo-basics"//c_null_char, &
                            & G_APPLICATION_FLAGS_NONE)
  call g_signal_connect(app, "activate"//c_null_char, c_funloc(activate), &
                      & c_null_ptr)
  exit_status = g_application_run(app, 0_c_int, [c_null_ptr])
  call g_object_unref(app)
end program cairo_basics

