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
  & gtk_window_set_default_size, gtk_window_set_title, CAIRO_SVG_VERSION_1_2, &
  & FALSE, CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL

  use cairo, only: cairo_arc, cairo_create, cairo_curve_to, cairo_destroy, &
  & cairo_get_target, cairo_line_to, cairo_move_to, cairo_new_sub_path, &
  & cairo_select_font_face, cairo_set_font_size, cairo_set_line_width, &
  & cairo_set_source, cairo_set_source_rgb, cairo_show_text, cairo_stroke, &
  & cairo_surface_write_to_png, cairo_svg_surface_create, &
  & cairo_svg_surface_restrict_to_version, cairo_surface_destroy, &
  & cairo_pdf_surface_create

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
  ! https://docs.gtk.org/gtk4/class.DrawingArea.html
  subroutine my_draw_function(widget, my_cairo_context, width, height, gdata) bind(c)
    use, intrinsic :: iso_c_binding, only: dp=>c_double
    type(c_ptr), value, intent(in)    :: widget, my_cairo_context, gdata
    integer(c_int), value, intent(in) :: width, height
    integer :: cstatus
    integer :: rendering
    type(c_ptr) :: surface_svg, surface_pdf, cr_svg, cr_pdf

    ! We will draw three times, once for screen, once in a SVG file, once in a PDF:
    do rendering = 1, 3
      if (rendering == 1) then
        ! Rendering on screen:
        call draw(my_cairo_context, width, height)
        ! Save the image as a PNG:
        cstatus = cairo_surface_write_to_png(cairo_get_target(my_cairo_context), &
                                          & "cairo-basics.png"//c_null_char)
        call cairo_destroy(my_cairo_context)
        print *, "Saved in cairo-basics.png"
      else if (rendering == 2) then
        ! Rendering in a SVG file:
        surface_svg = cairo_svg_surface_create("cairo-basics.svg"//c_null_char, &
                                  & real(width, KIND=dp), real(height, KIND=dp))
        cr_svg = cairo_create(surface_svg)
        call cairo_svg_surface_restrict_to_version(surface_svg, CAIRO_SVG_VERSION_1_2)
        call draw(cr_svg, width, height)
        call cairo_destroy(cr_svg)
        call cairo_surface_destroy(surface_svg)
        print *, "Saved in cairo-basics.svg"
      else
        ! Rendering in a PDF file:
        surface_pdf = cairo_pdf_surface_create("cairo-basics.pdf"//c_null_char, &
                                  & real(width, KIND=dp), real(height, KIND=dp))
        cr_pdf = cairo_create(surface_pdf)
        call draw(cr_pdf, width, height)
        call cairo_surface_destroy(surface_pdf)
        print *, "Saved in cairo-basics.pdf"
      end if
    end do
  end subroutine my_draw_function

  ! It will be called two time, for screen and SVG file:
  subroutine draw(cr, width, height)
    use, intrinsic :: iso_c_binding, only: dp=>c_double

    type(c_ptr), value, intent(in)    :: cr
    integer(c_int), value, intent(in) :: width, height
    integer                           :: t
    real(dp), parameter               :: pi = acos(-1.0_dp)

    ! Bezier curve:
    call cairo_set_source_rgb(cr, 0.9_dp, 0.8_dp, 0.8_dp)
    call cairo_set_line_width(cr, 4._dp)
    call cairo_move_to(cr, 0._dp, 0._dp)
    call cairo_curve_to(cr, 600._dp, 50._dp, 115._dp, 545._dp, &
                      & width*1._dp, height*1._dp)
    call cairo_stroke(cr)

    ! Lines:
    call cairo_set_source_rgb(cr, 0._dp, 0.5_dp, 0.5_dp)
    call cairo_set_line_width(cr, 2._dp)
    do t = 0, height, +20
      call cairo_move_to(cr, 0._dp, t*1._dp)
      call cairo_line_to(cr, t*1._dp, height*1._dp)
      call cairo_stroke(cr)
    end do

    ! Text:
    call cairo_set_source_rgb(cr, 0._dp, 0._dp, 1._dp)
    call cairo_select_font_face(cr, "Times"//c_null_char, &
                            & CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
    call cairo_set_font_size (cr, 40._dp)
    call cairo_move_to(cr, 100._dp, 30._dp)
    call cairo_show_text (cr, "gtk-fortran"//c_null_char)
    call cairo_move_to(cr, 100._dp, 75._dp)
    call cairo_show_text (cr, &
                        & "Cairo & Fortran are good friends"//c_null_char)

    ! Circles:
    call cairo_new_sub_path(cr)
    do t = 1, 50
        call cairo_set_source_rgb(cr, t/50._dp, 0._dp, 0._dp)
        call cairo_set_line_width(cr, 5._dp*t/50._dp)
        call cairo_arc(cr, 353._dp + 200._dp*cos(t*2._dp*pi/50), &
                     & 350._dp + 200._dp*sin(t*2._dp*pi/50), 50._dp, 0._dp, 2*pi)
        call cairo_stroke(cr)
    end do
  end subroutine draw

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

