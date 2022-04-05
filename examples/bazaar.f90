! Copyright (C) 2011
! Free Software Foundation, Inc.
!
! This file is part of the gtk-fortran GTK Fortran Interface library.
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
! This program is used to test various GTK widgets and functions
! Contributors: Vincent Magnin, James Tappin
! GTK 4 version: vmagnin 2020-05-28, 2021-01-28, 2022-04-05
!------------------------------------------------------------------------------

module various_functions
  use, intrinsic :: iso_c_binding
  use gtk_sup, only: c_f_string_copy
  implicit none

  contains

  subroutine some_glib_functions()
    use g, only: g_get_user_name, g_get_application_name, g_get_host_name, &
               & g_get_home_dir, g_get_current_dir, g_format_size, &
               & g_get_os_info

    character(len=512) :: my_string

    call c_f_string_copy(g_get_user_name(), my_string)
    print *, "Hello ", TRIM(my_string)

    call c_f_string_copy(g_get_host_name(), my_string)
    print *, "Host name: ", TRIM(my_string)

    call c_f_string_copy(g_get_application_name(), my_string)
    print *, "Application name: ", TRIM(my_string)

    call c_f_string_copy(g_get_home_dir(), my_string)
    print *, "Home dir: ", TRIM(my_string)

    call c_f_string_copy(g_get_current_dir(), my_string)
    print *, "Current dir: ", TRIM(my_string)
    if (my_string(1:1) == "/") then
        print *, "UNIX OS"
    else
        print *, "Not UNIX OS"
    endif

    call c_f_string_copy(g_get_os_info("PRETTY_NAME"//c_null_char), my_string)
    print *, "Your OS:", my_string

    call c_f_string_copy(g_format_size (123456789_c_int64_t), my_string)
    print *, "g_format_size: ", TRIM(my_string)
  end subroutine some_glib_functions
end module various_functions


module my_widgets
  use, intrinsic :: iso_c_binding

  implicit none
  type(c_ptr) :: window
  type(c_ptr) :: box1, table
  type(c_ptr) :: button1, button2, button3, button4, label1
  type(c_ptr) :: entry1
  type(c_ptr) :: progress
  type(c_ptr) :: view, buffer, scrolled_window
  type(c_ptr) :: my_drawing_area, my_pixbuf
  type(c_ptr) :: dialog
end module


module handlers
  use, intrinsic :: iso_c_binding, only: c_null_ptr, c_null_char

  use gtk, only: gtk_about_dialog_new, gtk_about_dialog_set_authors, &
  & gtk_about_dialog_set_comments, &
  & gtk_about_dialog_set_license_type, GTK_LICENSE_GPL_3_0, &
  & gtk_about_dialog_set_program_name, gtk_application_window_new, &
  & gtk_about_dialog_set_website, gtk_window_set_transient_for, &
  & gtk_button_new, gtk_button_new_with_label, &
  & gtk_window_set_child, gtk_scrolled_window_set_child, &
  & gtk_drawing_area_new, gtk_drawing_area_set_draw_func, &
  & gtk_entry_get_buffer, gtk_entry_buffer_get_text, gtk_entry_new, &
  & gtk_label_new, &
  & gtk_progress_bar_new, gtk_progress_bar_pulse, &
  & gtk_progress_bar_set_fraction, gtk_progress_bar_set_text, &
  & gtk_scrolled_window_new,&
  & gtk_grid_attach, gtk_grid_new, gtk_text_buffer_set_text,&
  & gtk_text_view_get_buffer, gtk_text_view_new, gtk_window_destroy, &
  & gtk_widget_show, gtk_window_set_title, &
  & g_signal_connect, g_signal_connect_swapped, &
  & FALSE, TRUE, GDK_COLORSPACE_RGB, GDK_COLORSPACE_RGB,&
  & gtk_grid_set_row_homogeneous, &
  & gtk_grid_set_column_homogeneous, &
  & gtk_widget_set_margin_start, gtk_widget_set_margin_end, &
  & gtk_widget_set_margin_top, gtk_widget_set_margin_bottom, &
  & gtk_get_major_version, gtk_get_minor_version, gtk_get_micro_version

  use g, only: g_object_unref

  use cairo, only: cairo_create, cairo_curve_to, cairo_destroy, cairo_line_to, &
  & cairo_move_to, cairo_paint, cairo_set_line_width, cairo_set_source, &
  & cairo_set_source_rgb, cairo_stroke

  use gdk, only: gdk_cairo_set_source_pixbuf

  use gdk_pixbuf, only: gdk_pixbuf_get_has_alpha, gdk_pixbuf_get_n_channels, &
  & gdk_pixbuf_get_pixels, gdk_pixbuf_get_rowstride, gdk_pixbuf_new

  use my_widgets

  implicit none

contains
  !*************************************
  ! User defined event handlers go here
  !*************************************

  ! Callback function for the signal "activate" emitted by g_application_run().
  ! We use a subroutine because it should return void.
  ! The GUI is defined here.
  subroutine activate(app, gdata) bind(c)
    use my_widgets
    use various_functions

    type(c_ptr), value, intent(in)  :: app, gdata

    ! Create the window:
    window = gtk_application_window_new(app)
    ! Don't forget that C strings must end with a null char:
    call gtk_window_set_title(window, "A great bazaar to test widgets..."//&
                                      &c_null_char)

    print '(A4,I0,A1,I0,A1,I0)', "GTK ", gtk_get_major_version(),".", &
         & gtk_get_minor_version(), ".", gtk_get_micro_version()
    call some_glib_functions()

    !******************************************************************
    ! Adding widgets in the window:
    !******************************************************************
    table = gtk_grid_new ()
    call gtk_grid_set_column_homogeneous(table, TRUE)
    call gtk_grid_set_row_homogeneous(table, TRUE)
    ! Set the border width around the container:
    call gtk_widget_set_margin_start (table, 10_c_int)
    call gtk_widget_set_margin_end (table, 10_c_int)
    call gtk_widget_set_margin_top (table, 10_c_int)
    call gtk_widget_set_margin_bottom (table, 10_c_int)
    call gtk_window_set_child(window, table)

    button1 = gtk_button_new_with_label ("Button1"//c_null_char)
    call gtk_grid_attach(table, button1, 0_c_int, 0_c_int, 1_c_int, 1_c_int)
    call g_signal_connect (button1, "clicked"//c_null_char, c_funloc(firstbutton))

    button2 = gtk_button_new_with_label ("Button2"//c_null_char)
    call gtk_grid_attach(table, button2, 1_c_int, 0_c_int, 1_c_int, 1_c_int)
    call g_signal_connect (button2, "clicked"//c_null_char, c_funloc(secondbutton))

    button3 = gtk_button_new_with_label ("Exit"//c_null_char)
    call gtk_grid_attach(table, button3, 2_c_int, 0_c_int, 1_c_int, 1_c_int)
    call g_signal_connect (button3, "clicked"//c_null_char, c_funloc(destroy))

    button4 = gtk_button_new_with_label ("About"//c_null_char)
    call gtk_grid_attach(table, button4, 3_c_int, 0_c_int, 1_c_int, 1_c_int)
    call g_signal_connect (button4, "clicked"//c_null_char, c_funloc(aboutbutton))

    label1 = gtk_label_new("My label"//c_null_char)
    call gtk_grid_attach(table, label1, 0_c_int, 1_c_int, 1_c_int, 1_c_int)

    entry1 = gtk_entry_new()
    call gtk_grid_attach(table, entry1, 1_c_int, 1_c_int, 1_c_int, 1_c_int)

    progress = gtk_progress_bar_new()
    call gtk_progress_bar_set_fraction (progress, 0.15d0)
    call gtk_progress_bar_set_text (progress, "My progress bar"//c_null_char)
    call gtk_grid_attach(table, progress, 1_c_int, 2_c_int, 3_c_int, 1_c_int)

    ! https://developer.gnome.org/gtk4/stable/GtkTextView.html#gtk-text-view-new
    view = gtk_text_view_new()
    buffer = gtk_text_view_get_buffer(view)
    call gtk_text_buffer_set_text(buffer, "This is just a great bazaar"//char(13)// &
        & "where I can test widgets"//c_new_line//"Vincent"//c_new_line//&
        &"You can edit this text. It will be scrollable."//c_null_char, -1_c_int)
    scrolled_window = gtk_scrolled_window_new()
    call gtk_scrolled_window_set_child(scrolled_window, view)
    call gtk_grid_attach(table, scrolled_window, 0_c_int, 3_c_int, 3_c_int, 3_c_int)

    my_drawing_area = gtk_drawing_area_new()
    ! https://docs.gtk.org/gtk4/method.DrawingArea.set_draw_func.html
    call gtk_drawing_area_set_draw_func(my_drawing_area, &
                     & c_funloc(my_draw_function), c_null_ptr, c_null_funptr)
    call gtk_grid_attach(table, my_drawing_area, 0_c_int, 6_c_int, 3_c_int, 6_c_int)

    call gtk_widget_show(window)
  end subroutine activate

  ! "It is called whenever GTK needs to draw the contents of the drawing area
  ! to the screen."
  subroutine my_draw_function(widget, my_cairo_context, width, height, gdata) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_char
    use, intrinsic :: iso_fortran_env, only: wp=>real64

    type(c_ptr), value, intent(in)    :: widget, my_cairo_context, gdata
    integer(c_int), value, intent(in) :: width, height
    type(c_ptr) :: my_pixbuf
    character(kind=c_char), dimension(:), pointer :: pixel
    integer(kind=c_int) :: i, nch, rowstride
    integer :: j
    integer :: x, y

    print *, "my_draw_function()"

    call cairo_set_line_width(my_cairo_context, 1d0)
    call cairo_move_to(my_cairo_context, 100d0, 20d0)
    call cairo_line_to(my_cairo_context, 100.5d0, 20d0)
    call cairo_stroke(my_cairo_context)

    call cairo_set_source_rgb(my_cairo_context, 1d0, 0d0, 0d0)
    call cairo_set_line_width(my_cairo_context, 0.5d0)
    call cairo_move_to(my_cairo_context, 50d0, 0d0)
    call cairo_line_to(my_cairo_context, 150d0, 100d0)
    call cairo_stroke(my_cairo_context)

    call cairo_set_source_rgb(my_cairo_context, 1d0, 0d0, 0d0)
    call cairo_set_line_width(my_cairo_context, 3d0)
    call cairo_move_to(my_cairo_context, 60d0, 0d0)
    call cairo_curve_to(my_cairo_context, 60d0, 50d0, 135d0, 45d0, 100d0, 50d0)
    call cairo_stroke(my_cairo_context)

    !*************
    ! Pixbuffers :
    !*************
    my_pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, TRUE, 8_c_int, width, height)
    nch = gdk_pixbuf_get_n_channels(my_pixbuf)
    rowstride = gdk_pixbuf_get_rowstride(my_pixbuf)
    print *, rowstride, nch, gdk_pixbuf_get_has_alpha(my_pixbuf)
    call c_f_pointer(gdk_pixbuf_get_pixels(my_pixbuf), pixel, [width*height*nch])

    ! pixel is an array with 4 bytes per pixel (RGBA)
    ! We use chars because we need unsigned integers
    !pixel=char(255)   ! All in white and maximum opacity

    do i=1, width*height*nch, nch
      pixel(i)   = char(0)    ! Red
      pixel(i+1) = char(0)    ! Green
      pixel(i+2) = char(255)  ! Blue
      pixel(i+3) = char(200)  ! Opacity (Alpha channel)
    end do

    ! (0,0) is the top left corner
    ! 0<=x<width, 0<=y<height

    ! Green sinus:
    do j=0, width-1
      x = j
      y = height/2 + int(50 * sin(j/10.0_wp))
      i = x * nch + y * rowstride + 1
      pixel(i)   = char(0)
      pixel(i+1) = char(255)
      pixel(i+2) = char(0)
      pixel(i+3) = char(255)
    end do

    ! We redraw the pixbuf:
    call gdk_cairo_set_source_pixbuf(my_cairo_context, my_pixbuf, 20d0, 0d0)

    ! Another Cairo line:
    call cairo_set_line_width(my_cairo_context, 4d0)
    call cairo_move_to(my_cairo_context, 0d0, 0d0)
    call cairo_line_to(my_cairo_context, width*1d0, height*1d0)
    call cairo_stroke(my_cairo_context)

    call cairo_paint(my_cairo_context)
  end subroutine my_draw_function

  ! GtkObject signal:
  subroutine destroy (widget, gdata) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr
    use gtk_sup, only: c_f_string_copy

    type(c_ptr), value, intent(in) :: widget, gdata
    type(c_ptr) :: buffer
    character(len=512) :: my_string

    print *, "my destroy"

    buffer = gtk_entry_get_buffer(entry1)
    call c_f_string_copy(gtk_entry_buffer_get_text(buffer), my_string)
    print *, "Entry box:", TRIM(my_string)

    ! This is the end of the program:
    call gtk_window_destroy(window)
  end subroutine destroy

  ! GtkButton signal:
  subroutine firstbutton (widget, gdata) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr
    type(c_ptr), value, intent(in) :: widget, gdata

    print *, "Hello World!"
  end subroutine firstbutton

  ! GtkButton signal:
  subroutine secondbutton (widget, gdata) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr
    type(c_ptr), value, intent(in) :: widget, gdata

    call gtk_progress_bar_pulse (progress)
  end subroutine secondbutton

  ! GtkButton signal:
  subroutine aboutbutton (widget, gdata) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr
    use gtk_sup, only: f_c_string

    type(c_ptr), value, intent(in) :: widget, gdata
    ! Authors of bazaar.f90 (list returned by git blame bazaar.f90):
    character(len=14, kind=c_char), dimension(2), parameter :: authors = &
                                          & ["Vincent MAGNIN", "James TAPPIN  "]
    character(kind=c_char), dimension(:), allocatable :: string
    character(kind=c_char), pointer, dimension(:) :: credit
    type(c_ptr), dimension(:), allocatable :: c_ptr_array
    integer :: i

    dialog = gtk_about_dialog_new()
    ! https://developer.gnome.org/gtk4/stable/GtkWindow.html#gtk-window-set-transient-for
    call gtk_window_set_transient_for(dialog, window)
    call gtk_about_dialog_set_program_name(dialog, "The gtk-fortran bazaar"//c_null_char)
    call gtk_about_dialog_set_license_type(dialog, GTK_LICENSE_GPL_3_0)
    call gtk_about_dialog_set_comments(dialog, "The gtk-fortran project &
    & aims to offer scientists programming in Fortran a cross-platform library &
    &to build Graphical User Interfaces (GUI)."//c_new_line//" Gtk-fortran&
    & is a partial GTK / Fortran binding 100% written in Fortran, thanks&
    & to the ISO_C_BINDING module for interoperability between C and Fortran,&
    & which is a part of the Fortran 2003 standard."//c_new_line//" GTK &
    &is a free software cross-platform graphical library available for &
    &Linux, UNIX, Windows and MacOs."//c_null_char)
    call gtk_about_dialog_set_website(dialog, &
                  & "https://github.com/vmagnin/gtk-fortran/wiki"//c_null_char)

    ! To add authors we need a pointer toward a null terminated array of strings.
    ! This code comes from src/gtk-hl-dialog.f90:
    allocate(c_ptr_array(size(authors)+1))

    do i = 1, size(authors)
      call f_c_string(authors(i), string)
      allocate(credit(size(string)))
      ! A Fortran pointer toward the Fortran string:
      credit(:) = string(:)
      ! Store the C address in the array:
      c_ptr_array(i) = c_loc(credit(1))
      nullify(credit)
    end do
    ! The array must be null terminated:
    c_ptr_array(size(authors)+1) = c_null_ptr
    ! https://docs.gtk.org/gtk3/method.AboutDialog.set_authors.html
    call gtk_about_dialog_set_authors(dialog, c_ptr_array)
    deallocate(c_ptr_array)

    call gtk_widget_show(dialog)
  end subroutine aboutbutton
end module handlers

!*******************************************************************************
! In the main program, we declare the GTK application, connect it to its
! "activate" function where we will create the GUI,
! and finally call the GLib main loop.
!*******************************************************************************
program bazaar
  use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char, c_null_ptr
  use gtk, only: gtk_application_new, G_APPLICATION_FLAGS_NONE
  use g, only: g_application_run, g_object_unref
  use handlers

  implicit none
  integer(c_int)     :: status
  type(c_ptr)        :: app

  app = gtk_application_new("gtk-fortran.examples.bazaar"//c_null_char, &
                            & G_APPLICATION_FLAGS_NONE)
  call g_signal_connect(app, "activate"//c_null_char, c_funloc(activate), &
                      & c_null_ptr)
  status = g_application_run(app, 0_c_int, [c_null_ptr])
  call g_object_unref(app)
end program bazaar
