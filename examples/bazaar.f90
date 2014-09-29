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

! This program is used to test GTK+ widgets
! Contributors: Vincent Magnin, Jerry DeLisle, Tobias Burnus 
! To compile under Linux:
! gfortran -I../src/ ../src/gtk.o ../src/gtk-sup.o ../src/gtk-hl.o bazaar.f90  `pkg-config --cflags --libs gtk+-2.0`

module my_widgets
  use iso_c_binding
  implicit none

  type(c_ptr) :: window
  type(c_ptr) :: box1, table
  type(c_ptr) :: button1, button2, button3, button4, label1
  type(c_ptr) :: entry1
  type(c_ptr) :: progress, file_selector
  type(c_ptr) :: view, buffer, scrolled_window
  type(c_ptr) :: my_drawing_area, my_pixbuf
  type(c_ptr) :: dialog
end module


module handlers
  use gtk, only: gtk_about_dialog_new, gtk_about_dialog_set_authors, &
  & gtk_about_dialog_set_comments, gtk_about_dialog_set_license, &
  & gtk_about_dialog_set_program_name,&
  & gtk_about_dialog_set_website, gtk_button_new, gtk_button_new_with_label, &
  & gtk_container_add, gtk_container_set_border_width, gtk_dialog_run, gtk_drawing_area_new,&
  & gtk_entry_get_text, gtk_entry_new, gtk_file_chooser_button_new, &
  & gtk_file_chooser_get_file, gtk_file_chooser_get_filename, gtk_label_new, gtk_main, &
  & gtk_main_quit, gtk_progress_bar_new, gtk_progress_bar_pulse, &
  & gtk_progress_bar_set_fraction, gtk_progress_bar_set_text, gtk_scrolled_window_new,&
  & gtk_grid_attach, gtk_grid_new, gtk_text_buffer_set_text,&
  & gtk_text_view_get_buffer, gtk_text_view_new, gtk_widget_destroy, gtk_widget_get_window,&
  & gtk_widget_show, gtk_widget_show_all, gtk_window_new, gtk_window_set_default,&
  & gtk_window_set_default_size, gtk_window_set_title, &
  & g_signal_connect, gtk_init, FALSE, TRUE, c_null_char, GDK_COLORSPACE_RGB, GDK_COLORSPACE_RGB,&
  & GTK_WINDOW_TOPLEVEL, c_null_ptr, gtk_grid_set_row_homogeneous, &
  & gtk_grid_set_column_homogeneous

  use cairo, only: cairo_create, cairo_curve_to, cairo_destroy, cairo_line_to, &
  & cairo_move_to, cairo_paint, cairo_set_line_width, cairo_set_source, &
  & cairo_set_source_rgb, cairo_stroke
  
  use gdk, only: gdk_cairo_create, gdk_cairo_set_source_pixbuf
  
  use gdk_pixbuf, only: gdk_pixbuf_get_has_alpha, gdk_pixbuf_get_n_channels, &
  & gdk_pixbuf_get_pixels, gdk_pixbuf_get_rowstride, gdk_pixbuf_new
  
  use my_widgets
  implicit none
  
contains
  !*************************************
  ! User defined event handlers go here
  !*************************************
  ! Note that events are a special type of signals, coming from the
  ! X Window system. Then callback functions must have an event argument.
  
  ! GtkWidget event:
  function delete_event (widget, event, gdata) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, event, gdata
    print *, "my delete_event"
    ret = FALSE
  end function delete_event


  ! GtkWidget event:
  function expose_event (widget, event, gdata) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int, c_char
    implicit none
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, event, gdata
    type(c_ptr) :: my_cairo_context, my_pixbuf
    character(kind=c_char), dimension(:), pointer :: pixel
    integer :: j
    integer(kind=c_int) :: i, nch, rowstride, width, height
    integer :: x, y
    
    print *, "my expose_event"
    my_cairo_context = gdk_cairo_create (gtk_widget_get_window(widget))
    
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
    width = 200
    height = 100
    my_pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, TRUE, 8_c_int, width, height)
    nch = gdk_pixbuf_get_n_channels(my_pixbuf)
    ! (/ 0 /) can't be right... based on indexing below assume the buffer 
    ! is the size given in the following.
    call C_F_POINTER( gdk_pixbuf_get_pixels(my_pixbuf), pixel,  &
        (/ width*height*nch /) )

    rowstride = gdk_pixbuf_get_rowstride(my_pixbuf)
    print *, rowstride, nch, gdk_pixbuf_get_has_alpha(my_pixbuf)
    
    ! pixel is an array with 4 bytes per pixel (RGBA)
    ! We use chars because we need unsigned integers
    !pixel=char(255)   ! All in white and maximum opacity
    
    do i=1, width*height*nch, nch
      pixel(i)=char(0)      ! Red
      pixel(i+1)=char(0)    ! Green
      pixel(i+2)=char(255)  ! Blue
      pixel(i+3)=char(200)  ! Opacity (Alpha channel)
    end do
    
    ! (0,0) is the top left corner
    ! 0<=x<width, 0<=y<height
    
    ! Green sinus:
    do j=0, width-1
      x=j
      y=height/2 + int(50 * sin(j/10d0))
      
      i = x * nch + y * rowstride + 1
      pixel(i)=char(0)
      pixel(i+1)=char(255)
      pixel(i+2)=char(0)
      pixel(i+3)=char(255)
    end do
    
    call gdk_cairo_set_source_pixbuf(my_cairo_context, my_pixbuf, 20d0, 0d0)
    call cairo_paint(my_cairo_context)
    !************
    call cairo_set_line_width(my_cairo_context, 4d0)
    call cairo_move_to(my_cairo_context, 0d0, 0d0)
    call cairo_line_to(my_cairo_context, 100d0, 50d0)
    call cairo_stroke(my_cairo_context) 

    call cairo_destroy(my_cairo_context)
    ret = FALSE
  end function expose_event


  ! GtkObject signal:
  subroutine destroy (widget, gdata) bind(c)
    use iso_c_binding, only: c_ptr
    use gtk_sup, only: c_f_string_copy
    type(c_ptr), value :: widget, gdata
    character(len=512) :: my_string

    print *, "my destroy"
    
    call c_f_string_copy(gtk_entry_get_text(entry1), my_string)
    print *, "Entry box:", TRIM(my_string)
    
    call gtk_main_quit()
  end subroutine destroy


  ! GtkButton signal:
  function firstbutton (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    
    print *, "Hello World!"
    ret = FALSE
  end function firstbutton

  ! GtkButton signal:
  function secondbutton (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    
    call gtk_progress_bar_pulse (progress)
    ret = FALSE
  end function secondbutton


  ! GtkButton signal:
  function aboutbutton (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    integer(c_int) :: response_id
    
    dialog = gtk_about_dialog_new()
    call gtk_about_dialog_set_program_name(dialog, "Gtk-fortran"//c_null_char)
    call gtk_about_dialog_set_license(dialog, "GNU GPL 3"//c_null_char)
    call gtk_about_dialog_set_comments(dialog, "The gtk-fortran project &
    & aims to offer scientists programming in Fortran a cross-platform library &
    &to build Graphical User Interfaces (GUI)."//c_new_line//" Gtk-fortran&
    & is a partial GTK+ / Fortran binding 100% written in Fortran, thanks&
    & to the ISO_C_BINDING module for interoperability between C and Fortran,&
    & which is a part of the Fortran 2003 standard."//c_new_line//" GTK+ &
    &is a free software cross-platform graphical library available for &
    &Linux, Unix, Windows and MacOs X."//c_null_char)
    call gtk_about_dialog_set_website(dialog, "https://github.com/jerryd/gtk-fortran/wiki"//c_null_char)
    !TODO: to add authors we need a pointer toward null terminated array of strings.
    !call gtk_about_dialog_set_authors(dialog, authors)
    response_id =  gtk_dialog_run(dialog)
    print *, "Dialog response ID:", response_id
    call gtk_widget_destroy(dialog)
    
    ret = FALSE
  end function aboutbutton


  ! GtkFileChooser signal:
  function file_changed (widget, gdata ) result(ret)  bind(c)
    use gtk_sup, only: c_f_string_copy
    use iso_c_binding, only: c_ptr, c_char
    integer(c_int)    :: ret
    character(kind=c_char), dimension(:), pointer :: textptr
    type(c_ptr), value :: widget, gdata
    character(len=512) :: my_string
    
    print *, "Selected File has changed:"
    call c_f_string_copy(gtk_file_chooser_get_filename(widget), my_string)
    print *, TRIM(my_string)
    
    ret = FALSE
  end function file_changed
  
  
  ! This is not a handler:
  subroutine convert_c_string(textptr, f_string)
    use iso_c_binding, only: c_char
    implicit none
    character(kind=c_char), dimension(:), pointer, intent(in) :: textptr
    character(len=*), intent(out) :: f_string
    integer :: i
          
    f_string=""
    i=1
    do while(textptr(i) .NE. char(0))
      f_string(i:i)=textptr(i)
      i=i+1
    end do
  end subroutine convert_c_string
end module handlers


program gtkFortran
  use handlers
  use my_widgets
  implicit none
  
  call gtk_init ()

  ! Create the window and set up some signals for it.
  window = gtk_window_new (GTK_WINDOW_TOPLEVEL)
  !call gtk_window_set_default_size(window, 500, 500)
  call gtk_window_set_title(window, "GTK+ & Fortran are good friends"//c_null_char)
  call gtk_container_set_border_width (window, 10_c_int)
  call g_signal_connect (window, "delete-event"//c_null_char, c_funloc(delete_event))
  call g_signal_connect (window, "destroy"//c_null_char, c_funloc(destroy))

  table = gtk_grid_new ()
  call gtk_grid_set_column_homogeneous(table, TRUE)
  call gtk_grid_set_row_homogeneous(table, TRUE)
  call gtk_container_add (window, table)

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

  view = gtk_text_view_new ()
  buffer = gtk_text_view_get_buffer (view)
  call gtk_text_buffer_set_text (buffer, "This is not clean code, just a great bazaar"//char(13)// &
      & "where I test widgets"//c_new_line//"Vincent"//c_new_line//&
      &"You can edit this text."//c_null_char, -1_c_int)
  scrolled_window = gtk_scrolled_window_new (c_null_ptr, c_null_ptr)
  call gtk_container_add (scrolled_window, view)
  call gtk_grid_attach(table, scrolled_window, 0_c_int, 3_c_int, 3_c_int, 3_c_int)  

  my_drawing_area = gtk_drawing_area_new()

  call g_signal_connect (my_drawing_area, "draw"//c_null_char, c_funloc(expose_event))
  call gtk_grid_attach(table, my_drawing_area, 0_c_int, 6_c_int, 3_c_int, 6_c_int)  
  
  file_selector = gtk_file_chooser_button_new ("gtk_file_chooser_button_new"//&
       & c_null_char, 0_c_int)
  call gtk_grid_attach(table, file_selector, 0_c_int, 12_c_int, 3_c_int, 1_c_int)  
  call g_signal_connect (file_selector, "selection-changed"//c_null_char, c_funloc(file_changed));

  call gtk_widget_show_all (window)
  call gtk_main ()
end program gtkFortran
