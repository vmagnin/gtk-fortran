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
! Contributed by Vincent Magnin and Jerry DeLisle
! Last modifications: vmagnin+Ian Harvey 02-21-2019, vmagnin 02-22-2019
! gfortran -I../src ../src/gtk.f90 julia_pixbuf.f90 `pkg-config --cflags --libs gtk+-3.0` -Wall -Wextra -pedantic -std=f2003 -g


module global_widgets
  use iso_c_binding, only: c_ptr, c_char, c_int
  type(c_ptr) :: my_pixbuf, my_drawing_area, spinButton1, spinButton2, spinButton3
  type(c_ptr) :: textView, buffer, scrolled_window, statusBar, combo1
  character(kind=c_char), dimension(:), pointer :: pixel
  integer(kind=c_int) :: nch, rowstride, width, height, pixwidth, pixheight
  logical :: computing = .false.
  character(LEN=80) :: string
end module global_widgets


module handlers
  use gtk, only: gtk_container_add, gtk_drawing_area_new, gtk_events_pending, gtk&
  &_main, gtk_main_iteration, gtk_main_iteration_do, gtk_widget_get_window, gtk_w&
  &idget_queue_draw, gtk_widget_show, gtk_window_new, gtk_window_set_default, gtk&
  &_window_set_default_size, gtk_window_set_title, TRUE, FALSE, c_null_ptr, c_null_char, &
  &GDK_COLORSPACE_RGB, GTK_WINDOW_TOPLEVEL, gtk_init, g_signal_connect, &
  &gtk_grid_new, gtk_grid_attach, gtk_container_add, gtk_button_new_with_label,&
  &gtk_widget_show_all, gtk_box_new, gtk_box_pack_start, gtk_spin_button_new,&
  &gtk_adjustment_new, gtk_spin_button_get_value, gtk_label_new, &
  &gtk_expander_new_with_mnemonic, gtk_expander_set_expanded, gtk_main_quit, &
  &gtk_toggle_button_new_with_label, gtk_toggle_button_get_active, gtk_notebook_new,&
  &gtk_notebook_append_page, gtk_text_view_new, gtk_text_view_get_buffer, gtk_text_buffer_set_text,&
  &gtk_scrolled_window_new, C_NEW_LINE, gtk_text_buffer_insert_at_cursor, gtk_statusbar_new,&
  &gtk_statusbar_push, gtk_statusbar_get_context_id, gtk_handle_box_new,&
  &CAIRO_STATUS_SUCCESS, CAIRO_STATUS_NO_MEMORY, CAIRO_STATUS_SURFACE_TYPE_MISMATCH,&
  &CAIRO_STATUS_WRITE_ERROR, gtk_button_new_with_mnemonic, gtk_link_button_new_with_label,&
  &gtk_toggle_button_new_with_mnemonic, gtk_label_new_with_mnemonic, &
  &gtk_window_set_mnemonics_visible, gtk_combo_box_text_new, &
  &gtk_combo_box_text_append_text, gtk_combo_box_text_get_active_text, &
  &gtk_combo_box_text_insert_text, gtk_spin_button_set_value, gtk_spin_button_update, &
  & GTK_ORIENTATION_VERTICAL, &
  & gtk_grid_set_column_homogeneous, &
  & gtk_grid_set_row_homogeneous, gtk_statusbar_remove_all

  use cairo, only: cairo_create, cairo_destroy, cairo_paint, cairo_set_source, &
  &cairo_surface_write_to_png, cairo_get_target

  use gdk, only: gdk_cairo_create, gdk_cairo_set_source_pixbuf

  use gdk_pixbuf, only: gdk_pixbuf_get_n_channels, gdk_pixbuf_get_pixels, gdk_pix&
  &buf_get_rowstride, gdk_pixbuf_new

  use g, only: g_usleep

  use iso_c_binding, only: c_int, c_ptr, c_char

  implicit none
  integer(c_int) :: run_status = TRUE
  integer(c_int) :: boolresult
  logical :: boolevent

contains
  ! User defined event handlers go here
  function delete_event (widget, event, gdata) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, event, gdata

    run_status = FALSE
    ret = FALSE
    call gtk_main_quit()
  end function delete_event

  ! In this example, this function is declared recursive because it can be
  ! called a second time by the toggle button callback function
  ! firstToggle (widget, gdata ):
  recursive subroutine pending_events ()
    do while(IAND(gtk_events_pending(), run_status) /= FALSE)
      boolresult = gtk_main_iteration_do(FALSE) ! False for non-blocking
    end do
  end subroutine pending_events


  function expose_event (widget, event, gdata) result(ret)  bind(c)
    use iso_c_binding, only: c_int, c_ptr
    use global_widgets
    implicit none
    integer(c_int)    :: ret
    type(c_ptr), value, intent(in) :: widget, event, gdata
    type(c_ptr) :: my_cairo_context

    my_cairo_context = gdk_cairo_create (gtk_widget_get_window(widget))
    call gdk_cairo_set_source_pixbuf(my_cairo_context, my_pixbuf, 0d0, 0d0)
    call cairo_paint(my_cairo_context)
    call cairo_destroy(my_cairo_context)
    ret = FALSE
  end function expose_event


  ! GtkButton signal:
  function firstbutton (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr
    use global_widgets
    implicit none

    integer(c_int)    :: ret, message_id
    type(c_ptr), value :: widget, gdata
    complex(kind(1d0)) :: c
    integer :: iterations

    c = gtk_spin_button_get_value (spinButton1) + &
        & (0d0, 1d0)*gtk_spin_button_get_value (spinButton2)
    iterations = INT(gtk_spin_button_get_value (spinButton3))

    write(string, '("c=",F8.6,"+i*",F8.6,"   ", I6, " iterations")') c, iterations
    call gtk_text_buffer_insert_at_cursor (buffer, string//C_NEW_LINE&
         & //c_null_char, -1_c_int)

    message_id = gtk_statusbar_push (statusBar, gtk_statusbar_get_context_id(statusBar, &
              & "Julia"//c_null_char), "Computing..."//c_null_char)
    call Julia_set(-2d0, +2d0, -2d0, +2d0, c, iterations)
    message_id = gtk_statusbar_push (statusBar, gtk_statusbar_get_context_id(statusBar, &
              & "Julia"//c_null_char), "Finished."//c_null_char)

    ret = FALSE
  end function firstbutton


  ! GtkComboBox signal:
  function firstCombo (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int, c_double, c_f_pointer
    use gtk_sup, only: c_f_string_copy
    use global_widgets
    implicit none
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    real(c_double) :: x, y
    integer :: choice
    character(len=512) :: my_string

    call c_f_string_copy( gtk_combo_box_text_get_active_text(combo1), my_string)
    read(my_string, *) choice

    select case (choice)
    case(1)
      x = +0d0
      y = +1d0
    case(2)
      x = -1d0
      y = +0d0
    case(3)
      x = -0.8d0
      y = +0.2d0
    case(4)
      x = +0.39d0
      y = +0.60d0
    case(5)
      x = -0.2d0
      y = +0.8d0
    case(6)
      x = -0.8d0
      y = +0.4d0
    case(7)
      x = +0.39d0
      y = +0.00d0
    end select

    call gtk_spin_button_set_value (spinButton1, x)
    call gtk_spin_button_set_value (spinButton2, y)

    ret = FALSE
  end function firstCombo


  ! GtkButton signal:
  function secondbutton (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr
    use global_widgets
    implicit none

    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    type(c_ptr) :: my_cairo_context
    integer(c_int) :: cstatus, message_id

    !my_cairo_context = gdk_cairo_create (gtk_widget_get_window(widget))
    my_cairo_context = gdk_cairo_create (gtk_widget_get_window(my_drawing_area))
    call gdk_cairo_set_source_pixbuf(my_cairo_context, my_pixbuf, 0d0, 0d0)
    ! Save the picture if finished:
    if (.not. computing) then
      cstatus = cairo_surface_write_to_png(cairo_get_target(my_cairo_context), "julia.png"//c_null_char)
      if (cstatus == CAIRO_STATUS_SUCCESS) then
        string = "Successfully saved: julia.png"//c_null_char
      else if (cstatus == CAIRO_STATUS_NO_MEMORY) then
        string = "Failed: memory allocation"//c_null_char
      else if (cstatus == CAIRO_STATUS_SURFACE_TYPE_MISMATCH) then
        string = "Failed: no pixel content"//c_null_char
      else if (cstatus == CAIRO_STATUS_WRITE_ERROR) then
        string = "Failed: I/O error"//c_null_char
      else
        string = "Failed"
      end if
      message_id = gtk_statusbar_push (statusBar, gtk_statusbar_get_context_id(statusBar, &
              & "Julia"//c_null_char), TRIM(string))
    end if
    ! FIXME: how to save only the drawing_area
    call cairo_destroy(my_cairo_context)

    ret = FALSE
  end function secondbutton


  ! GtkToggleButton signal:
  ! This function is declared recursive because it will be pressed a first time
  ! to pause and a second time to end pause (from the "do while" loop):
  recursive function firstToggle (widget, gdata) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_long
    use global_widgets
    implicit none

    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata

    if (gtk_toggle_button_get_active(widget) == TRUE) then
      call gtk_text_buffer_insert_at_cursor (buffer, &
           & "In pause"//C_NEW_LINE//c_null_char,&
           & -1_c_int)
      do while (gtk_toggle_button_get_active(widget) == TRUE)
        call pending_events
        if (run_status == FALSE) exit ! Exit if we had a delete event
        call g_usleep(50000_c_long)   ! In microseconds
      end do
    else
      call gtk_text_buffer_insert_at_cursor (buffer, "Not in pause"//C_NEW_LINE//c_null_char, -1_c_int)
    end if

    ret = FALSE
  end function firstToggle

end module handlers


program julia
  use iso_c_binding, only: c_ptr, c_funloc, c_f_pointer
  use handlers
  use global_widgets
  implicit none

  type(c_ptr) :: my_window, table, button1, button2, button3, box1
  type(c_ptr) :: label1, label2, label3, label4
  type(c_ptr) :: toggle1, expander, notebook, notebookLabel1, notebookLabel2
  type(c_ptr) :: linkButton
  integer(c_int) :: message_id, firstTab, secondTab

  call gtk_init ()

  ! Properties of the main window :
  width = 700
  height = 700
  my_window = gtk_window_new (GTK_WINDOW_TOPLEVEL)
  call gtk_window_set_default_size(my_window, width, height)
  call gtk_window_set_title(my_window, "Julia Set"//c_null_char)
  call g_signal_connect (my_window, "delete-event"//c_null_char, c_funloc(delete_event))

  button1 = gtk_button_new_with_mnemonic ("_Compute"//c_null_char)
  call g_signal_connect (button1, "clicked"//c_null_char, c_funloc(firstbutton))
  button2 = gtk_button_new_with_mnemonic ("_Save as PNG"//c_null_char)
  call g_signal_connect (button2, "clicked"//c_null_char, c_funloc(secondbutton))
  button3 = gtk_button_new_with_mnemonic ("_Exit"//c_null_char)
  call g_signal_connect (button3, "clicked"//c_null_char, c_funloc(delete_event))

  label1 = gtk_label_new("real(c)"//c_null_char)
  spinButton1 = gtk_spin_button_new (gtk_adjustment_new(-0.835d0,-2d0,+2d0,0.05d0,0.5d0,0d0),0.05d0, 7_c_int)
  label2 = gtk_label_new("imag(c) "//c_null_char)
  spinButton2 = gtk_spin_button_new (gtk_adjustment_new(-0.2321d0, -2d0,+2d0,0.05d0,0.5d0,0d0),0.05d0, 7_c_int)
  label3 = gtk_label_new("iterations"//c_null_char)
  spinButton3 = gtk_spin_button_new (gtk_adjustment_new(100000d0,1d0,+1000000d0,10d0,100d0,0d0),10d0, 0_c_int)

  label4 = gtk_label_new("Predefined values:"//c_null_char)
  combo1 = gtk_combo_box_text_new()
  call gtk_combo_box_text_append_text(combo1, "1"//c_null_char)
  call gtk_combo_box_text_append_text(combo1, "2"//c_null_char)
  call gtk_combo_box_text_append_text(combo1, "3"//c_null_char)
  call gtk_combo_box_text_append_text(combo1, "4"//c_null_char)
  call gtk_combo_box_text_append_text(combo1, "5"//c_null_char)
  call gtk_combo_box_text_append_text(combo1, "6"//c_null_char)
  call gtk_combo_box_text_append_text(combo1, "7"//c_null_char)
  call g_signal_connect (combo1, "changed"//c_null_char, c_funloc(firstCombo))

  toggle1 = gtk_toggle_button_new_with_mnemonic ("_Pause"//c_null_char)
  call g_signal_connect (toggle1, "toggled"//c_null_char, c_funloc(firstToggle))

  linkButton = gtk_link_button_new_with_label ("http://en.wikipedia.org/wiki/Julia_set"//c_null_char,&
               & "More on Julia sets"//c_null_char)

  ! A table container will contain buttons and labels:
  table = gtk_grid_new ()
  call gtk_grid_set_column_homogeneous(table, TRUE)
  call gtk_grid_set_row_homogeneous(table, TRUE)

  call gtk_grid_attach(table, button1, 0_c_int, 3_c_int, 1_c_int, 1_c_int)
  call gtk_grid_attach(table, button2, 1_c_int, 3_c_int, 1_c_int, 1_c_int)
  call gtk_grid_attach(table, button3, 3_c_int, 3_c_int, 1_c_int, 1_c_int)
  call gtk_grid_attach(table, label1, 0_c_int, 0_c_int, 1_c_int, 1_c_int)
  call gtk_grid_attach(table, label2, 0_c_int, 1_c_int, 1_c_int, 1_c_int)
  call gtk_grid_attach(table, label3, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
  call gtk_grid_attach(table, spinButton1, 1_c_int, 0_c_int, 1_c_int, 1_c_int)
  call gtk_grid_attach(table, spinButton2, 1_c_int, 1_c_int, 1_c_int, 1_c_int)
  call gtk_grid_attach(table, spinButton3, 1_c_int, 2_c_int, 1_c_int, 1_c_int)
  call gtk_grid_attach(table, linkButton, 3_c_int, 0_c_int, 1_c_int, 1_c_int)
  call gtk_grid_attach(table, label4, 2_c_int, 0_c_int, 1_c_int, 1_c_int)
  call gtk_grid_attach(table, combo1, 2_c_int, 1_c_int, 1_c_int,1_c_int)
  call gtk_grid_attach(table, toggle1, 2_c_int, 3_c_int, 1_c_int,1_c_int)

  ! The table is contained in an expander, which is contained in the vertical box:
  expander = gtk_expander_new_with_mnemonic ("_The parameters:"//c_null_char)
  call gtk_container_add (expander, table)
  call gtk_expander_set_expanded(expander, TRUE)

  ! We create a vertical box container:
  box1 = gtk_box_new (GTK_ORIENTATION_VERTICAL, 10_c_int);
  call gtk_box_pack_start (box1, expander, FALSE, FALSE, 0_c_int)

  ! The drawing area is contained in the vertical box:
  my_drawing_area = gtk_drawing_area_new()
  call g_signal_connect (my_drawing_area, "draw"//c_null_char, c_funloc(expose_event))
  ! In GTK+ 3.0 expose-event will be replaced by draw event:
  !call g_signal_connect (my_drawing_area, "draw"//c_null_char, c_funloc(expose_event))
  notebook = gtk_notebook_new ()
  notebookLabel1 = gtk_label_new_with_mnemonic("_Graphics"//c_null_char)
  firstTab = gtk_notebook_append_page (notebook, my_drawing_area, notebookLabel1)

  !handle1 = gtk_handle_box_new()

  textView = gtk_text_view_new ()
  buffer = gtk_text_view_get_buffer (textView)
  call gtk_text_buffer_set_text (buffer, "Julia Set"//C_NEW_LINE// &
      & "You can copy this text and even edit it !"//C_NEW_LINE//c_null_char,&
      & -1_c_int)
  scrolled_window = gtk_scrolled_window_new (c_null_ptr, c_null_ptr)
  notebookLabel2 = gtk_label_new_with_mnemonic("_Messages"//c_null_char)
  call gtk_container_add (scrolled_window, textView)
  !call gtk_container_add (handle1, scrolled_window)
  secondTab = gtk_notebook_append_page (notebook, scrolled_window, notebookLabel2)

  call gtk_box_pack_start (box1, notebook, TRUE, TRUE, 0_c_int)

  statusBar = gtk_statusbar_new ()
  message_id = gtk_statusbar_push (statusBar, gtk_statusbar_get_context_id(statusBar, &
              & "Julia"//c_null_char), "Waiting..."//c_null_char)
  call gtk_box_pack_start (box1, statusBar, FALSE, FALSE, 0_c_int)

  call gtk_container_add (my_window, box1)
  call gtk_window_set_mnemonics_visible (my_window, TRUE)
  call gtk_widget_show_all (my_window)

  ! We create a "pixbuffer" to store the pixels of the image:
  pixwidth  = 500
  pixheight = 500
  my_pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8_c_int, pixwidth, pixheight)
  nch = gdk_pixbuf_get_n_channels(my_pixbuf)
  rowstride = gdk_pixbuf_get_rowstride(my_pixbuf)
  call c_f_pointer(gdk_pixbuf_get_pixels(my_pixbuf), pixel, (/pixwidth*pixheight*nch/))
  ! We use char() because we need unsigned integers.
  ! This pixbuffer has no Alpha channel (15% faster), only RGB.
  pixel = char(0)

  ! Main loop:
  call gtk_main ()
  print *, "All done"
end program julia


!*********************************************
! Julia Set
! http://en.wikipedia.org/wiki/Julia_set
!*********************************************
subroutine Julia_set(xmin, xmax, ymin, ymax, c, itermax)
  use iso_c_binding
  use handlers
  use global_widgets
  implicit none

  integer    :: i, j, k, p, itermax
  double precision :: x, y, xmin, xmax, ymin, ymax ! coordinates in the complex plane
  complex(kind(1d0)) :: c, z
  double precision :: scx, scy       ! scales
  integer(1) :: red, green, blue     ! rgb color
  double precision :: system_time, t0, t1

  computing = .true.
  t0=system_time()

  scx = ((xmax - xmin) / pixwidth)   ! x scale
  scy = ((ymax - ymin) / pixheight)  ! y scale

  do i=0, pixwidth-1
    ! We provoke an expose_event only once in a while to improve performances:
    if (mod(i,10)==0) then
      call gtk_widget_queue_draw(my_drawing_area)
    end if

    x = xmin + scx * i
    do j=0, pixheight-1
      y = ymin + scy * j
      z = x + y*(0d0,1d0)   ! Starting point
      k = 1
      do while ((k <= itermax) .and. ((real(z)**2+aimag(z)**2)<4d0))
        z = z*z + c
        k = k + 1
      end do

      if (k>itermax) then
        ! Black pixel:
        red   = 0
        green = 0
        blue  = 0
      else
        ! User defined palette:
        red   = int(min(255, k*2),  KIND=1)
        green = int(min(255, k*5),  KIND=1)
        blue  = int(min(255, k*10), KIND=1)
      end if

      ! We write in the pixbuffer:
      p = i * nch + j * rowstride + 1
      pixel(p)   = char(red)
      pixel(p+1) = char(green)
      pixel(p+2) = char(blue)

      ! This subroutine processes gtk events as needed during the computation.
      call pending_events()
      if (run_status == FALSE) return ! Exit if we had a delete event.
    end do
  end do
  ! Final update of the display:
  call gtk_widget_queue_draw(my_drawing_area)
  computing = .false.

  t1=system_time()
  write(string, '("System time = ",F8.3, " s")') t1-t0
  call gtk_text_buffer_insert_at_cursor (buffer, &
       & string//C_NEW_LINE//c_null_char, -1_c_int)

end subroutine Julia_set

!***********************************************************
!  system time since 00:00
!***********************************************************
real(8) function system_time()
  implicit none
  integer, dimension(8) :: dt

  call date_and_time(values=dt)
  system_time=dt(5)*3600d0+dt(6)*60d0+dt(7)+dt(8)*0.001d0
end function system_time
