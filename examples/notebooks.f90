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
! Contributed by Jens Hunger, vmagnin
! Last modification: vmagnin 2020-05-15

module widgets
  use iso_c_binding

  implicit none
  type(c_ptr) :: mainwindow
  type(c_ptr) :: button
  type(c_ptr) :: table
  type(c_ptr) :: notebook_1, notebook_2
  type(c_ptr) :: frame
  type(c_ptr) :: label
  type(c_ptr) :: checkbutton

contains
  subroutine convert_c_string(textptr, f_string)
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
end module

module handlers
  use gtk, only: gtk_init, gtk_window_new, gtk_window_set_title, &
  & g_signal_connect, g_signal_connect_swapped, &
  & gtk_widget_destroy, gtk_widget_show, gtk_application_window_new, &
  & FALSE, TRUE, c_null_char, GTK_POS_TOP, &
  & gtk_button_new, gtk_button_new_with_label, &
  & gtk_check_button_new, gtk_check_button_new_with_label, gtk_container_add, &
  & gtk_frame_new, gtk_label_new, gtk_notebook_append_page, &
  & gtk_notebook_get_current_page, gtk_notebook_get_n_pages, &
  & gtk_notebook_get_show_border, &
  & gtk_notebook_get_show_tabs, gtk_notebook_get_tab_pos, &
  & gtk_notebook_insert_page, gtk_notebook_new, gtk_notebook_next_page, &
  & gtk_notebook_prepend_page, gtk_notebook_prev_page, &
  & gtk_notebook_remove_page, &
  & gtk_notebook_set_current_page, gtk_notebook_set_show_border, &
  & gtk_notebook_set_show_tabs, gtk_notebook_set_tab_detachable, &
  & gtk_notebook_set_tab_pos, &
  & gtk_notebook_set_tab_reorderable, gtk_grid_attach, &
  & gtk_grid_new, gtk_widget_queue_draw, gtk_widget_set_size_request, &
  & gtk_notebook_set_group_name, gtk_notebook_get_group_name, &
  & gtk_widget_set_margin_start, gtk_widget_set_margin_end, &
  & gtk_widget_set_margin_top, gtk_widget_set_margin_bottom

  use g, only: g_main_loop_new, g_main_loop_run, g_main_loop_quit
  use widgets

  implicit none
  type(c_ptr)    :: my_gmainloop

contains
  !*************************************
  ! User defined event handlers go here
  !*************************************

  ! next page
  function next_page_book (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)     :: ret
    type(c_ptr), value, intent(in) :: widget, gdata

    if (gtk_notebook_get_current_page(notebook_1) .eq. gtk_notebook_get_n_pages(notebook_1) - 1) then
      call gtk_notebook_set_current_page (notebook_1, 0_c_int)
    else
      call gtk_notebook_next_page (notebook_1)
    endif
    ret = FALSE
  end function next_page_book

! prev page
  function prev_page_book (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)     :: ret
    type(c_ptr), value, intent(in) :: widget, gdata

    if (gtk_notebook_get_current_page(notebook_1) .eq. 0) then
      call gtk_notebook_set_current_page (notebook_1, -1_c_int)
    else
      call gtk_notebook_prev_page (notebook_1)
    endif
    ret = FALSE
  end function prev_page_book

! Rotate the position of the tabs
  function rotate_book (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)     :: ret
    type(c_ptr), value, intent(in) :: widget, gdata

    call gtk_notebook_set_tab_pos (notebook_1, gtk_notebook_get_tab_pos(notebook_1)+1_c_int)
    ret = FALSE
  end function rotate_book

! Add/Remove the page tabs and the borders
  function tabsborder_book (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)     :: ret
    type(c_ptr), value, intent(in) :: widget, gdata
    integer(c_int) :: tval, bval

    tval = FALSE
    bval = FALSE
    if (gtk_notebook_get_show_tabs(notebook_1) == FALSE) tval = TRUE 
    if (gtk_notebook_get_show_border(notebook_1) == FALSE) bval = TRUE
    call gtk_notebook_set_show_tabs (notebook_1, tval)
    call gtk_notebook_set_show_border (notebook_1, bval)
    ret = FALSE
  end function tabsborder_book

! Remove a page from the notebook
  function remove_book (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)     :: ret
    type(c_ptr), value, intent(in) :: widget, gdata
    integer(c_int) :: page

    page = gtk_notebook_get_current_page (notebook_1)
    call gtk_notebook_remove_page (notebook_1, page)
    ! Need to refresh the widget -- This forces the widget to redraw itself.
    call gtk_widget_queue_draw (notebook_1)
    ret = FALSE
  end function remove_book

  ! Callback function for the signal "activate" emitted by g_application_run().
  ! We use a subroutine because it should return void.
  ! The GUI is defined here.
  subroutine activate(app, gdata) bind(c)
    type(c_ptr), value, intent(in)  :: app, gdata
    ! Pointers toward our GTK widgets:
    type(c_ptr) :: mainwindow
    ! Other variables:
    integer :: i
    character(kind=c_char,len=12) :: istr
    integer(c_int) :: nb
    character(kind=c_char), dimension(:), pointer :: textptr
    character(len=512) :: my_string

    ! Create the window:
    mainwindow = gtk_application_window_new(app)
    ! Don't forget that C strings must end with a null char:
    call gtk_window_set_title(mainwindow, "Notebooks Example"//c_null_char)
 
    !******************************************************************
    ! Adding widgets in the window:
    !******************************************************************
    ! Container for notebook
    table = gtk_grid_new ()
    ! Set the border width (10 pixels) around the container:
    call gtk_widget_set_margin_start (table, 10_c_int)
    call gtk_widget_set_margin_end (table, 10_c_int)
    call gtk_widget_set_margin_top (table, 10_c_int)
    call gtk_widget_set_margin_bottom (table, 10_c_int)
    call gtk_container_add (mainwindow, table)
   
    ! Create a new notebook, place the position of the tabs
    notebook_1=gtk_notebook_new()
    call gtk_notebook_set_tab_pos (notebook_1, GTK_POS_TOP)
    call gtk_grid_attach (table, notebook_1, 0_c_int, 0_c_int, 6_c_int, 1_c_int)

    ! Attach notebook to group, necessary to enable drag and drop
    ! between the two notebooks
    call gtk_notebook_set_group_name(notebook_1,"group"//c_null_char)
    call C_F_POINTER(gtk_notebook_get_group_name(notebook_1), textptr, (/64/))
    call convert_c_string(textptr, my_string)
    print *, "group name = <"//TRIM(my_string)//">"

    !append a bunch of pages to the notebook
    do i=1,3
      write(istr,*)i

      frame = gtk_frame_new ("Append Frame "//trim(adjustl(istr))//c_null_char)
        ! Set the border width (10 pixels) around the frame:
      call gtk_widget_set_margin_start (frame, 10_c_int)
      call gtk_widget_set_margin_end (frame, 10_c_int)
      call gtk_widget_set_margin_top (frame, 10_c_int)
      call gtk_widget_set_margin_bottom (frame, 10_c_int)
    
      call gtk_widget_set_size_request (frame, 100_c_int, 75_c_int)

      label = gtk_label_new ("Append Frame "//trim(adjustl(istr))//c_null_char)
      call gtk_container_add (frame, label)

      label = gtk_label_new ("Page "//trim(adjustl(istr))//c_null_char)
      nb = gtk_notebook_append_page (notebook_1, frame, label)
      call gtk_notebook_set_tab_reorderable (notebook_1, frame, TRUE)
      call gtk_notebook_set_tab_detachable (notebook_1, frame, TRUE)
    enddo

    ! add a page to a specific spot
    checkbutton = gtk_check_button_new_with_label ("Check me please!"//c_null_char)
    call gtk_widget_set_size_request (checkbutton, 20_c_int, 75_c_int)

    label = gtk_label_new ("Add page"//c_null_char)
    nb = gtk_notebook_insert_page (notebook_1, checkbutton, label, 2_c_int)
    call gtk_notebook_set_tab_reorderable(notebook_1, checkbutton, TRUE)
    call gtk_notebook_set_tab_detachable (notebook_1, checkbutton, TRUE)

    ! prepend pages to the notebook 
    do i=1,3
      write(istr,*) i

      frame = gtk_frame_new ("Prepend Frame "//trim(adjustl(istr))//c_null_char)
      ! Set the border width (10 pixels) around the frame:
      call gtk_widget_set_margin_start (frame, 10_c_int)
      call gtk_widget_set_margin_end (frame, 10_c_int)
      call gtk_widget_set_margin_top (frame, 10_c_int)
      call gtk_widget_set_margin_bottom (frame, 10_c_int)
      
      call gtk_widget_set_size_request (frame, 100_c_int, 75_c_int)

      label = gtk_label_new ("Prepend Frame "//trim(adjustl(istr))//c_null_char)
      call gtk_container_add (frame, label)

      label = gtk_label_new ("PPage "//trim(adjustl(istr))//c_null_char)
      nb = gtk_notebook_prepend_page (notebook_1, frame, label)
      call gtk_notebook_set_tab_reorderable (notebook_1, frame, TRUE)
      call gtk_notebook_set_tab_detachable (notebook_1, frame, TRUE)
    enddo

    ! Set what page to start at (page 4)
    call gtk_notebook_set_current_page (notebook_1, 4_c_int)

    ! Create a bunch of buttons
    button = gtk_button_new_with_label ("close"//c_null_char)
    ! Here the gtk_widget_destroy() function will be applied
    ! to window instead of button:
    call g_signal_connect_swapped(button, "clicked"//c_null_char, &
                                & c_funloc(gtk_widget_destroy), mainwindow)

    call gtk_grid_attach (table, button, 0_c_int, 1_c_int, 1_c_int, 1_c_int)

    button = gtk_button_new_with_label ("next page"//c_null_char)
    call g_signal_connect (button, "clicked"//c_null_char, c_funloc(next_page_book))
    call gtk_grid_attach (table, button, 1_c_int, 1_c_int, 1_c_int, 1_c_int)

    button = gtk_button_new_with_label ("prev page"//c_null_char)
    call g_signal_connect (button, "clicked"//c_null_char, c_funloc(prev_page_book))
    call gtk_grid_attach (table, button, 2_c_int, 1_c_int, 1_c_int, 1_c_int)

    button = gtk_button_new_with_label ("tab position"//c_null_char)
    call g_signal_connect (button, "clicked"//c_null_char, c_funloc(rotate_book))
    call gtk_grid_attach (table, button, 3_c_int, 1_c_int, 1_c_int, 1_c_int)

    button = gtk_button_new_with_label ("tabs/border on/off"//c_null_char)
    call g_signal_connect (button, "clicked"//c_null_char, c_funloc(tabsborder_book))
    call gtk_grid_attach (table, button, 4_c_int, 1_c_int, 1_c_int, 1_c_int)

    button = gtk_button_new_with_label ("remove page"//c_null_char)
    call g_signal_connect (button, "clicked"//c_null_char, c_funloc(remove_book))
    call gtk_grid_attach (table, button, 5_c_int, 1_c_int, 1_c_int, 1_c_int)

    ! Create second notebook, place the position of the tabs
    notebook_2=gtk_notebook_new()
    call gtk_notebook_set_tab_pos (notebook_2, GTK_POS_TOP)
    call gtk_grid_attach (table, notebook_2, 0_c_int, 2_c_int, 6_c_int, 1_c_int)

    ! Attach notebook to group
    call gtk_notebook_set_group_name(notebook_2,"group"//c_null_char)

    !append a bunch of pages to the notebook
    do i=1,3
      write(istr,*) i

      frame = gtk_frame_new ("Notebook 2 - Frame "//trim(adjustl(istr))//c_null_char)
      ! Set the border width (10 pixels) around the frame:
      call gtk_widget_set_margin_start (frame, 10_c_int)
      call gtk_widget_set_margin_end (frame, 10_c_int)
      call gtk_widget_set_margin_top (frame, 10_c_int)
      call gtk_widget_set_margin_bottom (frame, 10_c_int)
      
      call gtk_widget_set_size_request (frame, 100_c_int, 75_c_int)

      label = gtk_label_new ("Notebook 2 - Frame "//trim(adjustl(istr))//c_null_char)
      call gtk_container_add (frame, label)

      label = gtk_label_new ("Notebook 2 - Page "//trim(adjustl(istr))//c_null_char)
      nb = gtk_notebook_append_page (notebook_2, frame, label)
      call gtk_notebook_set_tab_reorderable (notebook_2, frame, TRUE)
      call gtk_notebook_set_tab_detachable (notebook_2, frame, TRUE)
    enddo
    !******************************************************************

    ! If you don't show it, nothing will appear on screen...
    call gtk_widget_show(mainwindow)

  end subroutine activate
end module handlers

!*******************************************************************************
! In the main program, we declare the GTK application, connect it to its 
! "activate" function where we will create the GUI, 
! and finally call the GLib main loop.
!*******************************************************************************
program notebooks
  use iso_c_binding, only: c_ptr, c_funloc
  ! We will use those GTK functions and values. The "only" statement can improve
  ! significantly the compilation time:
  use gtk, only: c_null_char, c_null_ptr, gtk_application_new, &
               & G_APPLICATION_FLAGS_NONE
  use g, only: g_application_run, g_object_unref
  use handlers

  implicit none
  integer(c_int)     :: status
  type(c_ptr)        :: app

  ! First, let's create a GTK application (it will initialize GTK).
  ! The application ID must contain at least one point:
  ! https://developer.gnome.org/gio/stable/GApplication.html#g-application-id-is-valid
  app = gtk_application_new("gtk-fortran.examples.notebooks"//c_null_char, &
                            & G_APPLICATION_FLAGS_NONE)
  ! The activate signal will be sent by g_application_run(). 
  ! The c_funloc() function returns the C address of the callback function.
  ! The c_null_ptr means no data is transfered to the callback function.
  call g_signal_connect(app, "activate"//c_null_char, c_funloc(activate), &
                      & c_null_ptr)
  ! Now, the whole application will be managed by GLib (=> main loop).
  ! Note that commandline arguments argc, argv are not passed.
  ! https://developer.gnome.org/gio/stable/GApplication.html#g-application-run
  status = g_application_run(app, 0_c_int, c_null_ptr)

  print *, "You have exited the GLib main loop, bye, bye..."

  ! Memory is freed:
  call g_object_unref(app)
end program notebooks

