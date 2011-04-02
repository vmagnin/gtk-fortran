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
! gfortran -g gtk.f90 notebooks.f90 -o notebooks `pkg-config --cflags --libs gtk+-2.0`
! Contributed by Jens Hunger

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

  ! *** These two functions will be avalaibable in GTK+ 2.24 ***
  !void gtk_notebook_set_group_name (GtkNotebook *notebook, const gchar *group_name);
  subroutine gtk_notebook_set_group_name (notebook, group_name) bind(c)
    use iso_c_binding, only: c_ptr, c_char
    type(c_ptr), value :: notebook
    character(kind=c_char), dimension(*) :: group_name
  end subroutine

  !const gchar *gtk_notebook_get_group_name (GtkNotebook *notebook);
  function gtk_notebook_get_group_name (notebook) bind(c)
    use iso_c_binding, only: c_ptr
    type(c_ptr), value :: notebook
    type(c_ptr) :: gtk_notebook_get_group_name
  end function

  subroutine convert_c_string(textptr, f_string)
    use iso_c_binding, only: c_char
    implicit none
    character(c_char), dimension(:), pointer, intent(in) :: textptr
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
  use gtk
  use widgets

  implicit none
  
contains
  !*************************************
  ! User defined event handlers go here
  !*************************************
  ! Note that events are a special type of signals, coming from the
  ! X Window system. Then callback functions must have an event argument.

  ! "destroy" is a GtkObject signal
  subroutine destroy (widget, gdata) bind(c)
    use iso_c_binding, only: c_ptr
    type(c_ptr), value :: widget, gdata
    print *, "my destroy"
    call gtk_main_quit ()
  end subroutine destroy

! delete event
  function delete_event (widget, event, gdata) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_bool
    integer(c_int)     :: ret
    type(c_ptr), value :: widget, event, gdata
    print *, "my delete_event"
    ret = FALSE
  end function delete_event

! next page
  function next_page_book (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_bool
    integer(c_int)     :: ret
    type(c_ptr), value :: widget, gdata
    if (gtk_notebook_get_current_page(notebook_1) .eq. gtk_notebook_get_n_pages(notebook_1) - 1) then
      call gtk_notebook_set_current_page (notebook_1, 0)
    else
      call gtk_notebook_next_page (notebook_1)
    endif
    ret = FALSE
  end function next_page_book

! prev page
  function prev_page_book (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_bool
    integer(c_int)     :: ret
    type(c_ptr), value :: widget, gdata
    if (gtk_notebook_get_current_page(notebook_1) .eq. 0) then
      call gtk_notebook_set_current_page (notebook_1, -1)
    else
  call gtk_notebook_prev_page (notebook_1)
    endif
    ret = FALSE
  end function prev_page_book

! Rotate the position of the tabs
  function rotate_book (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_bool
    integer(c_int)     :: ret
    type(c_ptr), value :: widget, gdata
    call gtk_notebook_set_tab_pos (notebook_1, gtk_notebook_get_tab_pos(notebook_1)+1)
    ret = FALSE
  end function rotate_book

! Add/Remove the page tabs and the borders
  function tabsborder_book (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_bool
    integer(c_int)     :: ret
    type(c_ptr), value :: widget, gdata
    integer(c_int) :: tval, bval ! tval = FALSE, bval = FALSE: does not work properly with gfortran 4.6 !
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
    use iso_c_binding, only: c_ptr, c_bool
    integer(c_int)     :: ret
    type(c_ptr), value :: widget, gdata
    integer(c_int) :: page
    page = gtk_notebook_get_current_page (notebook_1)
    call gtk_notebook_remove_page (notebook_1, page)
    ! Need to refresh the widget -- This forces the widget to redraw itself.
    call gtk_widget_queue_draw (notebook_1)
    ret = FALSE
  end function remove_book

end module handlers


program notebook_example
  use handlers

  implicit none

  integer :: i
  character(kind=c_char,len=12) :: istr
  integer(c_int) :: nb

  character(c_char), dimension(:), pointer :: textptr
  character(len=512) :: my_string

  ! GTK initialisation      
  call gtk_init ()
  
  ! Properties of the main window :
  mainwindow = gtk_window_new (GTK_WINDOW_TOPLEVEL)
  call gtk_window_set_title(mainwindow, "Notebook Example"//CNULL)
  call gtk_container_set_border_width (mainwindow, 10)

  ! Connect signals to the main window
  call g_signal_connect (mainwindow, "delete-event"//CNULL, c_funloc(delete_event))
  call g_signal_connect (mainwindow, "destroy"//CNULL, c_funloc(destroy))

  ! Container for notebook
  table = gtk_table_new (3, 6, FALSE)
  call gtk_container_add (mainwindow, table)
 
  ! Create a new notebook, place the position of the tabs
  notebook_1=gtk_notebook_new()
  call gtk_notebook_set_tab_pos (notebook_1, GTK_POS_TOP)
  call gtk_table_attach_defaults (table, notebook_1, 0, 6, 0, 1)

  ! Attach notebook to group, necessary to enable drag and drop between the two notebooks
  call gtk_notebook_set_group_name(notebook_1,"group"//CNULL)
! --> this is not working!!! (available in GTK+ 2.24): 
  call C_F_POINTER(gtk_notebook_get_group_name(notebook_1), textptr, (/64/))
  call convert_c_string(textptr, my_string)
  print *, "group name = <"//TRIM(my_string)//">"
  print *, "=> This should work with GTK+ 2.24 and later"

  !append a bunch of pages to the notebook
  do i=1,3
    write(istr,*)i
    
    frame = gtk_frame_new ("Append Frame "//trim(adjustl(istr))//CNULL)
    call gtk_container_set_border_width (frame, 10)
    call gtk_widget_set_size_request (frame, 100, 75)
    
    label = gtk_label_new ("Append Frame "//trim(adjustl(istr))//CNULL)
    call gtk_container_add (frame, label)
    
    label = gtk_label_new ("Page "//trim(adjustl(istr))//CNULL)
    nb = gtk_notebook_append_page (notebook_1, frame, label)
    call gtk_notebook_set_tab_reorderable (notebook_1, frame, TRUE)
    call gtk_notebook_set_tab_detachable (notebook_1, frame, TRUE)
  enddo
 
  ! add a page to a specific spot
  checkbutton = gtk_check_button_new_with_label ("Check me please!"//CNULL)
  call gtk_widget_set_size_request (checkbutton, 20, 75)
   
  label = gtk_label_new ("Add page"//CNULL)
  nb = gtk_notebook_insert_page (notebook_1, checkbutton, label, 2)
  call gtk_notebook_set_tab_reorderable (notebook_1, label, TRUE)
  call gtk_notebook_set_tab_detachable (notebook_1, label, TRUE)
    
  ! prepend pages to the notebook 
  do i=1,3
    write(istr,*) i
    
    frame = gtk_frame_new ("Prepend Frame "//trim(adjustl(istr))//CNULL)
    call gtk_container_set_border_width (frame, 10)
    call gtk_widget_set_size_request (frame, 100, 75)
    
    label = gtk_label_new ("Prepend Frame "//trim(adjustl(istr))//CNULL)
    call gtk_container_add (frame, label)
    
    label = gtk_label_new ("PPage "//trim(adjustl(istr))//CNULL)
    nb = gtk_notebook_prepend_page (notebook_1, frame, label)
    call gtk_notebook_set_tab_reorderable (notebook_1, frame, TRUE)
    call gtk_notebook_set_tab_detachable (notebook_1, frame, TRUE)
  enddo
    
  ! Set what page to start at (page 4)
  call gtk_notebook_set_current_page (notebook_1, 4)

  ! Create a bunch of buttons
  button = gtk_button_new_with_label ("close"//CNULL)
  call g_signal_connect (button, "clicked"//CNULL, c_funloc(destroy))
  call gtk_table_attach_defaults (table, button, 0, 1, 1, 2)
    
  button = gtk_button_new_with_label ("next page"//CNULL)
  call g_signal_connect (button, "clicked"//CNULL, c_funloc(next_page_book))
  call gtk_table_attach_defaults (table, button, 1, 2, 1, 2)
    
  button = gtk_button_new_with_label ("prev page"//CNULL)
  call g_signal_connect (button, "clicked"//CNULL, c_funloc(prev_page_book))
  call gtk_table_attach_defaults (table, button, 2, 3, 1, 2)
    
  button = gtk_button_new_with_label ("tab position"//CNULL)
  call g_signal_connect (button, "clicked"//CNULL, c_funloc(rotate_book))
  call gtk_table_attach_defaults (table, button, 3, 4, 1, 2)
    
  button = gtk_button_new_with_label ("tabs/border on/off"//CNULL)
  call g_signal_connect (button, "clicked"//CNULL, c_funloc(tabsborder_book))
  call gtk_table_attach_defaults (table, button, 4, 5, 1, 2)
    
  button = gtk_button_new_with_label ("remove page"//CNULL)
  call g_signal_connect (button, "clicked"//CNULL, c_funloc(remove_book))
  call gtk_table_attach_defaults (table, button, 5, 6, 1, 2)

  ! Create second notebook, place the position of the tabs
  notebook_2=gtk_notebook_new()
  call gtk_notebook_set_tab_pos (notebook_2, GTK_POS_TOP)
  call gtk_table_attach_defaults (table, notebook_2, 0, 6, 2, 3)

  ! Attach notebook to group
  call gtk_notebook_set_group_name(notebook_2,"group"//CNULL)

  !append a bunch of pages to the notebook
  do i=1,3
    write(istr,*) i
    
    frame = gtk_frame_new ("Notebook 2 - Frame "//trim(adjustl(istr))//CNULL)
    call gtk_container_set_border_width (frame, 10)
    call gtk_widget_set_size_request (frame, 100, 75)
    
    label = gtk_label_new ("Notebook 2 - Frame "//trim(adjustl(istr))//CNULL)
    call gtk_container_add (frame, label)
    
    label = gtk_label_new ("Notebook 2 - Page "//trim(adjustl(istr))//CNULL)
    nb = gtk_notebook_append_page (notebook_2, frame, label)
    call gtk_notebook_set_tab_reorderable (notebook_2, frame, TRUE)
    call gtk_notebook_set_tab_detachable (notebook_2, frame, TRUE)
  enddo

  call gtk_widget_show_all (mainwindow)
   
  ! Main loop
  call gtk_main ()

end program notebook_example
