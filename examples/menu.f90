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
! If not, see .
!
! gfortran -g gtk.f90 simplemenu.f90 -o simplemenu `pkg-config --cflags --libs gtk+-2.0`
! menu.xml must be copied to the same directory as the simplemenu executable!               
! Contributed by Jens Hunger
! Last modified: 03-13-2011

module handlers
  use gtk
  implicit none
  
  logical :: run_status = TRUE
  logical(c_bool) :: boolresult
  logical :: boolevent
  
  type(c_ptr) :: my_pixbuf
  character(c_char), dimension(:), pointer :: pixel
  integer :: nch, rowstride, width, height
  
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
    logical(c_bool)    :: ret
    type(c_ptr), value :: widget, event, gdata
    print *, "my delete_event"
    ret = FALSE
  end function delete_event

! GtkAction signals:
! open file
  function file_open (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_bool
    logical(c_bool)    :: ret
    type(c_ptr), value :: widget, gdata
    print *, "File open"
    ret = .false.
  end function file_open

! save file
  function file_save (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_bool
    logical(c_bool)    :: ret
    type(c_ptr), value :: widget, gdata
    print *, "File save"
    ret = .false.
  end function file_save

! close file
  function file_close (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_bool
    logical(c_bool)    :: ret
    type(c_ptr), value :: widget, gdata
    print *, "File close"
    ret = .false.
  end function file_close

! cut
  function cut (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_bool
    logical(c_bool)    :: ret
    type(c_ptr), value :: widget, gdata
    print *, "Cut"
    ret = .false.
  end function cut

! copy
  function copy (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_bool
    logical(c_bool)    :: ret
    type(c_ptr), value :: widget, gdata
    print *, "Copy"
    ret = .false.
  end function copy

! paste
  function paste (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_bool
    logical(c_bool)    :: ret
    type(c_ptr), value :: widget, gdata
    print *, "Paste"
    ret = .false.
  end function paste
  
! help
  function help (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_bool
    logical(c_bool)    :: ret
    type(c_ptr), value :: widget, gdata
    print *, "Help"
    ret = .false.
  end function help

! menu dummy function
  function menu (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_bool
    logical(c_bool)    :: ret
    type(c_ptr), value :: widget, gdata
    !print *, "Menu"
    ret = .false.
  end function menu
  
  ! This is not a handler
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
  
end module handlers


program simplemenu

  use iso_c_binding
  use gtk
  use handlers

  implicit none

  type ui_action
      character(kind=c_char,len=30)::name
      character(kind=c_char,len=30)::label
      character(kind=c_char,len=30)::tooltip
      character(kind=c_char,len=30)::stock_id
      type(c_funptr)   ::c_handler
  end type ui_action
    
  type(c_ptr) :: mainwindow
  type(c_ptr) :: box
  type(c_ptr) :: action_group,menu_manager,error
  character(c_char), dimension(:), pointer :: textptr
  character(len=512) :: error_string
  integer :: ui,i
  
  ! Menu action data
  type(ui_action),dimension(11)::action = (/&
    ui_action("FileMenuAction"//CNULL, "_File"//CNULL, CNULL, CNULL, c_null_funptr),&
    ui_action("OpenAction"//CNULL, "_Open"//CNULL, CNULL, CNULL, c_null_funptr),&
    ui_action("SaveAction"//CNULL, "_Save"//CNULL, CNULL, CNULL, c_null_funptr),&
    ui_action("CloseAction"//CNULL, "_Close"//CNULL, CNULL, CNULL, c_null_funptr),&
    ui_action("QuitAction"//CNULL, "_Quit"//CNULL, CNULL, CNULL, c_null_funptr),&
    ui_action("EditMenuAction"//CNULL, "_Edit"//CNULL, CNULL, CNULL, c_null_funptr),&
    ui_action("CutAction"//CNULL, "_Cut"//CNULL, CNULL, CNULL, c_null_funptr),&
    ui_action("CopyAction"//CNULL, "_Copy"//CNULL, CNULL, CNULL, c_null_funptr),&
    ui_action("PasteAction"//CNULL, "_Paste"//CNULL, CNULL, CNULL, c_null_funptr),&
    ui_action("HelpMenuAction"//CNULL, "_Help"//CNULL, CNULL, CNULL, c_null_funptr),&
    ui_action("HelpAction"//CNULL, "_Help"//CNULL, CNULL, CNULL, c_null_funptr)&
    /)
  
  ! this is necessary because gfortran gives error:
  ! Function 'c_funloc' in initialization expression at must be an intrinsic function
  ! and g95 e.g.:
  ! Variable 'destroy' cannot appear in an initialization expression
  action(1)%c_handler=c_funloc(menu)
  action(2)%c_handler=c_funloc(file_open)
  action(3)%c_handler=c_funloc(file_save)
  action(4)%c_handler=c_funloc(file_close)
  action(5)%c_handler=c_funloc(destroy)
  action(6)%c_handler=c_funloc(menu)
  action(7)%c_handler=c_funloc(cut)
  action(8)%c_handler=c_funloc(copy)
  action(9)%c_handler=c_funloc(paste)
  action(10)%c_handler=c_funloc(menu)
  action(11)%c_handler=c_funloc(help)
  
  ! GTK initialisation            
  call gtk_init ()
  
  ! Properties of the main window :
  width = 700
  height = 700
  mainwindow = gtk_window_new (GTK_WINDOW_TOPLEVEL)
  call gtk_window_set_title(mainwindow, "Simple Menu Example"//CNULL)
  call gtk_widget_set_size_request(mainwindow, 500, 500)
  ! You can also use the following statement to automatically 
  ! adapt the vertical size of the window:
  !call gtk_widget_set_size_request(mainwindow, 500, -1)

  ! Connect signals to the main window
  call g_signal_connect (mainwindow, "delete-event"//CNULL, c_funloc(delete_event))
  call g_signal_connect (mainwindow, "destroy"//CNULL, c_funloc(destroy))

  ! Fill action group with actions and connect signals
  action_group = gtk_action_group_new("Menu");
  do i=1,size(action)
      call gtk_action_group_add_action(action_group, gtk_action_new(action(i)%name,&
        action(i)%label,action(i)%tooltip,action(i)%stock_id))
      call g_signal_connect (gtk_action_group_get_action(action_group,action(i)%name),&
        "activate"//CNULL, action(i)%c_handler)
  enddo
  
  ! Insert action group into ui manager
  menu_manager = gtk_ui_manager_new ()
  call gtk_ui_manager_insert_action_group (menu_manager, action_group, 0)
  error = NULL
  ui = gtk_ui_manager_add_ui_from_file (menu_manager, "menu.xml"//CNULL, error)
            
  ! Handle error
  if (c_associated(error)) then
    call c_f_pointer(error, textptr, (/0/))
    call convert_c_string(textptr, error_string)
    print *,"building menus failed: ", error_string
  endif
  
  ! Container for menu
  box = gtk_vbox_new (FALSE,0)
  call gtk_container_add (mainwindow, box)
  call gtk_box_pack_start (box, gtk_ui_manager_get_widget (menu_manager, "/MainMenu"//CNULL), FALSE, FALSE, 0)

  ! Show all
  call gtk_widget_show_all (mainwindow)
  
  ! Main loop
  call gtk_main ()

end program simplemenu
