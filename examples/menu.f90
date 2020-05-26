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
! menu.xml must be copied to the same directory as the simplemenu executable
!
! Contributed by Jens Hunger
! Last modified: 2011-05-04, vmagnin+Ian Harvey 2019-02-21, vmagnin 2020-02-13

module handlers
  use gtk, only: gtk_action_group_add_action, gtk_action_group_get_action, gtk_ac&
  &tion_group_new, gtk_action_new, gtk_container_add, gtk_mai&
  &n, gtk_main_quit, gtk_ui_manager_add_ui, gtk_ui_manager_add_ui_from_file, gtk_&
  &ui_manager_add_ui_from_string, gtk_ui_manager_get_widget, gtk_ui_manager_inser&
  &t_action_group, gtk_ui_manager_new, gtk_box_new, gtk_widget_set_size_request,&
  & gtk_widget_show, gtk_window_new, gtk_window_set_title,&
  &gtk_init, g_signal_connect, FALSE, TRUE, c_null_ptr, c_null_char,&
  & GTK_ORIENTATION_VERTICAL
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
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, event, gdata
    print *, "my delete_event"
    ret = FALSE
  end function delete_event

! GtkAction signals:
! open file
  function file_open (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    print *, "File open"
    ret = FALSE
  end function file_open

! save file
  function file_save (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    print *, "File save"
    ret = FALSE
  end function file_save

! close file
  function file_close (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    print *, "File close"
    ret = FALSE
  end function file_close

! cut
  function cut (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    print *, "Cut"
    ret = FALSE
  end function cut

! copy
  function copy (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    print *, "Copy"
    ret = FALSE
  end function copy

! paste
  function paste (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    print *, "Paste"
    ret = FALSE
  end function paste
  
! help
  function help (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    print *, "Help"
    ret = FALSE
  end function help

! menu dummy function
  function menu (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    !print *, "Menu"
    ret = FALSE
  end function menu
  
  ! This is not a handler
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


program simplemenu
  use iso_c_binding
  use handlers
  use gtk_sup, only: c_f_string_copy
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
  character(len=512) :: error_string
  integer(c_size_t) :: buffer_length
  integer :: ui,i
  character(len=*), parameter :: buffer = &
    "<ui>"//C_NEW_LINE//&
    "  <menubar name=""MainMenu"">"//C_NEW_LINE//&
    "    <menu name=""FileMenu"" action=""FileMenuAction"">"//C_NEW_LINE//&
    "      <menuitem name=""Open"" action=""OpenAction"" />"//C_NEW_LINE//&
    "      <menuitem name=""Save"" action=""SaveAction"" />"//C_NEW_LINE//&
    "      <menuitem name=""Close"" action=""CloseAction"" />"//C_NEW_LINE//&
    "      <menuitem name=""Quit"" action=""QuitAction"" />"//C_NEW_LINE//&
    "    </menu>"//C_NEW_LINE//&
    "    <menu name=""EditMenu"" action=""EditMenuAction"">"//C_NEW_LINE//&
    "      <menuitem name=""Cut"" action=""CutAction""/>"//C_NEW_LINE//&
    "      <menuitem name=""Copy"" action=""CopyAction""/>"//C_NEW_LINE//&
    "      <menuitem name=""Paste"" action=""PasteAction""/>"//C_NEW_LINE//&
    "    </menu>"//C_NEW_LINE//&
    "    <menu name=""HelpMenu"" action=""HelpMenuAction"">"//C_NEW_LINE//&
    "      <menuitem name=""Help"" action=""HelpAction""/>"//C_NEW_LINE//&
    "    </menu>"//C_NEW_LINE//&
    "  </menubar>"//C_NEW_LINE//&
    "</ui>"

  
  ! Menu action data
  type(ui_action),dimension(11)::action = (/&
    ui_action("FileMenuAction"//c_null_char, "_File"//c_null_char, c_null_char, c_null_char, c_null_funptr),&
    ui_action("OpenAction"//c_null_char, "_Open"//c_null_char, c_null_char, c_null_char, c_null_funptr),&
    ui_action("SaveAction"//c_null_char, "_Save"//c_null_char, c_null_char, c_null_char, c_null_funptr),&
    ui_action("CloseAction"//c_null_char, "_Close"//c_null_char, c_null_char, c_null_char, c_null_funptr),&
    ui_action("QuitAction"//c_null_char, "_Quit"//c_null_char, c_null_char, c_null_char, c_null_funptr),&
    ui_action("EditMenuAction"//c_null_char, "_Edit"//c_null_char, c_null_char, c_null_char, c_null_funptr),&
    ui_action("CutAction"//c_null_char, "_Cut"//c_null_char, c_null_char, c_null_char, c_null_funptr),&
    ui_action("CopyAction"//c_null_char, "_Copy"//c_null_char, c_null_char, c_null_char, c_null_funptr),&
    ui_action("PasteAction"//c_null_char, "_Paste"//c_null_char, c_null_char, c_null_char, c_null_funptr),&
    ui_action("HelpMenuAction"//c_null_char, "_Help"//c_null_char, c_null_char, c_null_char, c_null_funptr),&
    ui_action("HelpAction"//c_null_char, "_Help"//c_null_char, c_null_char, c_null_char, c_null_funptr)&
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
  mainwindow = gtk_window_new()
  call gtk_window_set_title(mainwindow, "Simple Menu Example"//c_null_char)
  call gtk_widget_set_size_request(mainwindow, 500_c_int, 500_c_int)
  ! You can also use the following statement to automatically 
  ! adapt the vertical size of the window:
  !call gtk_widget_set_size_request(mainwindow, 500, -1)

  ! Connect signals to the main window
  call g_signal_connect (mainwindow, "delete-event"//c_null_char, c_funloc(delete_event))
  call g_signal_connect (mainwindow, "destroy"//c_null_char, c_funloc(destroy))

  ! Fill action group with actions and connect signals
  action_group = gtk_action_group_new("Menu");
  do i=1,size(action)
      call gtk_action_group_add_action(action_group, gtk_action_new(action(i)%name,&
        action(i)%label,action(i)%tooltip,action(i)%stock_id))
      call g_signal_connect (gtk_action_group_get_action(action_group,action(i)%name),&
        "activate"//c_null_char, action(i)%c_handler)
  enddo
  
  ! Insert action group into ui manager
  menu_manager = gtk_ui_manager_new ()
  call gtk_ui_manager_insert_action_group (menu_manager, action_group, 0_c_int)
  error = c_null_ptr
  !ui = gtk_ui_manager_add_ui_from_file (menu_manager, "menu.xml"//c_null_char, error)
  buffer_length = len_trim(buffer)
  ui = gtk_ui_manager_add_ui_from_string (menu_manager, trim(buffer)//c_null_char, buffer_length, error)

  ! Handle error
  if (c_associated(error)) then
    call c_f_string_copy(error, error_string)
    print *,"building menus failed: ", error_string
  endif
  
  ! Container for menu
  box = gtk_box_new (GTK_ORIENTATION_VERTICAL,0_c_int)
  call gtk_container_add (mainwindow, box)
  call gtk_container_add (box, gtk_ui_manager_get_widget (menu_manager, "/MainMenu"//c_null_char))

  ! Show all
  call gtk_widget_show(mainwindow)
  
  ! Main loop
  call gtk_main ()

end program simplemenu
