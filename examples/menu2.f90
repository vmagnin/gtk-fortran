! Copyright (C) 2011
! Free Software Foundation, Inc.
!
! This file is part of the gtk-fortran GTK / Fortran Interface library.
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
! You should have received a copy of the GNU General Public License along with
! this program; see the files COPYING3 and COPYING.RUNTIME respectively.
! If not, see <http://www.gnu.org/licenses/>.
!-------------------------------------------------------------------------------
! Vincent MAGNIN, 2020-01-31
! Last modified: 2020-01-31
! This example just creates an empty GTK window with a menu
!-------------------------------------------------------------------------------

!*************************************
! User defined event handlers go here
!*************************************
! Note that events are a special type of signals, coming from the X Window system.
! Callback functions must have an event argument.
module handlers

  use iso_c_binding, only: c_ptr, c_int
  use gtk, only: gtk_main_quit, FALSE
  implicit none

contains
  ! Signal emitted if a user requests that a toplevel window is closed:
  function delete_event(widget, event, gdata) result(ret)  bind(c)
    integer(c_int)     :: ret
    type(c_ptr), value :: widget, event, gdata

    print *, "My delete_event"
    ret = FALSE
  end function delete_event

  ! "destroy" is a GtkObject signal emitted when an object is destroyed:
  subroutine destroy(widget, event, gdata) bind(c)
    type(c_ptr), value :: widget, event, gdata

    print *, "My destroy: you will exit the GTK main loop..."
    call gtk_main_quit()
  end subroutine destroy

  ! GtkAction signals:
! New file
  function file_new (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    print *, "You want to create a new file."
    ret = FALSE
  end function file_new
  
! open file
  function file_open (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    print *, "You want to open a file."
    ret = FALSE
  end function file_open

! help
  function help_about (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    print *, "This is the menu2.f90 example, you can quit with CTRL+Q"
    ret = FALSE
  end function help_about
  
end module handlers

!*********************************************************************************************
! In the main program, we initialize GTK, we design the UI, and finally call the GTK main loop
! For more information on GTK functions: https://developer.gnome.org/gtk3/stable/
!*********************************************************************************************
program menu2

  use iso_c_binding, only: c_ptr, c_funloc
  ! We will use those GTK functions and values. The "only" statement can improve
  ! significantly the compilation time:
  use gtk, only: gtk_init, gtk_window_new, gtk_window_set_default_size, &
               & gtk_window_set_title, g_signal_connect, gtk_main, &
               & gtk_widget_show_all, c_null_char, &
               & gtk_menu_item_new_with_label, gtk_menu_bar_new, gtk_menu_new, &
               & gtk_menu_item_set_submenu, gtk_menu_shell_append, &
               & gtk_container_add, &
               & gtk_separator_menu_item_new, &
               & gtk_menu_item_new_with_mnemonic, gtk_menu_item_set_use_underline, TRUE, &
               & gtk_accel_group_new, gtk_window_add_accel_group, &
               & gtk_widget_add_accelerator, gtk_menu_set_accel_group, &
               & GTK_ACCEL_VISIBLE, GDK_CONTROL_MASK

  use handlers

  implicit none
  ! Pointers toward our GTK widgets:
  type(c_ptr) :: window
  type(c_ptr) :: menubar
  type(c_ptr) :: file_item, help_item
  type(c_ptr) :: fileMenu, helpMenu, recentFilesMenu
  type(c_ptr) :: new_item, open_item, recent_item, separator, quit_item, about_item
  type(c_ptr) :: file1_item, file2_item, file3_item
  type(c_ptr) :: group

  ! First, the GTK library must be initialized:
  call gtk_init()
  ! If you want to pass to GTK some command line arguments, see gtk.f90.

  ! Create the window and set up some signals for it.
  window = gtk_window_new()
  ! Not compulsory, but can be used if you want a larger window:
  call gtk_window_set_default_size(window, 400, 40)

  ! Don't forget that C strings must end with a null char:
  call gtk_window_set_title(window, "A GTK window with a menu"//c_null_char)

  ! We create the menu bar:
  menubar = gtk_menu_bar_new()

  ! and the main menus in that bar:
  fileMenu = gtk_menu_new()
  helpMenu = gtk_menu_new()
  
  file_item = gtk_menu_item_new_with_label("File"//c_null_char)
  help_item = gtk_menu_item_new_with_label("Help"//c_null_char)
  
  call gtk_menu_item_set_submenu(file_item, fileMenu)
  call gtk_menu_item_set_submenu(help_item, helpMenu)
  
  ! Let's append the items to the menu Bar
  call gtk_menu_shell_append(menubar, file_item)
  call gtk_menu_shell_append(menubar, help_item)

  ! Let's define the submenus of the File menu:
  new_item = gtk_menu_item_new_with_label("New"//c_null_char)
  open_item = gtk_menu_item_new_with_label("Open"//c_null_char)
  recent_item = gtk_menu_item_new_with_label("Recent files"//c_null_char)
  separator = gtk_separator_menu_item_new()
  ! For that one, we define a mnemonic with an underscore prefix:
  quit_item = gtk_menu_item_new_with_mnemonic("_Quit"//c_null_char)
  call gtk_menu_item_set_use_underline(quit_item, TRUE)
  ! Let's append those items to the File menu :
  call gtk_menu_shell_append(fileMenu, new_item)
  call gtk_menu_shell_append(fileMenu, open_item)
  call gtk_menu_shell_append(fileMenu, recent_item)
  call gtk_menu_shell_append(fileMenu, separator)
  call gtk_menu_shell_append(fileMenu, quit_item)
  ! and let's define the corresponding actions:
  call g_signal_connect(new_item, "activate"//c_null_char, c_funloc(file_new))
  call g_signal_connect(open_item, "activate"//c_null_char, c_funloc(file_open))
  call g_signal_connect(quit_item, "activate"//c_null_char, c_funloc(destroy))
  
  ! Let's define the File>Recent files submenu:
  recentFilesMenu = gtk_menu_new()
  call gtk_menu_item_set_submenu(recent_item, recentFilesMenu)
  file1_item = gtk_menu_item_new_with_label("First file"//c_null_char)
  file2_item = gtk_menu_item_new_with_label("Second file"//c_null_char)
  file3_item = gtk_menu_item_new_with_label("Third file"//c_null_char)  
  call gtk_menu_shell_append(recentFilesMenu, file1_item)
  call gtk_menu_shell_append(recentFilesMenu, file2_item)
  call gtk_menu_shell_append(recentFilesMenu, file3_item)

  ! Let's define the submenu of the Help menu:
  about_item = gtk_menu_item_new_with_label("About"//c_null_char)
  call gtk_menu_shell_append(helpMenu, about_item)
  ! and the corresponding action:
  call g_signal_connect(about_item, "activate"//c_null_char, c_funloc(help_about))

  ! Let's create a keyboard accelerator group:
  group = gtk_accel_group_new()
  call gtk_window_add_accel_group(window, group)
  call gtk_menu_set_accel_group(fileMenu, group)
  ! so that we can quit the program using CTRL+Q (81 ascii code):
  call gtk_widget_add_accelerator(quit_item, "activate"//c_null_char, group, 81, &
      & GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE)
    
  ! We add the menu bar to the window: 
  call gtk_container_add(window, menubar)
  
  ! Let's define two events for that window. The c_funloc() function returns
  ! the C address of the callback function.
  ! When you click on the cross to exit the program or close it from the window menu:
  call g_signal_connect(window, "delete-event"//c_null_char, c_funloc(delete_event))
  ! Before quitting the GTK main loop:
  call g_signal_connect(window, "destroy"//c_null_char, c_funloc(destroy))

  ! If you don't show it, nothing will appear on screen...
  call gtk_widget_show_all(window)

  ! Now, the events will be handled by the main GTK loop:
  call gtk_main()

  print *, "You have exited the GTK main loop, bye, bye..."

end program menu2
