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
! this program; see the files LICENSE and LICENSE_EXCEPTION respectively.
! If not, see <http://www.gnu.org/licenses/>.
!-------------------------------------------------------------------------------
! Vincent MAGNIN, 02-27-2019
! Last modified: 02-27-2019
! This example just creates an empty GTK window.
!-------------------------------------------------------------------------------

!*************************************
! User defined event handlers go here
!*************************************
! Note that events are a special type of signals, coming from the X Window system.
! Callback functions must have an event argument.
module handlers

  use, intrinsic :: iso_c_binding, only: c_ptr, c_int
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

end module handlers

!*********************************************************************************************
! In the main program, we initialize GTK, we design the UI, and finally call the GTK main loop
! For more information on GTK functions: https://developer.gnome.org/gtk3/stable/
!*********************************************************************************************
program gtkzero

  use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc
  ! We will use those GTK functions and values. The "only" statement can improve
  ! significantly the compilation time:
  use gtk, only: gtk_init, gtk_window_new, gtk_window_set_default_size, &
               & gtk_window_set_title, g_signal_connect, gtk_main, &
               & gtk_widget_show_all, GTK_WINDOW_TOPLEVEL, c_null_char
  use handlers

  implicit none
  ! Pointers toward our GTK widgets:
  type(c_ptr) :: window

  ! First, the GTK library must be initialized:
  call gtk_init()
  ! If you want to pass to GTK some command line arguments, see gtk.f90.

  ! Create the window and set up some signals for it.
  window = gtk_window_new(GTK_WINDOW_TOPLEVEL)
  ! Not compulsory, but can be used if you want a larger window:
  !call gtk_window_set_default_size(window, 400, 50)

  ! Don't forget that C strings must end with a null char:
  call gtk_window_set_title(window, "Hello GTK world!"//c_null_char)

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

end program gtkzero
