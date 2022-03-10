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
! Vincent MAGNIN, 2020-02-18
! Last modified: 2020-02-18
! This example just creates an GTK application with an empty window.
! https://developer.gnome.org/gtk3/unstable/gtk-getting-started.html
! https://developer.gnome.org/gio//2.30/GApplication.html
!-------------------------------------------------------------------------------

!*************************************
! User defined event handlers go here
!*************************************
module handlers

  use, intrinsic :: iso_c_binding, only: c_ptr, c_int
  use gtk, only: FALSE, c_null_char, gtk_window_set_default_size,  &
               & gtk_window_set_title, g_signal_connect, &
               & gtk_widget_show_all, gtk_application_window_new
  implicit none

contains
  ! Callback function for the signal "activate" emitted by g_application_run().
  ! We use a subroutine because it should return void.
  ! The GUI is defined here.
  subroutine activate(app, gdata) bind(c)
    type(c_ptr), value :: app, gdata
    type(c_ptr)        :: window
    
    ! Create the window:
    window = gtk_application_window_new(app)
    ! Not compulsory, but can be used if you want a larger window:
    call gtk_window_set_default_size(window, 300, 200)
    ! Don't forget that C strings must end with a null char:
    call gtk_window_set_title(window, "Hello GLib & GTK world!"//c_null_char)
    ! If you don't show it, nothing will appear on screen...
    call gtk_widget_show_all(window)  
  end subroutine activate

end module handlers

!*********************************************************************************************
! In the main program, we declare the GTK application, connect it to its "activate" function
! where we will create the GUI, and finally call the GLib main loop.
!*********************************************************************************************
program gtkzero

  use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc
  ! We will use those GTK functions and values. The "only" statement can improve
  ! significantly the compilation time:
  use gtk, only: c_null_char, c_null_ptr, gtk_application_new, G_APPLICATION_FLAGS_NONE
  use g, only: g_application_run, g_object_unref
  
  use handlers

  implicit none
  integer(c_int)     :: status
  type(c_ptr)        :: app

  ! First, let's create a GTK application (it will initialize GTK).
  ! The application ID must contain at least one point:
  ! https://developer.gnome.org/gio//2.30/GApplication.html#g-application-id-is-valid
  app = gtk_application_new("gtk-fortran.examples.gtkzero"//c_null_char, &
                            & G_APPLICATION_FLAGS_NONE)
  ! The activate signal will be sent by g_application_run(). 
  ! The c_funloc() function returns the C address of the callback function.
  ! The c_null_ptr means no data is transfered to the callback function.
  call g_signal_connect(app, "activate"//c_null_char, c_funloc(activate), c_null_ptr)
  ! Now, the whole application will be managed by GLib (=> main loop).
  ! Note that commandline arguments argc, argv are not passed.
  ! https://developer.gnome.org/gio//2.30/GApplication.html#g-application-run
  status = g_application_run(app, 0_c_int, [c_null_ptr])
  
  print *, "You have exited the GLib main loop, bye, bye..."
  
  ! Memory is freed:
  call g_object_unref(app)

end program gtkzero
