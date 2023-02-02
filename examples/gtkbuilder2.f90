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
! You should have received a copy of the GNU General Public License along with
! this program; see the files LICENSE and LICENSE_EXCEPTION respectively.
! If not, see <http://www.gnu.org/licenses/>.
!
! Example using GtkBuilder
! Jens Hunger, 04-01-2011
! Last modified: vmagnin 2020-06-20 (GTK 4 version), 2022-07-20

module widgets
  use, intrinsic :: iso_c_binding

  implicit none
  ! Declares the used GTK widgets:
  type(c_ptr) :: window, builder
  ! A GTK/GLib main loop:
  type(c_ptr) :: my_gmainloop
end module


module handlers
  use, intrinsic :: iso_c_binding
  use g, only: g_main_loop_quit
  use widgets, only: my_gmainloop

  implicit none

contains
  !*************************************
  ! User defined event handlers go here
  !*************************************

  ! "destroy" is a GtkObject signal
  subroutine destroy(widget, gdata) bind(c)
    !GCC$ ATTRIBUTES DLLEXPORT :: destroy
    type(c_ptr), value, intent(in) :: widget, gdata

    print *, "My destroy!"
    call g_main_loop_quit(my_gmainloop)
  end subroutine destroy

  ! "clicked" is a GtkButton signal
  subroutine hello(widget, gdata) bind(c)
    !GCC$ ATTRIBUTES DLLEXPORT :: hello
    type(c_ptr), value, intent(in) :: widget, gdata

    print *, "Hello World!"
  end subroutine hello

  subroutine button1clicked(widget, gdata) bind(c)
    !GCC$ ATTRIBUTES DLLEXPORT :: button1clicked
    type(c_ptr), value, intent(in) :: widget, gdata

    print *, "Button 1 clicked!"
  end subroutine button1clicked

  subroutine button2clicked(widget, gdata) bind(c)
    !GCC$ ATTRIBUTES DLLEXPORT :: button2clicked
    type(c_ptr), value, intent(in) :: widget, gdata

    print *, "Button 2 clicked!"
  end subroutine button2clicked
end module handlers


program gtkbuilder
  use gtk, only: gtk_init, gtk_builder_new_from_file, gtk_builder_get_object, gtk_widget_show, FALSE
  use g, only: g_object_unref, g_main_loop_new, g_main_loop_run
  use widgets

  implicit none

  ! Initialize the GTK Library:
  call gtk_init()

  ! Create a new GtkBuilder object, parse the file 'gtkbuilder.ui'
  ! (generated with Cambalache) and add its content:
  builder = gtk_builder_new_from_file("gtkbuilder.ui"//c_null_char)

  ! Get a pointer to the GObject "window" from GtkBuilder:
  window = gtk_builder_get_object(builder, "window"//c_null_char)

  ! Free all memory used by XML stuff:
  call g_object_unref(builder)

  ! Show the Application Window:
  call gtk_widget_show(window)

  ! Create and enter the GTK/GLib main loop:
  my_gmainloop = g_main_loop_new(c_null_ptr, FALSE)
  call g_main_loop_run(my_gmainloop)

end program gtkbuilder
