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
! You should have received a copy of the GNU General Public License along with
! this program; see the files COPYING3 and COPYING.RUNTIME respectively.
! If not, see <http://www.gnu.org/licenses/>.
!------------------------------------------------------------------------------
! Jens Hunger, 03-27-2011
! Last modified: vmagnin 2020-06-20 (GTK 4 version), 2021-01-22
!------------------------------------------------------------------------------

module widgets
  use iso_c_binding

  implicit none
  ! declares the used GTK widgets
  type(c_ptr) :: window
  type(c_ptr) :: builder
  type(c_ptr) :: my_gmainloop
end module

module handlers
  use widgets

  use gtk, only: gtk_builder_add_from_file, gtk_builder_get_object, &
  & gtk_builder_new, gtk_widget_show, FALSE, gtk_init

  use g, only: g_object_unref, &
             & g_main_loop_new, g_main_loop_run, g_main_loop_quit

  implicit none

contains
  !*************************************
  ! User defined event handlers go here
  !*************************************

  ! "destroy" is a GtkObject signal
  subroutine destroy (widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata

    print *, "my destroy"
    call g_main_loop_quit (my_gmainloop)
  end subroutine destroy

  ! "clicked" is a GtkButton signal
  subroutine hello (widget, gdata ) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata

    print *, "Hello World!"
  end subroutine hello

  subroutine button1clicked (widget, gdata ) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata

    print *, "Button 1 clicked!"
  end subroutine button1clicked

  subroutine button2clicked (widget, gdata ) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata
    integer, pointer :: val

    print *, "Button 2 clicked!"

    if (c_associated(gdata)) then
       call c_f_pointer(gdata, val)
       print *, "Value =", val
       val = val + 1
    end if
  end subroutine button2clicked
end module handlers


program gtkbuilder
  use connect

  implicit none
  integer(c_int) :: guint
  type(c_ptr) :: error
  error = c_null_ptr

  ! Initialize the GTK Library
  call gtk_init ()

  ! create a new GtkBuilder object
  builder = gtk_builder_new ()

  ! parse the Glade3 XML file 'gtkbuilder.glade' and add
  ! its contents to the GtkBuilder object
  guint = gtk_builder_add_from_file (builder, &
                         & "gtkbuilder.glade"//c_null_char, error)
  if (guint == 0) then
     print *, "Could not open gtkbuilder.glade"
     stop
  end if

  ! get a pointer to the GObject "window" from GtkBuilder
  window = gtk_builder_get_object (builder, "window"//c_null_char)

  ! free all memory used by XML stuff
  call g_object_unref (builder)
  
  ! Show the Application Window
  call gtk_widget_show (window)
  
  ! Enter the GTK Main Loop
  my_gmainloop = g_main_loop_new(c_null_ptr, FALSE)
  call g_main_loop_run(my_gmainloop)
end program gtkbuilder

