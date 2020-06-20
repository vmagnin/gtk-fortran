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
!
! Example using GtkBuilder
! Jens Hunger, 04-01-2011
! Last modified: vmagnin 2020-06-20 (GTK 4 version)

module widgets
  ! declares the used GTK widgets
  use iso_c_binding, only: c_null_char, c_null_ptr, c_ptr, c_int

  implicit none
  type(c_ptr) :: window
  type(c_ptr) :: builder
end module

module handlers
  use gtk, only: gtk_builder_add_from_file, &
        & gtk_builder_get_object, gtk_builder_new, gtk_widget_show, &
        & FALSE, gtk_init
  use g, only: g_main_loop_new, g_main_loop_run, g_main_loop_quit, &
             & g_object_unref
  use widgets

  implicit none
  type(c_ptr)    :: my_gmainloop

contains
  !*************************************
  ! User defined event handlers go here
  !*************************************
  ! Note that events are a special type of signals, coming from the
  ! X Window system. Then callback functions must have an event argument:
  function delete_event (widget, event, gdata) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    !GCC$ ATTRIBUTES DLLEXPORT :: delete_event
    integer(c_int)    :: ret
    type(c_ptr), value, intent(in) :: widget, event, gdata

    print *, "my delete_event"
    ret = FALSE
  end function delete_event

  ! "destroy" is a GtkObject signal
  subroutine destroy (widget, gdata) bind(c)
    use iso_c_binding, only: c_ptr
    !GCC$ ATTRIBUTES DLLEXPORT :: destroy
    type(c_ptr), value, intent(in) :: widget, gdata

    print *, "my destroy"
    call g_main_loop_quit(my_gmainloop)
  end subroutine destroy

  ! "clicked" is a GtkButton signal
  function hello (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    !GCC$ ATTRIBUTES DLLEXPORT :: hello
    integer(c_int)    :: ret
    type(c_ptr), value, intent(in) :: widget, gdata

    print *, "Hello World!"
    ret = FALSE
  end function hello

  function button1clicked (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    !GCC$ ATTRIBUTES DLLEXPORT :: button1clicked
    integer(c_int)    :: ret
    type(c_ptr), value, intent(in) :: widget, gdata

    print *, "Button 1 clicked!"
    ret = FALSE
  end function button1clicked

  function button2clicked (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    !GCC$ ATTRIBUTES DLLEXPORT :: button2clicked
    integer(c_int)    :: ret
    type(c_ptr), value, intent(in) :: widget, gdata

    print *, "Button 2 clicked!"
    ret = FALSE
  end function button2clicked
end module handlers

program gtkbuilder
  use handlers
  
  implicit none
  integer(c_int) :: guint
  type(c_ptr) :: error

  error = c_null_ptr

  ! Initialize the GTK Library:
  call gtk_init ()

  ! Create a new GtkBuilder object:
  builder = gtk_builder_new ()

  ! parse the Glade3 XML file 'gtkbuilder.glade' and add
  ! it's contents to the GtkBuilder object
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
  
  ! Enter the Main Loop:
  my_gmainloop = g_main_loop_new(c_null_ptr, FALSE)
  call g_main_loop_run(my_gmainloop)

end program gtkbuilder
