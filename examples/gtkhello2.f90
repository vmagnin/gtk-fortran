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
! gfortran gtk.f90 gtkhello2.f90 `pkg-config --cflags --libs gtk+-2.0`
! Jerry DeLisle , Tobias Burnus, and Vincent Magnin, 01-23-2011
! Data passing: James Tappin
! Last modified: 03-13-2011

module handlers
  use gtk, only: gtk_main_quit, FALSE
  implicit none

contains
  !*************************************
  ! User defined event handlers go here
  !*************************************
  ! Note that events are a special type of signals, coming from the
  ! X Window system. Then callback functions must have an event argument:
  function delete_event (widget, event, gdata) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, event, gdata
    print *, "my delete_event"
    ret = FALSE
  end function delete_event

  ! "destroy" is a GtkObject signal
  subroutine destroy (widget, gdata) bind(c)
    use iso_c_binding, only: c_ptr
    type(c_ptr), value :: widget, gdata
    print *, "my destroy"
    call gtk_main_quit ()
  end subroutine destroy

  ! "clicked" is a GtkButton signal
  function hello (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    print *, "Hello World!"
    ret = FALSE
  end function hello

  function button1clicked (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    print *, "Button 1 clicked!"
    ret = FALSE
  end function button1clicked

  function button2clicked (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int, c_associated, c_f_pointer
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata

    integer, pointer :: val

    print *, "Button 2 clicked!"
    ret = FALSE
    if (c_associated(gdata)) then
       call c_f_pointer(gdata, val)
       print *, "Value =", val
       val = val + 1
    end if
  end function button2clicked

end module handlers


program gtkFortran
  use iso_c_binding, only: c_ptr, c_funloc, c_loc
  use gtk, only: gtk_init, gtk_window_new, GTK_WINDOW_TOPLEVEL, gtk_window_set_title, &
      & gtk_container_set_border_width, g_signal_connect, gtk_box_new, gtk_container_add, &
      & gtk_button_new_with_label, gtk_box_pack_start, gtk_widget_show, gtk_main, FALSE, &
      & c_null_char, TRUE, GTK_ORIENTATION_HORIZONTAL
  ! The "only" statement can divide the compilation time by a factor 10 !
  use handlers
  implicit none

  type(c_ptr) :: window
  type(c_ptr) :: box1
  type(c_ptr) :: button1, button2, button3

  integer, target :: val = 4

  call gtk_init ()

  ! Create the window and set up some signals for it.
  window = gtk_window_new (GTK_WINDOW_TOPLEVEL)
  !call gtk_window_set_default_size(window, 500, 500)
  call gtk_window_set_title(window, "My title"//c_null_char)
  call gtk_container_set_border_width (window, 10)
  call g_signal_connect (window, "delete-event"//c_null_char, c_funloc(delete_event))
  call g_signal_connect (window, "destroy"//c_null_char, c_funloc(destroy))

  box1 = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 10);
  call gtk_container_add (window, box1)

  button1 = gtk_button_new_with_label ("Button1"//c_null_char)
  call gtk_box_pack_start (box1, button1, FALSE, FALSE, 0)
  call g_signal_connect (button1, "clicked"//c_null_char, c_funloc(button1clicked))
  call g_signal_connect (button1, "clicked"//c_null_char, c_funloc(hello))
  call gtk_widget_show (button1)

  ! This is an example of passing data to the callback function:
  button2 = gtk_button_new_with_label ("Button2"//c_null_char)
  call gtk_box_pack_start (box1, button2, FALSE, FALSE, 0)
  call g_signal_connect (button2, "clicked"//c_null_char, c_funloc(button2clicked), &
       & c_loc(val))
  call gtk_widget_show (button2)

  button3 = gtk_button_new_with_label ("Exit"//c_null_char)
  call gtk_box_pack_start (box1, button3, FALSE, FALSE, 0)
  call g_signal_connect (button3, "clicked"//c_null_char, c_funloc(destroy))
  call g_signal_connect (button3, "clicked"//c_null_char, c_funloc(hello))
  call gtk_widget_show (button3)

  call gtk_widget_show (box1)
  call gtk_widget_show (window)

  call gtk_main ()

end program gtkFortran
