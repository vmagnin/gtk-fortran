! Copyright (C) 2011
! Free Software Foundation, Inc.

! This file is part of the gtk-fortran GTK / Fortran Interface library.

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
! Vincent MAGNIN et al. (derived from gtkhello2.f90), 02-15-2019
! Last modified: 02-15-2019
! gfortran -I../src ../src/gtk.f90 gtkhello.f90 `pkg-config --cflags --libs gtk+-3.0` -Wall -Wextra -pedantic -std=f2003


!*************************************
! User defined event handlers go here
!*************************************
! Note that events are a special type of signals, coming from the X Window system.
! Then callback functions must have an event argument.
module handlers
  use gtk, only: gtk_main_quit, FALSE
  implicit none

contains
  ! Will be call if you click on the cross to exit the program, or if you close it from the window menu:
  function delete_event(widget, event, gdata) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, event, gdata
    print *, "My delete_event"
    ret = FALSE
  end function delete_event

  ! "destroy" is a GtkObject signal
  subroutine destroy(widget, gdata) bind(c)
    use iso_c_binding, only: c_ptr
    type(c_ptr), value :: widget, gdata
    print *, "My destroy"
    call gtk_main_quit ()
  end subroutine destroy

  ! "clicked" is a GtkButton signal
  function hello(widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    print *, "So I say Hello GTK World!"
    ret = FALSE
  end function hello

  function button1clicked(widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    print *, "Button 1 clicked!"
    ret = FALSE
  end function button1clicked

end module handlers

!*********************************************************************************************
! In the main program, we initialize GTK, we design the UI, and finally call the GTK main loop
!*********************************************************************************************
program gtkhello
  ! The "only" statement can divide the compilation time by a factor 10 !
  use iso_c_binding, only: c_ptr, c_funloc, c_loc, c_int
  use gtk, only: gtk_init, gtk_window_new, GTK_WINDOW_TOPLEVEL, gtk_window_set_title, &
               & gtk_container_set_border_width, g_signal_connect, gtk_box_new, gtk_container_add, &
               & gtk_button_new_with_label, gtk_box_pack_start, gtk_widget_show, gtk_main, FALSE, &
               & c_null_char, GTK_ORIENTATION_HORIZONTAL, gtk_window_set_default_size, GTK_ORIENTATION_VERTICAL,&
               & gtk_widget_show_all
  use handlers

  implicit none
  type(c_ptr) :: window
  type(c_ptr) :: box
  type(c_ptr) :: button1, button2

  ! First, the GTK library must be initialized:
  call gtk_init()

  ! Create the window and set up some signals for it.
  window = gtk_window_new(GTK_WINDOW_TOPLEVEL)
  ! Not compulsory, but can be used if you want a larger window:
  !call gtk_window_set_default_size(window, 400, 50)

  ! Don't forget that C strings must end with a null char:
  call gtk_window_set_title(window, "Hello GTK world!"//c_null_char)
  
  call g_signal_connect(window, "delete-event"//c_null_char, c_funloc(delete_event))
  call g_signal_connect(window, "destroy"//c_null_char, c_funloc(destroy))

  !******************************************************************
  ! You need a box where to put your buttons, separated by 10 pixels:
  box = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10_c_int);
  ! Perhaps you prefer a vertical organization:
  !box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10_c_int);

  call gtk_container_add(window, box)
  ! It's prettier with a 10 pixels border:
  call gtk_container_set_border_width(window, 10_c_int)
  
  button1 = gtk_button_new_with_label("I say hello"//c_null_char)
  call gtk_box_pack_start(box, button1, FALSE, FALSE, 0_c_int)
  call g_signal_connect(button1, "clicked"//c_null_char, c_funloc(button1clicked))
  call g_signal_connect(button1, "clicked"//c_null_char, c_funloc(hello))
  ! If you don't show it, it will not appear in the window...
  !call gtk_widget_show(button1)

  button2 = gtk_button_new_with_label("I don't know why you say goodbye"//c_null_char)
  call gtk_box_pack_start(box, button2, FALSE, FALSE, 0_c_int)
  call g_signal_connect(button2, "clicked"//c_null_char, c_funloc(destroy))
  !call gtk_widget_show(button2)
  !call gtk_widget_show(box)
  !******************************************************************
  ! Even the window does not appear until it is shown...
  !call gtk_widget_show(window)
  
  call gtk_widget_show_all(window)

  ! Now, everything will be handled by the main GTK loop:
  call gtk_main()

  print *, "You have exited the GTK main loop, bye, bye..."

end program gtkhello
