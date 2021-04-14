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
! Vincent MAGNIN et al. (derived from gtkhello2.f90), 2019-02-15
! Last modified: 2020-02-12
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
  subroutine destroy(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    print *, "My destroy"
    call gtk_main_quit()
  end subroutine destroy

  function hello(widget, gdata) result(ret)  bind(c)
    integer(c_int)     :: ret
    type(c_ptr), value :: widget, gdata

    print *, "So I say Hello GTK World!"
    ret = FALSE
  end function hello

  function button1clicked(widget, gdata) result(ret)  bind(c)
    integer(c_int)     :: ret
    type(c_ptr), value :: widget, gdata

    print *, "Button 1 clicked!"
    ret = FALSE
  end function button1clicked

end module handlers

!*********************************************************************************************
! In the main program, we initialize GTK, we design the UI, and finally call the GTK main loop
! For more information on GTK functions: https://developer.gnome.org/gtk3/stable/
!*********************************************************************************************
program gtkhello

  use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_int
  ! We will use those GTK functions and values. The "only" statement can improve
  ! significantly the compilation time:
  use gtk, only: gtk_init, gtk_window_new, gtk_window_set_default_size, gtk_window_set_title, &
               & g_signal_connect, gtk_box_new, gtk_container_add, &
               & gtk_button_new_with_label, gtk_button_new_with_mnemonic, &
               & gtk_widget_show, gtk_widget_show_all, gtk_main, &
               & GTK_WINDOW_TOPLEVEL, c_null_char, GTK_ORIENTATION_HORIZONTAL,&
               & gtk_widget_set_margin_start, gtk_widget_set_margin_end, &
               & gtk_widget_set_margin_top, gtk_widget_set_margin_bottom
  use handlers

  implicit none
  ! Pointers toward our GTK widgets:
  type(c_ptr) :: window
  type(c_ptr) :: box
  type(c_ptr) :: button1, button2

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

  !******************************************************************
  ! Widgets in the window:
  !******************************************************************
  ! You need a box where to arrange your buttons, separated by 10 pixels:
  box = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10_c_int);
  ! Perhaps you prefer a vertical organization:
  !box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10_c_int);
  ! This example can not be built with GTK 2 because gtk_box_nex() appeared
  ! in GTK3.
  ! Set the border width (10 pixels) around the box:
  call gtk_widget_set_margin_start (box, 10_c_int)
  call gtk_widget_set_margin_end (box, 10_c_int)
  call gtk_widget_set_margin_top (box, 10_c_int)
  call gtk_widget_set_margin_bottom (box, 10_c_int)
  ! You need a container where to put the box, it will manage layout:
  call gtk_container_add(window, box)
  
  ! It's easy to create a button:
  button1 = gtk_button_new_with_label("I say hello"//c_null_char)
  ! Let's pack the button in the box.
  call gtk_container_add(box, button1)
  ! Let's associate two callback functions when that button is clicked:
  call g_signal_connect(button1, "clicked"//c_null_char, c_funloc(button1clicked))
  ! "clicked" is a GtkButton signal emitted when you click on a button:
  call g_signal_connect(button1, "clicked"//c_null_char, c_funloc(hello))

  ! For that button, there is an ALT+g keyboard shortcut:
  button2 = gtk_button_new_with_mnemonic("I don't know why you say _goodbye"//c_null_char)
  ! Let's pack the second button in the box (packing occurs from left to right):
  call gtk_container_add(box, button2)
  ! Let's associate one callback function when that button is clicked:
  call g_signal_connect(button2, "clicked"//c_null_char, c_funloc(destroy))
  !******************************************************************

  ! If you don't show it, nothing will appear on screen...
  call gtk_widget_show_all(window)
  ! You could also show the widgets one by one by calling gtk_widget_show()
  ! for button1, button2, box and window.

  ! Now, the events will be handled by the main GTK loop:
  call gtk_main()

  print *, "You have exited the GTK main loop, bye, bye..."

end program gtkhello
