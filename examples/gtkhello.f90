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
! Vincent MAGNIN, 2020-05-12, based on gtkzero_gapp.f90
! Last modified: 2021-01-22
! A GTK application with two buttons.
! https://developer.gnome.org/gtk4/unstable/gtk-getting-started.html
! https://developer.gnome.org/gio/stable/GApplication.html
!-------------------------------------------------------------------------------

!*************************************
! User defined event handlers go here
!*************************************
! Note that events are a special type of signals, coming from 
! the X Window system. Callback functions must have an event argument.
module handlers

  use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_funloc, c_null_char
  use gtk, only: FALSE, gtk_window_set_default_size,  &
               & gtk_window_set_title, gtk_window_destroy, &
               & g_signal_connect, g_signal_connect_swapped, &
               & gtk_widget_show, gtk_application_window_new, &
               & gtk_box_new, gtk_box_append, gtk_window_set_child, &
               & GTK_ORIENTATION_VERTICAL, GTK_ORIENTATION_HORIZONTAL, &
               & gtk_button_new_with_label, gtk_button_new_with_mnemonic, &
               & gtk_widget_set_margin_start, gtk_widget_set_margin_end, &
               & gtk_widget_set_margin_top, gtk_widget_set_margin_bottom
  implicit none

contains
  ! Callback function for the signal "activate" emitted by g_application_run().
  ! We use a subroutine because it should return void.
  ! The GUI is defined here.
  subroutine activate(app, gdata) bind(c)
    type(c_ptr), value, intent(in)  :: app, gdata
    ! Pointers toward our GTK widgets:
    type(c_ptr) :: window
    type(c_ptr) :: box
    type(c_ptr) :: button1, button2

    ! Create the window:
    window = gtk_application_window_new(app)
    ! Not compulsory, but can be used if you want a larger window:
    !call gtk_window_set_default_size(window, 300, 200)
    ! Don't forget that C strings must end with a null char:
    call gtk_window_set_title(window, "Hello GLib & GTK world!"//c_null_char)

    !******************************************************************
    ! Adding widgets in the window:
    !******************************************************************
    ! You need a box where to arrange your buttons, separated by 10 pixels:
    box = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10_c_int);
    ! Perhaps you prefer a vertical organization:
    !box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10_c_int);
    ! Set the border width (10 pixels) around the box:
    call gtk_widget_set_margin_start (box, 10_c_int)
    call gtk_widget_set_margin_end (box, 10_c_int)
    call gtk_widget_set_margin_top (box, 10_c_int)
    call gtk_widget_set_margin_bottom (box, 10_c_int)
    ! You need a container where to put the box, it will manage layout:
    call gtk_window_set_child(window, box)

    ! It's easy to create a button:
    button1 = gtk_button_new_with_label("I say hello"//c_null_char)
    ! Let's pack the button in the box:
    call gtk_box_append(box, button1)
    ! You can associate one or several callback functions with the button,
    ! "clicked" is a GtkButton signal emitted when you click on it:
    call g_signal_connect(button1, "clicked"//c_null_char, &
                        & c_funloc(button1clicked))
    call g_signal_connect(button1, "clicked"//c_null_char, c_funloc(hello))

    ! For that second button, there is an ALT+g keyboard shortcut:
    button2 = gtk_button_new_with_mnemonic("I don't know why you say _goodbye"&
                                          &//c_null_char)
    ! Let's pack the second button in the box (from left to right):
    call gtk_box_append(box, button2)
    ! Let's associate one callback function when that button is clicked.
    ! Here the gtk_window_destroy() function will be applied
    ! to window instead of button2:
    call g_signal_connect_swapped(button2, "clicked"//c_null_char, &
                                & c_funloc(gtk_window_destroy), window)
    !******************************************************************

    ! If you don't show it, nothing will appear on screen...
    call gtk_widget_show(window)

  end subroutine activate

  ! The two callback functions for the button1, here subroutines
  ! because the C prototype returns void:
  ! void user_function (GtkButton *button, gpointer   user_data)
  ! https://developer.gnome.org/gtk4/stable/GtkButton.html#GtkButton-clicked
  subroutine hello(widget, gdata) bind(c)
    type(c_ptr), value, intent(in)  :: widget, gdata

    print *, "So I say Hello GTK World!"
  end subroutine hello

  subroutine button1clicked(widget, gdata) bind(c)
    type(c_ptr), value, intent(in)  :: widget, gdata

    print *, "Button 1 clicked!"
  end subroutine button1clicked

end module handlers

!*******************************************************************************
! In the main program, we declare the GTK application, connect it to its 
! "activate" function where we will create the GUI, 
! and finally call the GLib main loop.
!*******************************************************************************
program gtkhello

  use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char, c_null_ptr
  ! We will use those GTK functions and values. The "only" statement can improve
  ! significantly the compilation time:
  use gtk, only: gtk_application_new, G_APPLICATION_FLAGS_NONE
  use g, only: g_application_run, g_object_unref
  use handlers

  implicit none
  integer(c_int)     :: status
  type(c_ptr)        :: app

  ! First, let's create a GTK application (it will initialize GTK).
  ! The application ID must contain at least one point:
  ! https://developer.gnome.org/gio/stable/GApplication.html#g-application-id-is-valid
  app = gtk_application_new("gtk-fortran.examples.gtkhello"//c_null_char, &
                            & G_APPLICATION_FLAGS_NONE)
  ! The activate signal will be sent by g_application_run(). 
  ! The c_funloc() function returns the C address of the callback function.
  ! The c_null_ptr means no data is transfered to the callback function.
  call g_signal_connect(app, "activate"//c_null_char, c_funloc(activate), &
                      & c_null_ptr)
  ! Now, the whole application will be managed by GLib (=> main loop).
  ! Note that commandline arguments argc, argv are not passed.
  ! https://developer.gnome.org/gio/stable/GApplication.html#g-application-run
  status = g_application_run(app, 0_c_int, [c_null_ptr])

  print *, "You have exited the GLib main loop, bye, bye..."

  ! Memory is freed:
  call g_object_unref(app)

end program gtkhello

