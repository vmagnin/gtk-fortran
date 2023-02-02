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
! this program; see the files LICENSE and LICENSE_EXCEPTION respectively.
! If not, see <http://www.gnu.org/licenses/>.
!
! gfortran -g gtk.f90 gtk-sup.f90 gtk-hl.f90 hl_dialog.f90 `pkg-config --cflags --libs gtk+-2.0`
! Contributed by James Tappin.

module handlers
  use gtk_hl
  use gtk, only: gtk_button_new, gtk_container_add, gtk_main, gtk_main_quit, gtk_&
       &widget_destroy, gtk_widget_show, gtk_widget_show_all, gtk_window_new, &
       & gtk_init

  implicit none

  type(c_ptr) :: win, box, label

contains
  subroutine my_destroy(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata
    print *, "Exit called"
!    call gtk_widget_destroy(win)
    call gtk_main_quit ()
  end subroutine my_destroy

  subroutine msg_alert(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int) :: resp

    character(len=40), dimension(5) :: msg

    msg(1) = "ALERT"
    msg(2) = ""
    msg(3) = "You have pressed an alert button"
    msg(4) = ""
    msg(5) = "You know that's dangerous"

    resp = hl_gtk_message_dialog_show(msg, GTK_BUTTONS_OK, &
         & "ALERT"//c_null_char, &
         & type=GTK_MESSAGE_WARNING, parent=win)
  end subroutine msg_alert

  subroutine msg_quit(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int) :: resp

    character(len=40), dimension(3) :: msg

    msg(1) ="QUIT?"
    msg(2) = ""
    msg(3) = "Do you really want to quit?"

    resp = hl_gtk_message_dialog_show(msg, GTK_BUTTONS_YES_NO, &
         & "QUIT"//c_null_char, parent=win)
    if (resp == GTK_RESPONSE_YES) call gtk_main_quit()

  end subroutine msg_quit

  subroutine msg_about(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    call hl_gtk_about_dialog_gtk_fortran(win)

  end subroutine msg_about
end module handlers

program dialog_demo
  use handlers
  implicit none

  type(c_ptr) :: but

  call gtk_init()
  ! Make a window & put a horizontal box in it
  win = hl_gtk_window_new('Dialogue Demo'//c_null_char, &
       & destroy=c_funloc(my_destroy), &
       & border=10_c_int )
  box = hl_gtk_box_new(horizontal=TRUE, spacing=10_c_int)
  call gtk_container_add(win, box)

  ! 3 Buttons one shows a message, the next an about dialogue and the
  ! last a confirm exit dialog
  but = hl_gtk_button_new('Alert'//c_null_char, clicked=c_funloc(msg_alert))
  call hl_gtk_box_pack(box, but)

  but = hl_gtk_button_new("About"//c_null_char, clicked=c_funloc(msg_about))
  call hl_gtk_box_pack(box, but)

  but = hl_gtk_button_new('Quit'//c_null_char, clicked=c_funloc(msg_quit))
  call hl_gtk_box_pack(box, but)

  ! Display the window
  call gtk_widget_show_all(Win)

  call gtk_main()
end program dialog_demo
