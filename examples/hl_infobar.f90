! Copyright (C) 2012
! Free Software Foundation, Inc.

! This file is part of the gtk-fortran GTK+ Fortran Interface library.

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

! You should have received a copy of the GNU General Public
!  License along with
! this program; see the files COPYING3 and COPYING.RUNTIME respectively.
! If not, see <http://www.gnu.org/licenses/>.
!
! Contributed by James Tappin
! Last modification: 08-30-2012


! A demo of the HL infobar widget.

module ib_handers

  use gtk_hl
  use gtk, only: gtk_main, gtk_main_quit, gtk_widget_get_no_show_all, &
       & gtk_widget_hide, gtk_widget_show_all, gtk_init, TRUE, FALSE, &
       & GTK_FILL, GTK_MESSAGE_INFO, GTK_MESSAGE_WARNING, &
       & GTK_MESSAGE_QUESTION, GTK_MESSAGE_ERROR, GTK_MESSAGE_OTHER

  implicit none

  type(c_ptr) :: tlwindow, infobar

  enum, bind(c)
  enumerator :: my_quit
  enumerator :: my_no
  enumerator :: my_yes
  enumerator :: my_ok
  enumerator :: my_print
  enumerator :: my_ignore
end enum

contains
subroutine delete_h(widget, data) bind(c)
 type(c_ptr), value :: widget, data

 print *, "Exit called"
 call gtk_main_quit()
end subroutine delete_h

subroutine response_h(widget, id, data) bind(c)
 type(c_ptr), value :: widget, data
 integer(kind=c_int), value :: id

 select case(id)
 case(my_quit)
    print *, "Pressed 'QUIT'"
    call gtk_main_quit()
    return
 case(my_yes)
    print *, "Pressed 'YES'"
 case(my_no)
    print *, "Pressed 'NO'"
 case(my_ok)
    print *, "Pressed 'OK'"
 case(my_ignore)
 end select

 if (c_f_logical(gtk_widget_get_no_show_all(widget))) &
      & call gtk_widget_hide(widget)

end subroutine response_h

subroutine button_h(widget, data) bind(c)
 type(c_ptr), value :: widget, data

 integer(kind=c_int), pointer :: index
 integer(kind=c_int), parameter, dimension(5) :: ids=[my_ok, my_yes, &
      & my_no, my_ignore, my_quit]

 call c_f_pointer(data, index)

 select case (index)
 case(0)
    call hl_gtk_info_bar_message(infobar, &
         & "For your informatation"//c_null_char, &
         & type=GTK_MESSAGE_INFO, ids=ids, &
         & state=[TRUE, FALSE, FALSE, TRUE, FALSE], &
         & default=my_ok)
 case(1)
    call hl_gtk_info_bar_message(infobar, &
         & "Take care"//c_null_char, &
         & type=GTK_MESSAGE_WARNING, ids=ids, &
         & state=[TRUE, FALSE, FALSE, TRUE, FALSE], &
         & default=my_ignore)
 case(2)
    call hl_gtk_info_bar_message(infobar, &
         & "This is BAD!!!!"//c_null_char, &
         & type=GTK_MESSAGE_ERROR, ids=ids, &
         & state=[FALSE, FALSE, FALSE, TRUE, TRUE], &
         & default=my_quit)
 case(3)
    call hl_gtk_info_bar_message(infobar, &
         & "Are you certain"//c_null_char, &
         & type=GTK_MESSAGE_QUESTION, ids=ids, &
         & state=[FALSE, TRUE, TRUE, TRUE, FALSE], &
         & default=my_no)
 case(4)
    call hl_gtk_info_bar_message(infobar, &
         & "Something else again"//c_null_char, &
         & type=GTK_MESSAGE_OTHER, ids=ids, &
         & state=[TRUE, FALSE, FALSE, TRUE, TRUE], &
         & default=my_ignore)
 end select
end subroutine button_h

end module ib_handers

program hl_infobar
  use ib_handers
  implicit none

  integer(kind=c_int), dimension(5), target :: button_states = &
       & [0,1,2,3,4]
  type(c_ptr) :: jb, junk

  call gtk_init()
  tlwindow = hl_gtk_window_new("InfoBar Demo"//c_null_char, &
       & destroy = c_funloc(delete_h), resizable=FALSE)

  jb = hl_gtk_table_new()
  call gtk_container_add(tlwindow, jb)

  junk=hl_gtk_button_new("Info"//c_null_char, &
       & clicked=c_funloc(button_h), data=c_loc(button_states(1)))
  call hl_gtk_table_attach(jb, junk, 0_c_int, 0_c_int, yopts=0_c_int)
  junk=hl_gtk_button_new("Warning"//c_null_char, &
       & clicked=c_funloc(button_h), data=c_loc(button_states(2)))
  call hl_gtk_table_attach(jb, junk, 1_c_int, 0_c_int, yopts=0_c_int)
  junk=hl_gtk_button_new("Error"//c_null_char, &
       & clicked=c_funloc(button_h), data=c_loc(button_states(3)))
  call hl_gtk_table_attach(jb, junk, 2_c_int, 0_c_int, yopts=0_c_int)
  junk=hl_gtk_button_new("Question"//c_null_char, &
       & clicked=c_funloc(button_h), data=c_loc(button_states(4)))
  call hl_gtk_table_attach(jb, junk, 3_c_int, 0_c_int, yopts=0_c_int)
  junk=hl_gtk_button_new("Other"//c_null_char, &
       & clicked=c_funloc(button_h), data=c_loc(button_states(5)))
  call hl_gtk_table_attach(jb, junk, 4_c_int, 0_c_int, yopts=0_c_int)

  infobar = hl_gtk_info_bar_new(buttons=&
       & [character(len=6) :: 'OK','Yes','No','Ignore','Quit'], &
       & ids=[my_ok, my_yes, my_no, my_ignore, my_quit], &
       & response=c_funloc(response_h), horizontal=TRUE, buttons_below=TRUE)

  call hl_gtk_table_attach(jb,infobar,0_c_int,1_c_int, &
       & xspan=5_c_int, yopts=0_c_int)

  call gtk_widget_show_all (tlwindow)
  call gtk_main()

end program hl_infobar
