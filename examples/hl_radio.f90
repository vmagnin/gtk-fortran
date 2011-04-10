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
! gfortran -g gtk.f90 gtk-sup.f90 gtk-hl.f90 hl_radio.f90 `pkg-config --cflags --libs gtk+-2.0`
! Contributed by James Tappin.

module rb_handlers

  use gtk_hl
  use gtk, only: gtk_button_new, gtk_container_add, gtk_main, gtk_main_quit, gtk_&
       &object_destroy, gtk_radio_button_new, gtk_toggle_button_get_active, gtk_widget&
       &_show, gtk_widget_show_all, gtk_window_new, gtk_init

  implicit none

  type(c_ptr) :: box, window, qbut, group
  type(c_ptr),dimension(6) :: rbut

contains
  subroutine my_destroy(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata
    print *, "Exit called"
    call gtk_object_destroy(window)
    call gtk_main_quit ()
  end subroutine my_destroy

  subroutine rb_toggle(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int), pointer :: fdata
    integer(kind=c_int) :: sdata

    ! Don't do anything for the implicit release event.
    if (gtk_toggle_button_get_active(widget) == FALSE) then
       print *, "Autonomous release"
       return
    end if

    if (c_associated(gdata)) then
       call c_f_pointer(gdata, fdata)
       print *, "Selected", fdata
    end if
    sdata = hl_gtk_radio_group_get_select(group)
    print *, "Selection", sdata
  end subroutine rb_toggle

end module rb_handlers

program radio

  ! RADIO
  ! Demo of a radio button group

  ! use gtk_hl
  use rb_handlers

  implicit none
  integer :: i
  integer(kind=c_int), dimension(6), target :: isel=[ (i-1,i=1,6) ]
  character(len=10) :: label
  logical :: changed

  ! Initialize GTK+
  call gtk_init()

  ! Create a window and a vertical box
  window = hl_gtk_window_new('radios'//cnull, destroy=c_funloc(my_destroy))
  box = hl_gtk_box_new(homogeneous=TRUE)
  call gtk_container_add(window, box)

  ! make 6 radio buttons and put them into the box (the group is
  ! the list item that links the buttons together and is used for
  ! collective operations on the set of buttons. The group must be set to
  ! a NULL pointer before entry otherwise the internals may get confused.

  group=NULL

  do i=1,6
     write(label,"('Choice #',i0)") i-1
     rbut(i) = hl_gtk_radio_button_new(group, trim(label)//cnull, &
          & toggled=c_funloc(rb_toggle), data=c_loc(isel(i)))
     call hl_gtk_box_pack(box, rbut(i))
  end do

  ! Set a selection (3)
  call hl_gtk_radio_group_set_select(group, 3)

  ! Make a "quit" button and put it in the box as well, then put the
  ! box in the window
  qbut = hl_gtk_button_new('Quit'//cnull, clicked=c_funloc(my_destroy))
  call hl_gtk_box_pack(box, qbut)

  ! Realize the hierarchy
  call gtk_widget_show_all(window)

  ! Event loop

  call gtk_main

end program radio
