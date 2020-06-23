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
!
! You should have received a copy of the GNU General Public License along with
! this program; see the files COPYING3 and COPYING.RUNTIME respectively.
! If not, see <http://www.gnu.org/licenses/>.
!------------------------------------------------------------------------------
! Contributed by James Tappin.
! Last modification: vmagnin 2020-06-23 (GTK 4 version)
!------------------------------------------------------------------------------

module rb_handlers
!  use gtk_hl
  use gtk_hl_container
  use gtk_hl_button
  use gtk, only: gtk_button_new, gtk_window_set_child, &
               & gtk_radio_button_new, gtk_toggle_button_get_active, &
               & gtk_widget_show

  implicit none
  type(c_ptr) :: box, window, qbut, group
  type(c_ptr), dimension(6) :: rbut

contains

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


  subroutine activate(app, gdata) bind(c)
    use gtk, only: gtk_application_window_new, gtk_window_destroy, &
                 & g_signal_connect_swapped

    type(c_ptr), value, intent(in)  :: app, gdata
    ! Pointers toward our GTK widgets:
    type(c_ptr) :: window
    integer :: i
    integer(kind=c_int), dimension(6), target :: isel=[ (i-1,i=1,6) ]
    character(len=10) :: label

    ! Create the window:
    window = gtk_application_window_new(app)

    ! Create a vertical box
    box = hl_gtk_box_new(homogeneous=TRUE)
    call gtk_window_set_child(window, box)

    ! make 6 radio buttons and put them into the box (the group is
    ! the list item that links the buttons together and is used for
    ! collective operations on the set of buttons. The group must be set to
    ! a NULL pointer before entry otherwise the internals may get confused.)
    group = c_null_ptr

    do i=1,6
       write(label,"('Choice #',i0)") i-1
       rbut(i) = hl_gtk_radio_button_new(group, trim(label)//c_null_char, &
            & toggled=c_funloc(rb_toggle), data=c_loc(isel(i)))
       call hl_gtk_box_pack(box, rbut(i))
    end do

    ! Set a selection (3)
    call hl_gtk_radio_group_set_select(group, 3_c_int)

    ! Make a "quit" button:
    qbut = hl_gtk_button_new('Quit'//c_null_char)
    ! This button is special, it will be connected to a callback
    ! function of the window. So we can not use the "clicked" argumunt
    ! of hl_gtk_button_new():
    call g_signal_connect_swapped(qbut, "clicked"//c_null_char, &
                                & c_funloc(gtk_window_destroy), window)

    ! Put the box in the window
    call hl_gtk_box_pack(box, qbut)

    ! Realize the hierarchy
    call gtk_widget_show(window)
  end subroutine activate
end module rb_handlers


program radio
  ! RADIO
  ! Demo of a radio button group
  ! https://developer.gnome.org/gtk4/stable/GtkRadioButton.html

  use iso_c_binding, only: c_ptr, c_funloc, c_null_char, c_null_ptr
  use gtk, only: gtk_application_new, G_APPLICATION_FLAGS_NONE
  use g, only: g_application_run, g_object_unref
  use rb_handlers

  implicit none
  integer(c_int)     :: status
  type(c_ptr)        :: app

  app = gtk_application_new("gtk-fortran.examples.hl_radio"//c_null_char, &
                            & G_APPLICATION_FLAGS_NONE)
  call g_signal_connect(app, "activate"//c_null_char, c_funloc(activate), &
                      & c_null_ptr)
  status = g_application_run(app, 0_c_int, c_null_ptr)

  print *, "You have exited the GLib main loop, bye, bye..."

  call g_object_unref(app)
end program radio

