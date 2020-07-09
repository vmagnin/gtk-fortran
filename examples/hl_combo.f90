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
!------------------------------------------------------------------------------
! Contributed by James Tappin.
! Last modification: vmagnin 2020-05-28 (GTK 4 version), 2020-07-09
!------------------------------------------------------------------------------

module handlers
  use gtk_hl_container
  use gtk_hl_combobox
  use gtk_hl_button
  use gtk, only: gtk_button_new, gtk_combo_box_get_active, gtk_combo_box_new, &
       & gtk_window_set_child, gtk_window_destroy, &
       & gtk_widget_show, TRUE, FALSE

  implicit none
  type(c_ptr) :: win, box, c1, c2, qbut

contains

  subroutine my_destroy(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata

    print *, "Exit called"
    call gtk_window_destroy(win)
  end subroutine my_destroy

  subroutine c_change(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata

    integer, pointer :: index
    integer(kind=c_int) :: isel, nrow
    character(len=40) :: value

    if (c_associated(gdata)) then
       call c_f_pointer(gdata, index)
       print "('Box:',I2)", index
    end if

    isel = hl_gtk_combo_box_get_active(widget, ftext=value)
    nrow = hl_gtk_combo_box_n_entries(widget)

    print "('Choice:',I2,' of ',i2,' Text:',a)", isel, nrow, trim(value)
  end subroutine c_change


  subroutine activate(app, gdata) bind(c)
    use iso_c_binding, only: c_ptr, c_funloc, c_null_char
    use gtk, only: gtk_application_window_new, gtk_window_set_title
    implicit none
    type(c_ptr), value, intent(in)  :: app, gdata
    character(len=20), dimension(8) :: list1
    character(len=20) :: item2
    integer :: i
    integer, target :: d1=1, d2=2

    ! Create the window:
    win = gtk_application_window_new(app)
    call gtk_window_set_title(win, "Combo box demo"//c_null_char)

    ! Column box
    box = hl_gtk_box_new()
    call gtk_window_set_child(win, box)

    ! A list with an entry box
    do i = 1, 8
       write(list1(i), "('Item # ',I0)") i-1
    end do
    c1 = hl_gtk_combo_box_new(has_entry=TRUE, changed=c_funloc(c_change), &
         & initial_choices=list1, data=c_loc(d1))
    call hl_gtk_box_pack(box, c1)

    ! One without
    c2 = hl_gtk_combo_box_new(changed=c_funloc(c_change), data=c_loc(d2))
    call hl_gtk_box_pack(box, c2)

    do i = 1, 5
       write(item2, "('Choice Number',I2)") i
       call hl_gtk_combo_box_add_text(c2, trim(item2)//c_null_char, at_start=TRUE)
    end do

    ! Quit button
    qbut = hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(my_destroy))
    call hl_gtk_box_pack(box, qbut)

    ! Realize & enter event loop
    call gtk_widget_show(win)
  end subroutine activate
end module handlers


program combo_demo
  use iso_c_binding, only: c_ptr, c_funloc, c_null_char
  use handlers

  implicit none
  type(c_ptr)        :: app

  app = hl_gtk_application_new("gtk-fortran.examples.hl_combo"//c_null_char, &
                             & c_funloc(activate))
end program combo_demo

