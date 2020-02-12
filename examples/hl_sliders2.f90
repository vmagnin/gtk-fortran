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
! Contributed by James Tappin.
! Last modification: vmagnin 02-20-2019

module handlers
  use gtk_hl
  use gtk, only: gtk_button_new, gtk_container_add, gtk_main, gtk_main_quit, gtk_&
       &spin_button_get_value, gtk_spin_button_new, gtk_spin_button_set_value, gtk_wid&
       &get_destroy, gtk_widget_show, gtk_widget_show_all, gtk_window_new, &
       & TRUE, FALSE, gtk_init

  implicit none
  type(c_ptr) :: slid, win, qbut, spin

contains
  subroutine my_destroy(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    print *, "Exit called"
    call gtk_main_quit ()
  end subroutine my_destroy

  subroutine slider1(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata
    ! Moved the float slider

    real(kind=c_double) :: val

    val = hl_gtk_slider_get_value(widget)
    print *, "Slider moved to", val

  end subroutine slider1
  subroutine spin1(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata
    ! Moved the spinner

    real(kind=c_double) :: val

    val = hl_gtk_spin_button_get_value(widget)
    print *, "Spinner moved to", val
  end subroutine spin1

  subroutine set_upper(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    real(kind=c_double) :: ulim
    character(len=20) :: slim
    integer ::ios
    call hl_gtk_entry_get_text(widget, slim)

    read(slim, *, iostat=ios) ulim
    if (ios /= 0) then 
       print *, "Bad FP value: ", slim
       return
    else
       print *, "New upper limit:", ulim
    end if

    call hl_gtk_slider_set_range(slid, upper=ulim)
    call hl_gtk_spin_button_set_range(spin, upper=ulim)
  end subroutine set_upper

  subroutine set_lower(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    real(kind=c_double) :: llim
    character(len=20) :: slim
    integer ::ios
    call hl_gtk_entry_get_text(widget, slim)

    read(slim, *, iostat=ios) llim
    if (ios /= 0) then 
       print *, "Bad FP value: ", slim
       return
    else
       print *, "New lower limit:", llim
    end if

    call hl_gtk_slider_set_range(slid, lower=llim)
    call hl_gtk_spin_button_set_range(spin, lower=llim)
  end subroutine set_lower

end module handlers

program sliders_2

  ! SLIDERS_2
  ! Demo of sliders & spin buttons

  use handlers

  implicit none

  type(c_ptr) :: base, box, junk
  ! Initialize gtk
  call gtk_init()

  ! Create a window and 2 boxes, one horizontal,one vertical
  win = hl_gtk_window_new("Sliders demo"//c_null_char, destroy=c_funloc(my_destroy))
  base = hl_gtk_box_new()
  call gtk_container_add(win, base)

  ! make a floating point vertical slider with a range 0-10 and step 0.1
  ! put it in the box
  slid = hl_gtk_slider_new(0._c_double, 10._c_double, 0.1_c_double, &
       & vertical = FALSE, value_changed=c_funloc(slider1), length=200_c_int)
  call hl_gtk_box_pack(base, slid)


  ! Make a spin button with range 0-10 and step 0.1 and put it in
  ! the vertical box
  spin = hl_gtk_spin_button_new(0._c_double, 10._c_double, 0.1_c_double, &
       & value_changed=c_funloc(spin1))
  call hl_gtk_box_pack(base, spin)

  ! Upper and lower bound entry boxes
  box = hl_gtk_box_new(horizontal=TRUE)
  call hl_gtk_box_pack(base, box)

  junk = gtk_label_new("Lower bound:"//c_null_char)
  call hl_gtk_box_pack(box, junk)
  junk = hl_gtk_entry_new(value="0."//c_null_char, activate=c_funloc(set_lower))
  call hl_gtk_box_pack(box, junk)

  box = hl_gtk_box_new(horizontal=TRUE)
  call hl_gtk_box_pack(base, box)

  junk = gtk_label_new("Upper bound:"//c_null_char)
  call hl_gtk_box_pack(box, junk)
  junk = hl_gtk_entry_new(value="10."//c_null_char, activate=c_funloc(set_upper))
  call hl_gtk_box_pack(box, junk)

  ! Finally make a quit button, put that in the vertical box and put
  ! the vertical box in the window.
  qbut = hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(my_destroy))
  call hl_gtk_box_pack(base, qbut)

  ! Realize the hierarchy
  call gtk_widget_show_all(win)
  call gtk_main

end program sliders_2
