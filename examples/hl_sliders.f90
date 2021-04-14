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
! Last modification: vmagnin 2020-05-28 (GTK 4), 2020-07-14
!------------------------------------------------------------------------------

module handlers
  use gtk_hl_container
  use gtk_hl_button
  use gtk_hl_spin_slider
  use gtk, only: gtk_button_new, gtk_window_set_child, &
       & gtk_spin_button_get_value, gtk_spin_button_new, &
       & gtk_spin_button_set_value, gtk_window_destroy, &
       & gtk_widget_show, TRUE, FALSE
 
  implicit none
  type(c_ptr) :: base, box, slid, islid, win, qbut, spin, ispin

contains
  subroutine my_destroy(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata
    print *, "Exit called"
    call gtk_window_destroy(win)
  end subroutine my_destroy

  subroutine slider1(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata
    ! Moved the float slider, set the int spinner and report
    real(kind=c_double) :: val

    val = hl_gtk_slider_get_value(widget)
    print *, "FP slider moved to", val
    call hl_gtk_spin_button_set_value(spin, val)
  end subroutine slider1

  subroutine slider2(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata
    ! Moved the int slider, set the int spinner and report
    integer(kind=c_int) :: ival

    ival = nint(hl_gtk_slider_get_value(widget), c_int)
    print *, 'INT slider moved to', ival
    call hl_gtk_spin_button_set_value(ispin, ival)
  end subroutine slider2

  subroutine spinner1(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata
    ! Moved the FP spinner, set the FP slider & report
    real(kind=c_double) :: val

    val = hl_gtk_spin_button_get_value(widget)
    print *, "FP spinner moved to", val
    call hl_gtk_slider_set_value(slid, val)
  end subroutine spinner1

  subroutine spinner2(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata
    ! Move the int spinner, set the int slider & report
    integer(kind=c_int) :: ival

    ival = nint(hl_gtk_spin_button_get_value(widget), c_int)
    print *, 'INT spinner moved to', ival
    call hl_gtk_slider_set_value(islid, ival)
  end subroutine spinner2

  subroutine activate(app, gdata) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char
    use gtk, only: gtk_application_window_new, gtk_window_set_title
    implicit none
    type(c_ptr), value, intent(in)  :: app, gdata

    ! Create a window and 2 boxes, one horizontal,one vertical
    win = gtk_application_window_new(app)
    call gtk_window_set_title(win, "Sliders demo"//c_null_char)

    base = hl_gtk_box_new()
    call gtk_window_set_child(win, base)
    box = hl_gtk_box_new(horizontal=TRUE, homogeneous=TRUE)
    call hl_gtk_box_pack(base, box)

    ! make a floating point vertical slider with a range 0-10 and step 0.1
    ! put it in the horizontal box
    slid = hl_gtk_slider_new(0._c_double, 10._c_double, 0.1_c_double, &
         & vertical = TRUE, value_changed=c_funloc(slider1), length=200_c_int)
    call hl_gtk_box_pack(box, slid)

    ! Now an integer slider from 0-64 and put it in the horizontal box
    islid = hl_gtk_slider_new(0_c_int, 64_c_int, vertical=TRUE, &
         & value_changed=c_funloc(slider2), length=200_c_int)
    call hl_gtk_box_pack(box, islid)

    ! Make a spin button with range 0-10 and step 0.1 and put it in
    ! the vertical box
    spin = hl_gtk_spin_button_new(0._c_double, 10._c_double, 0.1_c_double, &
         & value_changed=c_funloc(spinner1))
    call hl_gtk_box_pack(base, spin)

    ! Make an integer spin button with range from 0-64 and put it in the
    ! vertical box
    ispin = hl_gtk_spin_button_new(0_c_int, 64_c_int, &
         & value_changed=c_funloc(spinner2), &
         & wrap=TRUE)
    call hl_gtk_box_pack(base, ispin)

    ! Finally make a quit button, put that in the vertical box and put
    ! the vertical box in the window.

    qbut = hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(my_destroy))
    call hl_gtk_box_pack(base, qbut)

    ! Realize & enter event loop
    call gtk_widget_show(win)
  end subroutine activate
end module handlers


program sliders
  ! SLIDERS
  ! Demo of sliders & spin buttons
  use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char
  use handlers

  implicit none
  type(c_ptr)        :: app

  app = hl_gtk_application_new("gtk-fortran.examples.hl_sliders"//c_null_char, &
                             & c_funloc(activate))
end program sliders

