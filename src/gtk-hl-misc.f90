! Copyright (C) 2011
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

! You should have received a copy of the GNU General Public License along with
! this program; see the files COPYING3 and COPYING.RUNTIME respectively.
! If not, see <http://www.gnu.org/licenses/>.
!
! Contributed by James Tappin
! Last modification: 12-1-2011

! --------------------------------------------------------
! gtk-hl-misc.f90
! Generated: Thu Dec  1 21:27:37 2011 GMT
! Please do not edit this file directly,
! Edit gtk-hl-misc-tmpl.f90, and use ./mk_gtk_hl.pl to regenerate.
! --------------------------------------------------------


module gtk_hl_misc
  !*
  ! Misc utility functions
  ! Assorted functions and subroutines that don't fit anywhere else.
  !/

  use gtk_sup

  use iso_c_binding
  use iso_fortran_env, only: error_unit

  use gtk, only: gtk_bin_get_child, gtk_label_set_markup,&
       & TRUE, FALSE, CNULL, NULL

contains

  !+
  subroutine hl_gtk_bin_set_label_markup(widget, label)
    type(c_ptr) :: widget
    character(kind=c_char), dimension(*), intent(in) :: label

    ! Add a markup label to a button or menu item.
    !
    ! WIDGET: c_ptr: required: The widget to update.
    ! LABEL: string: required: The new label string.
    !-

    type(c_ptr) :: label_w

    label_w = gtk_bin_get_child(widget)
    if (.not. c_associated(label_w)) then
       write(error_unit, *) &
            & "ERROR: hl_gtk_bin_set_label_markup: specified widget does not have a label"
       return
    end if

    call gtk_label_set_markup(label_w, label)

  end subroutine hl_gtk_bin_set_label_markup

end module gtk_hl_misc
