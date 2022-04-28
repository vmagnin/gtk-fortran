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
! Contributed by James Tappin, 2011
! Last modification: vmagnin 2020-05-28 (GTK 4)


!*
! Misc utility functions
module gtk_hl_misc
  ! Assorted functions and subroutines that don't fit anywhere else.
  !/

  use gtk_sup

  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: error_unit

  use gtk, only: gtk_label_set_markup, gtk_separator_new, &
       & GTK_ORIENTATION_HORIZONTAL, GTK_ORIENTATION_VERTICAL, &
       & TRUE, FALSE, gtk_widget_get_first_child

  implicit none

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! GtkBin has been removed in GTK 4
  !+
  subroutine hl_gtk_bin_set_label_markup(widget, label)
    type(c_ptr) :: widget
    character(kind=c_char), dimension(*), intent(in) :: label

    ! Add a markup label to a button or menu item.
    !
    ! WIDGET |  c_ptr |  required |  The widget to update.
    ! LABEL |  string |  required |  The new label string.
    !-

    type(c_ptr) :: label_w

    label_w = gtk_widget_get_first_child(widget)
    if (.not. c_associated(label_w)) then
       write(error_unit, *) &
            & "ERROR: hl_gtk_bin_set_label_markup: specified widget does not have a label"
       return
    end if

    call gtk_label_set_markup(label_w, label)

  end subroutine hl_gtk_bin_set_label_markup

  !+
  function hl_gtk_separator_new(vertical) result(spacer)
    type(c_ptr) :: spacer
    integer(c_int), intent(in), optional :: vertical

    ! A Version-independent separator.
    !
    ! HORIZONTAL |  boolean  |  optional  |  Set to TRUE to make a vertical separator.
    !-

    integer(c_int) :: direction

    if (present(vertical)) then
       if (c_f_logical(vertical)) then
          direction = GTK_ORIENTATION_VERTICAL
       else
          direction = GTK_ORIENTATION_HORIZONTAL
       end if
    else
       direction = GTK_ORIENTATION_HORIZONTAL
    end if
    spacer = gtk_separator_new(direction)

  end function hl_gtk_separator_new
end module gtk_hl_misc
