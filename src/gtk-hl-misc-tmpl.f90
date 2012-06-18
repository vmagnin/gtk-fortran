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

!!$T Template file for gtk-hl-misc.f90.
!!$T  Make edits to this file, and keep them identical between the
!!$T  GTK2 & GTK3 branches.

!!$T Lines to be used only in the GTK2 tree should be prefixed with !!$2
!!$T Lines to be used only in the GTK3 tree should be prefixed with !!$3
!!$T The mk_gtk_hl.pl script should be used to generate the source file.

module gtk_hl_misc
  !*
  ! Misc utility functions
  ! Assorted functions and subroutines that don't fit anywhere else.
  !/

  use gtk_sup

  use iso_c_binding
  use iso_fortran_env, only: error_unit

  use gtk, only: gtk_bin_get_child, gtk_label_set_markup,&
!!$2       & gtk_hseparator_new, gtk_vseparator_new, &
!!$3       & gtk_separator_new, &
!!$3       & GTK_ORIENTATION_HORIZONTAL, GTK_ORIENTATION_VERTICAL, &
       & TRUE, FALSE

  implicit none

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

  !+
  function hl_gtk_separator_new(vertical) result(spacer)
    type(c_ptr) :: spacer
    integer(kind=c_int), intent(in), optional :: vertical

    ! A Version-independent separator.
    !
    ! HORIZONTAL: boolean : optional : Set to TRUE to make
    ! 		a vertical separator.
    !-

!!$2    logical :: is_vertical
!!$3    integer(kind=c_int) :: direction

!!$2    if (present(vertical)) then
!!$2       is_vertical = c_f_logical(vertical)
!!$2    else
!!$2       is_vertical = .false.
!!$2    end if
!!$2
!!$2    if (is_vertical) then
!!$2       spacer = gtk_vseparator_new()
!!$2    else
!!$2       spacer = gtk_hseparator_new()
!!$2    end if

!!$3    if (present(vertical)) then
!!$3       if (c_f_logical(vertical)) then
!!$3          direction = GTK_ORIENTATION_VERTICAL
!!$3       else
!!$3          direction = GTK_ORIENTATION_HORIZONTAL
!!$3       end if
!!$3    else
!!$3       direction = GTK_ORIENTATION_HORIZONTAL
!!$3    end if
!!$3
!!$3    spacer = gtk_separator_new(direction)

  end function hl_gtk_separator_new
end module gtk_hl_misc
