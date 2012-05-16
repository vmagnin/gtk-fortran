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
! Last modification: 11-21-2011

!!$T Template file for gtk-hl-accelerator.f90.
!!$T  Make edits to this file, and keep them identical between the
!!$T  GTK2 & GTK3 branches.

!!$T Lines to be used only in the GTK2 tree should be prefixed with !!$2
!!$T Lines to be used only in the GTK3 tree should be prefixed with !!$3
!!$T The mk_gtk_hl.pl script should be used to generate the source file.

module gtk_hl_accelerator
  !*
  ! Accelerator
  ! A convenience function to add an accelerator to a widget (button or
  ! menu item). 
  !/

  use iso_c_binding

  ! Auto-generated use's
  use gtk, only: gtk_widget_add_accelerator, &
       & GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE, &
       & TRUE, FALSE

  use gdk, only: gdk_keyval_from_name


  implicit none

contains
  !+
  subroutine hl_gtk_widget_add_accelerator(widget, signal, accel_group, &
       & accel_key, accel_mods, accel_flags)
    type(c_ptr), intent(in) :: widget
    character(kind=c_char), dimension(*), intent(in) :: signal
    type(c_ptr), intent(in) :: accel_group
    character(kind=c_char), dimension(*), intent(in) :: accel_key
    integer(kind=c_int), intent(in), optional :: accel_mods, accel_flags
    
    ! Add an accelerator to a widget (just saves a lot of code duplication)
    !
    ! WIDGET: c_ptr: required: The widget with which the accelerator is
    ! 		associated.
    ! SIGNAL: string: required: The signal with which the accelerator is
    ! 		associated
    ! ACCEL_GROUP: c_ptr: required: The accelerator group to which the
    ! 		accelerator belongs (must have been created and added to
    ! 		the top-level window).
    ! ACCEL_KEY: string: required: The key name to use for the accelerator
    ! ACCEL_MODS: c_int: optional: The key modifiers for the accelerator,
    ! 		This defaults to GDK_CONTROL_MASK, set it to 0 to use the
    ! 		unmodified key.
    ! ACCEL_FLAGS: c_int: optional: Flags for the accelerator, if not present
    ! 		then GTK_ACCEL_VISIBLE, is used (to hide the accelerator,
    ! 		use ACCEL_FLAGS=0).
    !-

    integer(kind=c_int) :: ikey, imods, iflags

    ikey = gdk_keyval_from_name(accel_key)
    if (present(accel_mods)) then
       imods = accel_mods
    else
       imods = GDK_CONTROL_MASK
    end if
    if (present(accel_flags)) then
       iflags = accel_flags
    else
       iflags = GTK_ACCEL_VISIBLE
    end if
    call gtk_widget_add_accelerator(widget, signal, &
               & accel_group, ikey, imods, iflags)
  end subroutine hl_gtk_widget_add_accelerator
end module gtk_hl_accelerator
