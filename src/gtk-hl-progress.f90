! Copyright (C) 2011
! Free Software Foundation, Inc.
!
! This file is part of the gtk-fortran GTK Fortran Interface library.
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
! Contributed by James Tappin
! Last modification: 2011-11-21, vmagnin 2020-06-25
!------------------------------------------------------------------------------

!*
! Progress Bar
module gtk_hl_progress
  ! Implements the GtkProgressBar widget. Includes the facility to
  ! make a bar display "n of m" as well as the usual fraction.
  !/

  use gtk_sup
  use, intrinsic :: iso_c_binding
  ! Autogenerated use's
  use gtk, only: gtk_orientable_set_orientation, gtk_progress_bar_new, &
       & gtk_progress_bar_pulse, gtk_progress_bar_set_fraction,&
       & gtk_progress_bar_set_pulse_step,&
       & gtk_progress_bar_set_text, &
       & gtk_progress_bar_set_show_text, &
       & gtk_progress_bar_set_inverted, &
       & GTK_ORIENTATION_VERTICAL,  GTK_ORIENTATION_HORIZONTAL, &
       & TRUE, FALSE

  implicit none

  ! A progress bar value can be given as a fraction or m of n
  interface hl_gtk_progress_bar_set
     module procedure  hl_gtk_progress_bar_set_f
     module procedure  hl_gtk_progress_bar_set_ii
  end interface hl_gtk_progress_bar_set

contains

  !+
  function hl_gtk_progress_bar_new(vertical, reversed, step) result(bar)

    type(c_ptr) :: bar
    integer(c_int), optional :: vertical, reversed
    real(c_double), optional :: step

    ! Intializer for a progress bar
    !
    ! VERTICAL: boolean: optional: The orientation of the bar.
    ! REVERSED: boolean: optional: Whether the direction of the bar should
    ! 		be reversed.
    ! STEP: double: optional: The fractional step to advance when
    ! 		pulsing the bar
    !-

    bar = gtk_progress_bar_new()

    if (present(vertical)) then
       if (vertical == TRUE) then
          call gtk_orientable_set_orientation (bar, &
               & GTK_ORIENTATION_VERTICAL)
       else
          call gtk_orientable_set_orientation (bar, &
               & GTK_ORIENTATION_HORIZONTAL)
       end if
    end if

    if (present(reversed)) call gtk_progress_bar_set_inverted(bar, reversed)

    if (present(step)) &
         & call gtk_progress_bar_set_pulse_step(bar, step)

  end function hl_gtk_progress_bar_new

  !+
  subroutine hl_gtk_progress_bar_set_f(bar, val, string, text)

    type(c_ptr) :: bar
    real(c_double), optional :: val
    integer(c_int), optional :: string
    character(len=*), intent(in), optional:: text

    ! Set the value of a progress bar (fraction or pulse)
    !
    ! BAR: c_ptr: required: The bar to set
    ! VAL: double: optional: The value to set. If absent, the bar is pulsed
    ! STRING: boolean: optional: Whether to put a string on the bar.
    ! TEXT: string: optional: Text to put in the bar, (overrides STRING)
    !
    ! This routine is normally accessed via the generic interface
    ! hl_gtk_progress_bar_set
    !-

    character(len=50) :: sval

    ! If no value given pulse the bar
    if (.not. present(val)) then
       call gtk_progress_bar_pulse(bar)
    else
       ! Determine the fraction to fill & fill it
       call gtk_progress_bar_set_fraction(bar, val)
    end if

    ! If annotation is needed, add it.
    if (present(text)) then
       call gtk_progress_bar_set_text (bar, text//c_null_char)
       call gtk_progress_bar_set_show_text(bar, TRUE)
    else if (present(string)) then
       if (string == FALSE .or. .not. present(val)) return
       ! Otherwise we display a percentage
       write(sval, "(F5.1,'%')") val*100.

       call gtk_progress_bar_set_text (bar, trim(sval)//c_null_char)
       call gtk_progress_bar_set_show_text(bar, TRUE)
    else
       call gtk_progress_bar_set_show_text(bar, FALSE)
    end if
  end subroutine hl_gtk_progress_bar_set_f

  !+
  subroutine hl_gtk_progress_bar_set_ii(bar, val, maxv, string, text)

    type(c_ptr) :: bar
    integer(c_int) :: val, maxv
    integer(c_int), optional :: string
    character(len=*), intent(in), optional:: text

    ! Set the value of a progress bar (n of m)
    !
    ! BAR: c_ptr: required: The bar to set
    ! VAL: int: required: The value to set.
    ! MAXV: int: required: The maximum value for the bar
    ! STRING: boolean: optional: Whether to put a string on the bar.
    ! TEXT: string: optional: Text to put in the bar, (overrides STRING)
    !
    ! This routine is normally accessed via the generic interface
    ! hl_gtk_progress_bar_set
    !-

    real(c_double) :: frac
    character(len=50) :: sval

    frac = real(val,c_double)/real(maxv,c_double)
    call gtk_progress_bar_set_fraction(bar, frac)

    ! If annotation is needed, add it.
    if (present(text)) then
       call gtk_progress_bar_set_text (bar, text//c_null_char)
       call gtk_progress_bar_set_show_text(bar, TRUE)
    else if (present(string)) then
       if (string == FALSE) return
       ! Otherwise we display n or m
       write(sval, "(I0,' of ',I0)") val, maxv
       call gtk_progress_bar_set_text (bar, trim(sval)//c_null_char)
       call gtk_progress_bar_set_show_text(bar, TRUE)
    else
       call gtk_progress_bar_set_show_text(bar, FALSE)
    end if
  end subroutine hl_gtk_progress_bar_set_ii
end module gtk_hl_progress

