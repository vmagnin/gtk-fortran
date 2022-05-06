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
!
!==============================================================================
! Contributed by Vincent MAGNIN, 2022-05-06, last modified: 2022-05-06
!
! This program is testing functions of the gtk_sup module.
!==============================================================================

module tests
  use, intrinsic :: iso_c_binding
  use gtk, only: TRUE, FALSE, gtk_init, gtk_label_new, gtk_label_get_text
  use gtk_sup, only: c_f_string_copy, strlen, c_f_string_copy_alloc, fdate

  implicit none

contains

  integer function test_string_conversion_routines() result(errors)
    type(c_ptr) :: str_ptr, widget_ptr
    integer :: status
    character(len=128) :: fortran_string
    character(len=2)   :: short_fortran_string
    character(:), allocatable :: allocatable_fortran_string

    errors = 0

    widget_ptr = gtk_label_new("A label string"//c_null_char)
    str_ptr = gtk_label_get_text(widget_ptr)

    ! ------------------------------------------------
    print '(A)', ">> c_f_string_copy()"

    call c_f_string_copy(str_ptr, fortran_string, status)

    if (trim(fortran_string) /= "A label string") then
      print '(A)', "c_f_string_copy() does not return the expected Fortran string 'A label string'"
      errors = errors + 1
    end if

    if (len(trim(fortran_string)) /= strlen(str_ptr)) then
      print '(A)', "c_f_string_copy(): len() Fortran function and strlen() C function does not return the same value"
      errors = errors + 1
    end if

    call c_f_string_copy(str_ptr, short_fortran_string, status)

    if (status /= -1) then
      print '(A)', "c_f_string_copy() should have return status=-1 (Fortran string too short)"
      errors = errors + 1
    end if

    if (trim(short_fortran_string) /= "A ") then
      print '(A)', "c_f_string_copy() does not return the expected Fortran string 'A '"
      errors = errors + 1
    end if

    ! ------------------------------------------------
    print '(A)', ">> c_f_string_copy_alloc()"

    call c_f_string_copy_alloc(str_ptr, allocatable_fortran_string)

    if (trim(allocatable_fortran_string) /= "A label string") then
      print '(A)', "c_f_string_copy_alloc() does not return the expected Fortran string 'A label string'"
      errors = errors + 1
    end if

    if (len(trim(allocatable_fortran_string)) /= strlen(str_ptr)) then
      print '(A)', "c_f_string_copy_alloc(): len() Fortran function and strlen() C function does not return the same value"
      errors = errors + 1
    end if

  end function test_string_conversion_routines


  integer function test_date_routines() result(errors)
    character(:), allocatable :: today

    print '(A)', ">> fdate()"
    today = fdate()
    print '(A)', today

    errors = 0
  end function test_date_routines

end module tests


program tests_gtk_sup
  use tests

  implicit none
  integer :: errors

  call gtk_init()

  print '(A)', "Testing some gtk_sup functions..."

  print '(A)', "> test_string_conversion_routines()"
  errors = test_string_conversion_routines()
  print '(A)', "> test_date_routines()"
  errors = test_date_routines()

  print *
  if (errors == 0) then
    print '(A)', "No error"
  else
    print *, errors, "errors"
    print '(A)', "See the 'tests_errors.txt' file"
  end if
end program tests_gtk_sup
