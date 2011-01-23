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
! Interfaces contributed by Vincent Magnin, 01-23-2011

module cairo
  use iso_c_binding, only: c_int, c_bool, c_char, c_null_char, c_ptr, c_null_ptr
  implicit none

  ! cairo_format:
  integer(c_int), parameter :: CAIRO_FORMAT_INVALID   = -1
  integer(c_int), parameter :: CAIRO_FORMAT_ARGB32    = 0
  integer(c_int), parameter :: CAIRO_FORMAT_RGB24     = 1
  integer(c_int), parameter :: CAIRO_FORMAT_A8        = 2
  integer(c_int), parameter :: CAIRO_FORMAT_A1        = 3
  integer(c_int), parameter :: CAIRO_FORMAT_RGB16_565 = 4

  function gdk_cairo_create (drawable) bind(c)
    use iso_c_binding, only: c_ptr
    type(c_ptr), value :: drawable
    type(c_ptr) :: gdk_cairo_create
  end function

  function cairo_create (target0) bind(c)
    use iso_c_binding, only: c_ptr
    type(c_ptr), value :: target0
    type(c_ptr) :: cairo_create
  end function

  function cairo_image_surface_create (format0, width, height) bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int), value :: format0, width, height
    type(c_ptr) :: cairo_image_surface_create
  end function
  
  subroutine cairo_destroy(cr) bind(c)
    use iso_c_binding, only: c_ptr
    type(c_ptr), value :: cr
  end subroutine 

  subroutine cairo_stroke(cr) bind(c)
    use iso_c_binding, only: c_ptr
    type(c_ptr), value :: cr
  end subroutine 

  subroutine cairo_move_to(cr, x, y) bind(c)
    use iso_c_binding, only: c_ptr, c_double
    type(c_ptr), value :: cr
    real(c_double), value :: x, y
  end subroutine 
  
  subroutine cairo_line_to(cr, x, y) bind(c)
    use iso_c_binding, only: c_ptr, c_double
    type(c_ptr), value :: cr
    real(c_double), value :: x, y
  end subroutine 

  subroutine cairo_curve_to(cr, x1, y1, x2, y2, x3, y3) bind(c)
    use iso_c_binding, only: c_ptr, c_double
    type(c_ptr), value :: cr
    real(c_double), value :: x1, y1, x2, y2, x3, y3
  end subroutine 
  
  subroutine cairo_set_source_rgb(cr, red, green, blue) bind(c)
    use iso_c_binding, only: c_ptr, c_double
    type(c_ptr), value :: cr
    real(c_double), value :: red, green, blue
  end subroutine 

  subroutine cairo_set_line_width(cr, width) bind(c)
    use iso_c_binding, only: c_ptr, c_double
    type(c_ptr), value :: cr
    real(c_double), value :: width
  end subroutine 
end module cairo
