! This file is part of gtk-fortran, a GTK / Fortran interface library.
! Copyright (C) 2012 The gtk-fortran team
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
! this program; see the files LICENSE and LICENSE_EXCEPTION respectively.
! If not, see <http://www.gnu.org/licenses/>.
!-------------------------------------------------------------------------------
! Contributed by: James Tappin, last modification: 2012-02-08
! vmagnin, 2022-04-15, 2023-03-10
!-------------------------------------------------------------------------------

module plplot_extra
  implicit none

  ! Interface for the PLplot pl_cmd() routine, not available
  ! in the Fortran binding, but in the plplot.h header file:
  interface
    ! Front-end to driver escape function. In principle this can be used
    ! to pass just about anything directly to the driver:
    subroutine pl_cmd(cmd, arg) bind(c)
      use, intrinsic :: iso_c_binding, only: c_int, c_ptr
      implicit none
      ! A PLESC command to pass to the driver:
      integer(c_int), value :: cmd
      ! Data associated with the cmd command:
      type(c_ptr), value :: arg
    end subroutine pl_cmd
  end interface
end module plplot_extra
