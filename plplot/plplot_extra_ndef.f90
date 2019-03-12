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
! Contributed by: James Tappin
! Last modification: 8/2/2012

module plplot_extra
  ! Plplot extras for accessing the pl_cmd routine.

  use iso_c_binding

  implicit none

  ! The operation codes for pl_cmd were removed because
  ! in plplot>=5.9.8 these definitions are in the F95 binding

  ! Interface for the pl_cmd routine
  interface
     subroutine pl_cmd(cmd, arg) bind(c)
       use iso_c_binding, only: c_int, c_ptr
       integer(kind=c_int), value :: cmd
       type(c_ptr), value :: arg
     end subroutine pl_cmd
  end interface
end module plplot_extra
