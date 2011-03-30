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

! You should have received a copy of the GNU General Public
!  License along with
! this program; see the files COPYING3 and COPYING.RUNTIME respectively.
! If not, see <http://www.gnu.org/licenses/>.
!
! Contributed by James Tappin
! Last modification: 03-21-2011

module gtk_sup
  ! The definitions in this supplementary module are ones that are not (yet)
  !  automatically extracted from the GTK headers.

  ! Currently it contains:
  ! GTYPE: Definitions of the integer length and the values for each type.
  ! GtkTreeIter: Type definition.
  ! GValue: Pseudo type definition.

  use iso_c_binding
  use gtk

  implicit none

  ! Gtype definitions
  integer, parameter :: type_kind=c_long
  integer(kind=c_int), parameter :: g_type_fundamental_shift=2
  integer(kind=type_kind), parameter :: G_TYPE_INVALID = &
       & ishft(0_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_NONE = &
       & ishft(1_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_INTERFACE = &
       & ishft(2_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_CHAR = &
       & ishft(3_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_UCHAR = &
       & ishft(4_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_BOOLEAN = &
       & ishft(5_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_INT = &
       & ishft(6_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_UINT = &
       & ishft(7_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_LONG = &
       & ishft(8_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_ULONG = &
       & ishft(9_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_INT64 = &
       & ishft(10_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_UINT64 = &
       & ishft(11_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_ENUM = &
       & ishft(12_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_FLAGS = &
       & ishft(13_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_FLOAT = &
       & ishft(14_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_DOUBLE = &
       & ishft(15_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_STRING = &
       & ishft(16_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_POINTER = &
       & ishft(17_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_BOXED = &
       & ishft(18_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_PARAM = &
       & ishft(19_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_OBJECT = &
       & ishft(20_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_VARIANT = &
       & ishft(21_type_kind, g_type_fundamental_shift)


  ! Define a GtkTreeIter (this has to be pre-allocated in the calls)
  type, bind(c) :: gtktreeiter
     integer(kind=c_int) :: intv=0
     type(c_ptr) :: p0=NULL, p1=NULL, p2=NULL
  end type gtktreeiter

  ! Define a spacemaker for GValue It's 24 bytes on 64 bit & 20 on 32,
  ! i.e. one long and 2 64-bit integers
  type, bind(c) :: gvalue
     integer(kind=c_long) :: il = 0
     integer(kind=c_int64_t), dimension(2) :: i64 = (/0, 0/)
  end type gvalue

contains
  ! These 2 clear_ routines are only needed of you need to re-initialize
  ! the types. The definitions include the intial setting to zero or NULL.
  subroutine clear_gtktreeiter(iter)
    type(gtktreeiter), intent(inout) :: iter

    iter%intv=0
    iter%p0=NULL
    iter%p1=NULL
    iter%p2=NULL
  end subroutine clear_gtktreeiter
  subroutine clear_gvalue(gval)
    type(gvalue) :: gval
    gval%il=0
    gval%i64=(/0,0/)
  end subroutine clear_gvalue
end module gtk_sup
