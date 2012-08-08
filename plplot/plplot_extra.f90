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

  ! The operation codes for pl_cmd
  integer, parameter :: PLESC_SET_RGB          = 1      ! obsolete
  integer, parameter :: PLESC_ALLOC_NCOL       = 2      ! obsolete
  integer, parameter :: PLESC_SET_LPB          = 3      ! obsolete
  integer, parameter :: PLESC_EXPOSE           = 4      ! handle window expose
  integer, parameter :: PLESC_RESIZE           = 5      ! handle window resize
  integer, parameter :: PLESC_REDRAW           = 6      ! handle window redraw
  integer, parameter :: PLESC_TEXT             = 7      ! switch to text screen
  integer, parameter :: PLESC_GRAPH            = 8      ! switch to graphics screen
  integer, parameter :: PLESC_FILL             = 9      ! fill polygon
  integer, parameter :: PLESC_DI               = 10     ! handle DI command
  integer, parameter :: PLESC_FLUSH            = 11     ! flush output
  integer, parameter :: PLESC_EH               = 12     ! handle Window events
  integer, parameter :: PLESC_GETC             = 13     ! get cursor position
  integer, parameter :: PLESC_SWIN             = 14     ! set window parameters
  integer, parameter :: PLESC_DOUBLEBUFFERING  = 15     ! configure double buffering
  integer, parameter :: PLESC_XORMOD           = 16     ! set xor mode
  integer, parameter :: PLESC_SET_COMPRESSION  = 17     ! AFR: set compression
  integer, parameter :: PLESC_CLEAR            = 18     ! RL: clear graphics region
  integer, parameter :: PLESC_DASH             = 19     ! RL: draw dashed line
  integer, parameter :: PLESC_HAS_TEXT         = 20     ! driver draws text
  integer, parameter :: PLESC_IMAGE            = 21     ! handle image
  integer, parameter :: PLESC_IMAGEOPS         = 22     ! plimage related operations
  integer, parameter :: PLESC_PL2DEVCOL        = 23     ! convert PLColor to device color
  integer, parameter :: PLESC_DEV2PLCOL        = 24     ! convert device color to PLColor
  integer, parameter :: PLESC_SETBGFG          = 25     ! set BG, FG colors
  integer, parameter :: PLESC_DEVINIT          = 26     ! alternate device initialization
  integer, parameter :: PLESC_GETBACKEND       = 27     ! get used backend of (wxWidgets) driver
  integer, parameter :: PLESC_BEGIN_TEXT       = 28     ! get ready to draw a line of text
  integer, parameter :: PLESC_TEXT_CHAR        = 29     ! render a character of text
  integer, parameter :: PLESC_CONTROL_CHAR     = 30     ! handle a text control character= super/subscript, etc.)
  integer, parameter :: PLESC_END_TEXT         = 31     ! finish a drawing a line of text
  integer, parameter :: PLESC_START_RASTERIZE  = 32     ! start rasterized rendering
  integer, parameter :: PLESC_END_RASTERIZE    = 33     ! end rasterized rendering
  integer, parameter :: PLESC_ARC              = 34     ! render an arc
  integer, parameter :: PLESC_GRADIENT         = 35     ! render a gradient
  integer, parameter :: PLESC_MODESET          = 36     ! set drawing mode
  integer, parameter :: PLESC_MODEGET          = 37     ! get drawing mode

  !      Plotting options for 3d plots, see plplot.h for the C definitions
  !      of these options.

  ! As of (at worst) plplot 5.9.9 these definitions are in the standard plplot
  ! modules. They are left in so that users with an older version (e.g. 5.9.7)
  ! can uncomment them.

!!$  integer, parameter :: DRAW_LINEX = 1
!!$  integer, parameter :: DRAW_LINEY = 2
!!$  integer, parameter :: DRAW_LINEXY = 3
!!$  integer, parameter :: MAG_COLOR = 4
!!$  integer, parameter :: BASE_CONT = 8
!!$  integer, parameter :: TOP_CONT = 16
!!$  integer, parameter :: SURF_CONT = 32
!!$  integer, parameter :: DRAW_SIDES = 64
!!$  integer, parameter :: FACETED = 128
!!$  integer, parameter :: MESH = 256

  ! Interface for the pl_cmd routine
  interface
     subroutine pl_cmd(cmd, arg) bind(c)
       use iso_c_binding, only: c_int, c_ptr
       integer(kind=c_int), value :: cmd
       type(c_ptr), value :: arg
     end subroutine pl_cmd
  end interface
end module plplot_extra
