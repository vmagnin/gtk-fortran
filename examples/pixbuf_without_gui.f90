! Copyright (C) 2011
! Free Software Foundation, Inc.
!
! This file is part of the gtk-fortran GTK / Fortran Interface library.
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
! If not, see <http:!www.gnu.org/licenses/>.
!------------------------------------------------------------------------------
! Draw a Sierpinski triangle in a PNG file, without any GUI
! https://en.wikipedia.org/wiki/Sierpi%C5%84ski_triangle#Chaos_game
! Contributed by Vincent Magnin, 2020-05-26
!------------------------------------------------------------------------------

program pixbuf_without_gui
  use, intrinsic :: iso_fortran_env, only: wp=>real64
  use, intrinsic :: iso_c_binding, only: c_ptr, c_null_char, c_null_ptr, &
                         & c_f_pointer, c_char, c_int
  use gdk_pixbuf, only: gdk_pixbuf_get_n_channels, gdk_pixbuf_get_pixels, &
                      & gdk_pixbuf_get_rowstride, gdk_pixbuf_new
  use gtk, only: GDK_COLORSPACE_RGB, FALSE
  use gtk_os_dependent, only: gdk_pixbuf_savev

  implicit none
  type(c_ptr) :: my_pixbuf
  ! We use chars because we need unsigned integers:
  character(c_char), dimension(:), pointer :: pixel
  integer(c_int) :: nch, rowstride, pixwidth, pixheight
  integer(c_int) :: cstatus   ! Command status
  real(wp), dimension(1:3) :: x, y
  real(wp) :: xx, yy, diag, r
  integer  :: s            ! Triangle vertex number
  integer  :: n = 300000   ! Number of points
  integer  :: i, p

  ! We create a "pixbuffer" to store the pixels of the image.
  ! This pixbuffer has no Alpha channel (15% faster), only RGB.
  ! https://developer.gnome.org/gdk-pixbuf/stable/gdk-pixbuf-The-GdkPixbuf-Structure.html
  pixwidth  = 800
  pixheight = 800
  my_pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8_c_int, &
                           & pixwidth, pixheight)
  nch = gdk_pixbuf_get_n_channels(my_pixbuf)
  rowstride = gdk_pixbuf_get_rowstride(my_pixbuf)
  print *, "Channels= ", nch, "      Rowstride=", rowstride
  call c_f_pointer(gdk_pixbuf_get_pixels(my_pixbuf), pixel, &
                 & [pixwidth*pixheight*nch])

  ! The background is black (red=0, green=0, blue=0):
  pixel = char(0)
  ! Diagonal of the image:
  diag = sqrt(real(pixwidth*pixwidth + pixheight*pixheight, kind(0d0)))
  ! Coordinates of the triangle vertices:
  x = [ pixwidth/2d0,  0d0,                      (pixwidth-1)*1d0        ]
  y = [ 0d0,           pixheight*sqrt(3d0)/2d0,  pixheight*sqrt(3d0)/2d0 ]
  ! We start at an arbitrary position:
  xx = (x(1) + x(2)) / 2d0
  yy = (y(1) + y(2)) / 2d0

  do i = 1, n
      ! We choose randomly a vertex number (1, 2 or 3):
      call random_number(r)
      s = 1 + int(3*r)
      ! We compute the coordinates of the new point:
      xx = (xx + x(s)) / 2d0
      yy = (yy + y(s)) / 2d0
      ! Position of the corresponding pixel in the pixbuffer:
      p = 1 + nint(xx)*nch + nint(yy)*rowstride
      ! Red, Green, Blue values computed from the distances to vertices:
      pixel(p)   = char(int(255 * sqrt((xx-x(1))**2 + (yy-y(1))**2) / diag))
      pixel(p+1) = char(int(255 * sqrt((xx-x(2))**2 + (yy-y(2))**2) / diag))
      pixel(p+2) = char(int(255 * sqrt((xx-x(3))**2 + (yy-y(3))**2) / diag))
  end do

  ! Save the picture as a PNG:
  ! https://developer.gnome.org/gdk-pixbuf/stable/gdk-pixbuf-File-saving.html
  ! https://mail.gnome.org/archives/gtk-list/2004-October/msg00186.html
  cstatus = gdk_pixbuf_savev(my_pixbuf, "sierpinski_triangle.png"//c_null_char,&
              & "png"//c_null_char, c_null_ptr, c_null_ptr, c_null_ptr)
end program pixbuf_without_gui

