! Copyright (C) 2011
! Free Software Foundation, Inc.
!
! This file is part of the gtk-fortran  GTK Fortran Interface library.
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
! You should have received a copy of the GNU General Public
!  License along with
! this program; see the files LICENSE and LICENSE_EXCEPTION respectively.
! If not, see <http://www.gnu.org/licenses/>.
!-----------------------------------------------------------------------------
! Contributed by James Tappin
! Last modifications: 2012-12-13
!   vmagnin 2022-04-09
!-----------------------------------------------------------------------------

!*
! gdk_pixbuf_hl
module gdk_pixbuf_hl
  ! Some routines to facilitate the use of GDK pixbufs from Fortran.
  ! Allows the use of short-int images to avoid the issues that may result
  ! from Fortran's 8-bit integers being signed while GdkPixbuf pixels are
  ! unsigned.
  !/

  use g, only: g_slist_free, g_slist_length, g_slist_nth_data

  use gdk_pixbuf, only: gdk_pixbuf_format_get_description, &
       & gdk_pixbuf_format_get_extensions, gdk_pixbuf_format_get_license, &
       & gdk_pixbuf_format_get_mime_types, gdk_pixbuf_format_get_name, &
       & gdk_pixbuf_format_is_scalable, gdk_pixbuf_format_is_writable, &
       & gdk_pixbuf_get_bits_per_sample, gdk_pixbuf_get_formats, &
       & gdk_pixbuf_get_has_alpha, gdk_pixbuf_get_height, &
       & gdk_pixbuf_get_n_channels, gdk_pixbuf_get_pixels, &
       & gdk_pixbuf_get_rowstride, gdk_pixbuf_get_width, gdk_pixbuf_new, &
       & gdk_pixbuf_new_from_file, &
       & gdk_pixbuf_new_from_file_at_scale, gdk_pixbuf_savev

  use gtk, only: TRUE, FALSE, GDK_COLORSPACE_RGB

  use gtk_sup

  use, intrinsic :: iso_c_binding, only: c_null_ptr
  use, intrinsic :: iso_fortran_env, only: error_unit

  implicit none

  interface hl_gdk_pixbuf_new
     module procedure hl_gdk_pixbuf_new_empty
     module procedure hl_gdk_pixbuf_new_file
     module procedure hl_gdk_pixbuf_new_data8
     module procedure hl_gdk_pixbuf_new_data16
     module procedure hl_gdk_pixbuf_new_data8g
     module procedure hl_gdk_pixbuf_new_data16g
  end interface hl_gdk_pixbuf_new

  interface hl_gdk_pixbuf_get_pixels
     module procedure hl_gdk_pixbuf_get_pixels8
     module procedure hl_gdk_pixbuf_get_pixels16
  end interface hl_gdk_pixbuf_get_pixels

  interface hl_gdk_pixbuf_set_pixels
     module procedure hl_gdk_pixbuf_set_pixels8
     module procedure hl_gdk_pixbuf_set_pixels16
     module procedure hl_gdk_pixbuf_set_pixels8g
     module procedure hl_gdk_pixbuf_set_pixels16g
  end interface hl_gdk_pixbuf_set_pixels

  integer, parameter :: hl_gdk_pixbuf_type_len=20
  integer, parameter :: hl_gdk_pixbuf_option_len=80

contains

  !+
  function hl_gdk_pixbuf_new_empty(width, height, alpha, bits) &
       & result(pixbuf)
    type(c_ptr) :: pixbuf
    integer(c_int), intent(in) :: width, height
    integer(c_int), intent(in), optional :: alpha, bits

    ! Create a new empty pixbuf of the given size
    !
    ! WIDTH |  int |  required |  The width in pixels of the pixbuf
    ! HEIGHT |  int |  required |  The height in pixels of the pixbuf
    ! ALPHA |  boolean |  optional |  Whether to include an alpha channel (default=FALSE)
    ! BITS |  int |  optional |  The nuber of bits per sample (default=8).
    !
    ! This routine will usually be called via the generic interface
    ! hl_gdk_pixbuf_new.
    !-

    integer(c_int) :: isalpha, bpp

    if (present(alpha)) then
       isalpha = alpha
    else
       isalpha = FALSE
    end if
    if (present(bits)) then
       bpp = bits
    else
       bpp = 8
    end if

    pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, isalpha, bpp, width, height)

  end function hl_gdk_pixbuf_new_empty

  !+
  function hl_gdk_pixbuf_new_file(file, width, height, aspect, &
       & error) result(pixbuf)
    type(c_ptr) :: pixbuf
    character(len=*), intent(in) :: file
    integer(c_int), optional, intent(in) :: width, height, aspect
    character(len=*), optional, intent(out) :: error

    ! Read an image file into a new pixbuf
    !
    ! FILE |  string |  required |  The file to read
    ! WIDTH |  int |  optional |  The desired width for the pixbuf
    ! HEIGHT |  int |  optional |  The desired height for the pixbuf
    ! ASPECT |  boolean |  optional |  If sizing is given then set to TRUE to preserve the aspect ratio, or FALSE not to. If both dimensions are given, then the default is FALSE, if one is given, the default is TRUE.
    ! ERROR |  gerror |  optional |  The error code & message.
    !
    ! This routine will usually be called via the generic interface
    ! hl_gdk_pixbuf_new.
    !-

    integer(c_int) :: nx, ny, keep_asp
    type(gerror), pointer :: error_struct
    type(c_ptr), target :: error_str
    character(len=120) :: errmsg

    error_str = c_null_ptr
    if (present(width) .or. present(height)) then
       if (present(width)) then
          nx = width
       else
          nx = -1
       end if
       if (present(height)) then
          ny = height
       else
          ny = -1
       end if
       if (present(aspect)) then
          keep_asp = aspect
       else if (present(width) .and. present(height)) then
          keep_asp = FALSE
       else
          keep_asp = TRUE
       end if
       pixbuf = gdk_pixbuf_new_from_file_at_scale(trim(file)//c_null_char, &
            & nx, ny, keep_asp, c_loc(error_str))
    else
       pixbuf = gdk_pixbuf_new_from_file(trim(file)//c_null_char, &
            & c_loc(error_str))
    end if

    if (.not. c_associated(pixbuf)) then
       call c_f_pointer(error_str, error_struct)
       call convert_c_string(error_struct%message, &
            & errmsg)
       if (present(error)) then
          error = errmsg
       else
          write(error_unit, *) "HL_GDK_PIXBUF_NEW_FILE: ",trim(errmsg)
       end if
    else if (present(error)) then
       error = ''
    end if

  end function hl_gdk_pixbuf_new_file

  !+
  function hl_gdk_pixbuf_new_data8(data) result(pixbuf)
    type(c_ptr) :: pixbuf
    integer(c_int8_t), dimension(:,:,:), intent(in) :: data

    ! Create a pixbuf from an RGB(A) array of values.
    !
    ! DATA |  int8 |  required |  The data values as a 1..4 x n x m array.
    !
    ! This routine will usually be called via the generic interface
    ! hl_gdk_pixbuf_new.
    !-

    integer(c_int), dimension(3) :: sz
    integer(c_int) :: alpha

    sz = shape(data)
    select case (sz(1))
    case(1,3)
       alpha = FALSE
    case(2,4)
       alpha = TRUE
    case default
       pixbuf = c_null_ptr
       write(error_unit, *) &
            & "HL_GDK_PIXBUF_NEW_DATA8: first dimension of the data"//&
            & " must be between 1 and 4"
       return
    end select

    pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, alpha, 8_c_int, &
         & sz(2), sz(3))

    call hl_gdk_pixbuf_set_pixels(pixbuf, data)
  end function hl_gdk_pixbuf_new_data8


  !+
  function hl_gdk_pixbuf_new_data8g(data) result(pixbuf)
    type(c_ptr) :: pixbuf
    integer(c_int8_t), dimension(:,:), intent(in) :: data

    ! Create a pixbuf from a greyscale array of values.
    !
    ! DATA |  int8 |  required |  The data values as a n x m array.
    !
    ! This routine will usually be called via the generic interface
    ! hl_gdk_pixbuf_new.
    !-

    integer(c_int), dimension(2) :: sz

    sz = shape(data)

    pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8_c_int, &
         & sz(1), sz(2))

    call hl_gdk_pixbuf_set_pixels(pixbuf, data)
  end function hl_gdk_pixbuf_new_data8g


  !+
  function hl_gdk_pixbuf_new_data16(data) result(pixbuf)
    type(c_ptr) :: pixbuf
    integer(c_short), dimension(:,:,:), intent(in) :: data

    ! Create a pixbuf from an RGB(A) array of values. This version
    ! uses 2-byte integers to avoid the signing issues of the c_int8_t type.
    !
    ! DATA |  short |  required |  The data values as a 1..4 x n x m array.
    !
    ! This routine will usually be called via the generic interface
    ! hl_gdk_pixbuf_new.
    !-

    integer(c_int), dimension(3) :: sz
    integer(c_int) :: alpha

    sz = shape(data)

    select case (sz(1))
    case(1,3)
       alpha = FALSE
    case(2,4)
       alpha = TRUE
    case default
       pixbuf = c_null_ptr
       write(error_unit, *) &
            & "HL_GDK_PIXBUF_NEW_DATA16: first dimension of the data"//&
            & " must be between 1 and 4"
       return
    end select

    pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, alpha, 8_c_int, &
         & sz(2), sz(3))

    call hl_gdk_pixbuf_set_pixels(pixbuf, data)
  end function hl_gdk_pixbuf_new_data16

  !+
  function hl_gdk_pixbuf_new_data16g(data) result(pixbuf)
    type(c_ptr) :: pixbuf
    integer(c_short), dimension(:,:), intent(in) :: data

    ! Create a pixbuf from a greyscale array of values. This version
    ! uses 2-byte integers to avoid the signing issues of the c_int8_t type.
    !
    ! DATA |  short |  required |  The data values as a n x m array.
    !
    ! This routine will usually be called via the generic interface
    ! hl_gdk_pixbuf_new.
    !-

    integer(c_int), dimension(2) :: sz

    sz = shape(data)

    pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8_c_int, &
         & sz(1), sz(2))

    call hl_gdk_pixbuf_set_pixels(pixbuf, data)
  end function hl_gdk_pixbuf_new_data16g

  !+
  subroutine hl_gdk_pixbuf_get_pixels8(pixbuf, pixels)
    type(c_ptr), intent(in) :: pixbuf
    integer(c_int8_t), dimension(:,:,:), allocatable, intent(out) :: pixels

    ! Get the pixels of a pixbuf and return them as a Fortran 3xnxm or 4xnxm
    ! array
    !
    ! PIXBUF |  c_ptr |  required |  The pixbuf to read
    ! PIXELS |  int8 |  required |  Will contain the image.
    !
    ! This is normally called via the generic hl_gdk_pixbuf_get_pixels
    ! interface. Implemented as a subroutine to allow this.
    !-

    type(c_ptr), target :: cpixels
    integer(c_int8_t), pointer, dimension(:) :: fpixels
    integer :: i,j, iroff, ioff
    integer(c_int) :: rowstr, nrows, ncols, nchans
    integer :: lpix

    call hl_gdk_pixbuf_info(pixbuf, nchannels=nchans, height=nrows, &
         & width=ncols, rowstride=rowstr)

    allocate(pixels(nchans, ncols, nrows))

    lpix = int(rowstr*(nrows-1) + ncols*nchans)

    cpixels = gdk_pixbuf_get_pixels(pixbuf)
    call c_f_pointer(cpixels, fpixels, [lpix])

    do j = 0,nrows-1
       iroff = j*rowstr + 1
       do i = 0, ncols-1
          ioff = iroff + i*nchans
          pixels(:,i+1,j+1) = fpixels(ioff:ioff+nchans-1)
       end do
    end do
  end subroutine hl_gdk_pixbuf_get_pixels8

  !+
  subroutine hl_gdk_pixbuf_get_pixels16(pixbuf, pixels)
    type(c_ptr), intent(in) :: pixbuf
    integer(c_short), dimension(:,:,:), allocatable, intent(out) :: pixels

    ! Get the pixels of a pixbuf and return them as a Fortran 3xnxm or 4xnxm
    ! array. This version returns as a short array to evade the signing issues
    ! of 8-bit integers in Fortran
    !
    ! PIXBUF |  c_ptr |  required |  The pixbuf to read
    ! PIXELS |  short |  required |  Will contain the image.
    !
    ! This is normally called via the generic hl_gdk_pixbuf_get_pixels
    ! interface. Implemented as a subroutine to allow this.
    !-

    type(c_ptr), target :: cpixels
    integer(c_int8_t), pointer, dimension(:) :: fpixels
    integer :: i,j, iroff, ioff
    integer(c_int) :: rowstr, nrows, ncols, nchans
    integer :: lpix

    call hl_gdk_pixbuf_info(pixbuf, nchannels=nchans, height=nrows, &
         & width=ncols, rowstride=rowstr)

    allocate(pixels(nchans, ncols, nrows))
    lpix = int(rowstr*(nrows-1) + ncols*nchans)

    cpixels = gdk_pixbuf_get_pixels(pixbuf)
    call c_f_pointer(cpixels, fpixels, [lpix])

    do j = 0,nrows-1
       iroff = j*rowstr + 1
       do i = 0, ncols-1
          ioff = iroff + i*nchans
          pixels(:,i+1,j+1) = iand(int(fpixels(ioff:ioff+nchans-1), c_short), &
               & 255_c_short)
       end do
    end do
  end subroutine hl_gdk_pixbuf_get_pixels16

  !+
  subroutine hl_gdk_pixbuf_set_pixels8(pixbuf, pixels, xoff, yoff)
    type(c_ptr), intent(in) :: pixbuf
    integer(c_int8_t), dimension(:,:,:), intent(in) :: pixels
    integer, intent(in), optional :: xoff, yoff

    ! Set the pixels of a pixbuf from a Fortran array.
    !
    ! PIXBUF |  c_ptr |  required |  The pixbuf to update
    ! PIXELS |  int8 |  required |  Contains the image to insert.
    ! XOFF |  int |  optional |  The X-offset at which to write the image.
    ! YOFF |  int |  optional |  The Y-offset at which to write the image.
    !
    ! This is normally called via the generic hl_gdk_pixbuf_set_pixels
    ! interface.
    ! N.B. To leave a gap at the "high" sides of the pixbuf, just use a
    ! smaller input array or array slice than the pixbuf size.
    !-

    integer :: i,j, ioff, iroff, xstart, ystart, xtop, ytop, lput
    integer(c_int) :: rowstr, nrows, ncols, nchans
    integer :: lpix
    type(c_ptr), target :: cpixels
    integer(c_int8_t), pointer, dimension(:) :: fpixels
    integer, dimension(3) :: sz
    logical :: salpha, gscale

    sz=shape(pixels)
    if (present(xoff)) then
       xstart = xoff
    else
       xstart = 0
    end if
    if (present(yoff)) then
       ystart = yoff
    else
       ystart = 0
    end if

    call hl_gdk_pixbuf_info(pixbuf, nchannels=nchans, height=nrows, &
         & width=ncols, rowstride=rowstr)
    lpix = int(rowstr*(nrows-1) + ncols*nchans)

    ! Checks on sizes etc.

    if (sz(2) > ncols-xstart) then
       write(error_unit, *) &
            & "HL_GDK_SET_PIXELS8: Input image has too many columns,"// &
            & " using lower part."
       xtop = ncols-xstart
    else
       xtop = sz(2)
    end if
    if (sz(3) > nrows-ystart) then
       write(error_unit, *) &
            & "HL_GDK_SET_PIXELS8: Input image has too many rows,"// &
            & " using lower part."
       ytop = nrows-ystart
    else
       ytop = sz(3)
    end if

    salpha = .false.
    gscale = .false.

    select case (sz(1))
    case(3)
       if (nchans == 4) then
          write(error_unit, *) &
               & "HL_GDK_SET_PIXELS8: No alpha channel in input, using opaque."
          salpha = .true.
       end if
    case(4)
       if (nchans == 3) then
          write(error_unit, *) &
               & "HL_GDK_SET_PIXELS8: No alpha channel in output, omitting."
       end if
    case(2)
       gscale = .true.
       if (nchans == 3) then
          write(error_unit, *) &
               & "HL_GDK_SET_PIXELS8: No alpha channel in output, omitting."
       end if
    case(1)
       gscale = .true.
       if (nchans == 4) then
          write(error_unit, *) &
               & "HL_GDK_SET_PIXELS8: No alpha channel in input, using opaque."
          salpha = .true.
       end if
    case(5:)
       write(error_unit, *) &
            & "HL_GDK_SET_PIXELS8: too many channels in input, omitting excess."
    end select
    lput = min(sz(1), nchans)

    ! Actually set the pixels.
    cpixels = gdk_pixbuf_get_pixels(pixbuf)
    call c_f_pointer(cpixels, fpixels, [lpix])

    if (gscale) then
       do j = 0, ytop-1
          iroff = (j+ystart)*rowstr + 1 + xstart
          do i = 0,xtop-1
             ioff = iroff + i*nchans
             fpixels(ioff:ioff+2) = pixels(1, i+1,j+1)
             if (salpha) then
                fpixels(ioff+3) = -1_c_int8_t
             else if (nchans == 4) then
                fpixels(ioff+3) = pixels(2, i+1,j+1)
             end if
          end do
       end do
    else
       do j = 0, ytop-1
          iroff = (j+ystart)*rowstr + 1 + xstart
          do i = 0,xtop-1
             ioff = iroff + i*nchans
             fpixels(ioff:ioff+lput-1) = pixels(:lput, i+1,j+1)
             if (salpha) fpixels(ioff+lput) = -1_c_int8_t
          end do
       end do
    end if

  end subroutine hl_gdk_pixbuf_set_pixels8

  !+
  subroutine hl_gdk_pixbuf_set_pixels8g(pixbuf, pixels, xoff, yoff)
    type(c_ptr), intent(in) :: pixbuf
    integer(c_int8_t), dimension(:,:), intent(in) :: pixels
    integer, intent(in), optional :: xoff, yoff

    ! Set the pixels of a pixbuf from a Fortran array (greyscale).
    !
    ! PIXBUF |  c_ptr |  required |  The pixbuf to update
    ! PIXELS |  int8 |  required |  Contains the image to insert.
    ! XOFF |  int |  optional |  The X-offset at which to write the image.
    ! YOFF |  int |  optional |  The Y-offset at which to write the image.
    !
    ! This is normally called via the generic hl_gdk_pixbuf_set_pixels
    ! interface.
    ! N.B. To leave a gap at the "high" sides of the pixbuf, just use a
    ! smaller input array or array slice than the pixbuf size.
    !-

    integer :: i,j, ioff, iroff, xstart, ystart, xtop, ytop
    integer(c_int) :: rowstr, nrows, ncols, nchans
    integer :: lpix
    type(c_ptr), target :: cpixels
    integer(c_int8_t), pointer, dimension(:) :: fpixels
    integer, dimension(2) :: sz

    sz=shape(pixels)
    if (present(xoff)) then
       xstart = xoff
    else
       xstart = 0
    end if
    if (present(yoff)) then
       ystart = yoff
    else
       ystart = 0
    end if

    call hl_gdk_pixbuf_info(pixbuf, nchannels=nchans, height=nrows, &
         & width=ncols, rowstride=rowstr)
    lpix = int(rowstr*(nrows-1) + ncols*nchans)

    ! Checks on sizes etc.

    if (sz(1) > ncols-xstart) then
       write(error_unit, *) &
            & "HL_GDK_SET_PIXELS8G: Input image has too many columns,"// &
            & " using lower part."
       xtop = ncols-xstart
    else
       xtop = sz(1)
    end if
    if (sz(2) > nrows-ystart) then
       write(error_unit, *) &
            & "HL_GDK_SET_PIXELS8G: Input image has too many rows,"// &
            & " using lower part."
       ytop = nrows-ystart
    else
       ytop = sz(2)
    end if

    ! Actually set the pixels.
    cpixels = gdk_pixbuf_get_pixels(pixbuf)
    call c_f_pointer(cpixels, fpixels, [lpix])

    do j = 0, ytop-1
       iroff = (j+ystart)*rowstr + 1 + xstart
       do i = 0,xtop-1
          ioff = iroff + i*nchans
          fpixels(ioff:ioff+2) = pixels(i+1,j+1)
          if (nchans == 4) fpixels(ioff+3) = -1_c_int8_t
       end do
    end do

  end subroutine hl_gdk_pixbuf_set_pixels8g

  !+
  subroutine hl_gdk_pixbuf_set_pixels16(pixbuf, pixels, xoff, yoff)
    type(c_ptr), intent(in) :: pixbuf
    integer(c_short), dimension(:,:,:), intent(in) :: pixels
    integer, intent(in), optional :: xoff, yoff

    ! Set the pixels of a pixbuf from a Fortran array (16-bit).
    !
    ! PIXBUF |  c_ptr |  required |  The pixbuf to update
    ! PIXELS |  short |  required |  Contains the image to insert.
    ! XOFF |  int |  optional |  The X-offset at which to write the image.
    ! YOFF |  int |  optional |  The Y-offset at which to write the image.
    !
    ! This is normally called via the generic hl_gdk_pixbuf_set_pixels
    ! interface.
    ! N.B. To leave a gap at the "high" sides of the pixbuf, just use a
    ! smaller input array or array slice than the pixbuf size.
    !-

    integer :: i,j, ioff, iroff, xstart, ystart, xtop, ytop, lput
    integer(c_int) :: rowstr, nrows, ncols, nchans
    integer :: lpix
    type(c_ptr), target :: cpixels
    integer(c_int8_t), pointer, dimension(:) :: fpixels
    integer, dimension(3) :: sz
    logical :: salpha, gscale

    sz=shape(pixels)
    if (present(xoff)) then
       xstart = xoff
    else
       xstart = 0
    end if
    if (present(yoff)) then
       ystart = yoff
    else
       ystart = 0
    end if

    call hl_gdk_pixbuf_info(pixbuf, nchannels=nchans, height=nrows, &
         & width=ncols, rowstride=rowstr)
    lpix = int(rowstr*(nrows-1) + ncols*nchans)

    ! Checks on sizes etc.

    if (sz(2) > ncols-xstart) then
       write(error_unit, *) &
            & "HL_GDK_SET_PIXELS16: Input image has too many columns,"// &
            & " using lower part."
       xtop = ncols-xstart
    else
       xtop = sz(2)
    end if
    if (sz(3) > nrows-ystart) then
       write(error_unit, *) &
            & "HL_GDK_SET_PIXELS16: Input image has too many rows,"// &
            & " using lower part."
       ytop = nrows-ystart
    else
       ytop = sz(3)
    end if

    salpha = .false.
    gscale = .false.

    select case (sz(1))
    case(3)
       if (nchans == 4) then
          write(error_unit, *) &
               & "HL_GDK_SET_PIXELS16: No alpha channel in input, using opaque."
          salpha = .true.
       end if
    case(4)
       if (nchans == 3) then
          write(error_unit, *) &
               & "HL_GDK_SET_PIXELS16: No alpha channel in output, omitting."
       end if
    case(2)
       gscale = .true.
       if (nchans == 3) then
          write(error_unit, *) &
               & "HL_GDK_SET_PIXELS16: No alpha channel in output, omitting."
       end if
    case(1)
       gscale = .true.
       if (nchans == 4) then
          write(error_unit, *) &
               & "HL_GDK_SET_PIXELS16: No alpha channel in input, using opaque."
          salpha = .true.
       end if
    case(5:)
       write(error_unit, *) &
            & "HL_GDK_SET_PIXELS16: too many channels in input, omitting excess."
    end select
    lput = min(sz(1), nchans)

    ! Actually set the pixels.
    cpixels = gdk_pixbuf_get_pixels(pixbuf)
    call c_f_pointer(cpixels, fpixels, [lpix])

    if (gscale) then
       do j = 0, ytop-1
          iroff = (j+ystart)*rowstr + 1 + xstart
          do i = 0,xtop-1
             ioff = iroff + i*nchans
             fpixels(ioff:ioff+2) = int(pixels(1, i+1,j+1), c_int8_t)
             if (salpha) then
                fpixels(ioff+3) = -1_c_int8_t
             else if (nchans == 4) then
                fpixels(ioff+3) = int(pixels(2, i+1,j+1), c_int8_t)
             end if
          end do
       end do
    else
       do j = 0, ytop-1
          iroff = (j+ystart)*rowstr + 1 + xstart
          do i = 0,xtop-1
             ioff = iroff + i*nchans
             fpixels(ioff:ioff+lput-1) = int(pixels(:lput, i+1,j+1), c_int8_t)
             if (salpha) fpixels(ioff+lput) = -1_c_int8_t
          end do
       end do
    end if
  end subroutine hl_gdk_pixbuf_set_pixels16

  !+
  subroutine hl_gdk_pixbuf_set_pixels16g(pixbuf, pixels, xoff, yoff)
    type(c_ptr), intent(in) :: pixbuf
    integer(c_short), dimension(:,:), intent(in) :: pixels
    integer, intent(in), optional :: xoff, yoff

    ! Set the pixels of a pixbuf from a Fortran array (16-bit, greyscale).
    !
    ! PIXBUF |  c_ptr |  required |  The pixbuf to update
    ! PIXELS |  short |  required |  Contains the image to insert.
    ! XOFF |  int |  optional |  The X-offset at which to write the image.
    ! YOFF |  int |  optional |  The Y-offset at which to write the image.
    !
    ! This is normally called via the generic hl_gdk_pixbuf_set_pixels
    ! interface.
    ! N.B. To leave a gap at the "high" sides of the pixbuf, just use a
    ! smaller input array or array slice than the pixbuf size.
    !-

    integer :: i,j, ioff, iroff, xstart, ystart, xtop, ytop
    integer(c_int) :: rowstr, nrows, ncols, nchans
    integer :: lpix
    type(c_ptr), target :: cpixels
    integer(c_int8_t), pointer, dimension(:) :: fpixels
    integer, dimension(2) :: sz

    sz=shape(pixels)
    if (present(xoff)) then
       xstart = xoff
    else
       xstart = 0
    end if
    if (present(yoff)) then
       ystart = yoff
    else
       ystart = 0
    end if

    call hl_gdk_pixbuf_info(pixbuf, nchannels=nchans, height=nrows, &
         & width=ncols, rowstride=rowstr)
    lpix = int(rowstr*(nrows-1) + ncols*nchans)

    ! Checks on sizes etc.

    if (sz(1) > ncols-xstart) then
       write(error_unit, *) &
            & "HL_GDK_SET_PIXELS16G: Input image has too many columns,"// &
            & " using lower part."
       xtop = ncols-xstart
    else
       xtop = sz(1)
    end if
    if (sz(2) > nrows-ystart) then
       write(error_unit, *) &
            & "HL_GDK_SET_PIXELS16G: Input image has too many rows,"// &
            & " using lower part."
       ytop = nrows-ystart
    else
       ytop = sz(2)
    end if


    ! Actually set the pixels.
    cpixels = gdk_pixbuf_get_pixels(pixbuf)
    call c_f_pointer(cpixels, fpixels, [lpix])

    do j = 0, ytop-1
       iroff = (j+ystart)*rowstr + 1 + xstart
       do i = 0,xtop-1
          ioff = iroff + i*nchans
          fpixels(ioff:ioff+2) = int(pixels(i+1,j+1), c_int8_t)
          if (nchans == 4)  fpixels(ioff+3) = -1_c_int8_t
       end do
    end do
  end subroutine hl_gdk_pixbuf_set_pixels16g

  !+
  subroutine hl_gdk_pixbuf_info(pixbuf, nchannels, bits, alpha, &
       & height, width, rowstride)

    type(c_ptr), intent(in) :: pixbuf
    integer(c_int), optional, intent(out) :: nchannels, bits, alpha, &
         & height, width, rowstride

    ! Return information about an existing pixbuf.
    !
    ! PIXBUF |  c_ptr |  required |  The pixbuf to be queried.
    ! NCHANNELS |  int |  optional |  How many channels does the pixbuf have?
    ! BITS |  int |  optional |  How many bits per pixel?
    ! ALPHA |  boolean |  optional |  Does the pixbuf have an alpha channel?
    ! HEIGHT |  int |  optional |  How many rows in the image.
    ! WIDTH |  int |  optional |  How many columns in the image.
    ! ROWSTRIDE |  int |  optional |  How many bytes between the start of successive rows.
    !-

    if (present(nchannels)) nchannels = gdk_pixbuf_get_n_channels(pixbuf)
    if (present(alpha)) alpha = gdk_pixbuf_get_has_alpha(pixbuf)
    if (present(bits)) bits = gdk_pixbuf_get_bits_per_sample(pixbuf)
    if (present(width)) width = gdk_pixbuf_get_width(pixbuf)
    if (present(height)) height = gdk_pixbuf_get_height(pixbuf)
    if (present(rowstride)) rowstride = gdk_pixbuf_get_rowstride(pixbuf)

  end subroutine hl_gdk_pixbuf_info

  !+
  subroutine hl_gdk_pixbuf_save(pixbuf, file, type, options, ok, error)
    type(c_ptr), intent(in) :: pixbuf
    character(len=*), intent(in) :: file
    character(len=*), intent(in), optional :: type
    character(len=*), dimension(:), intent(in), optional :: options
    logical, intent(out), optional :: ok
    character(len=*), intent(out), optional :: error

    ! Save a pixbuf to a file.
    !
    ! PIXBUF |  c_ptr |  required |  The pixbuf to save
    ! FILE |  f_string |  required |  The filename to which to save.
    ! TYPE |  f_string |  optional |  The file type to use, if not given then it is deduced from the extension.
    ! OPTIONS |  f_string() |  optional |  A list of options in the form "option=value".
    ! OK |  logical |  optional |  Was the write successful.
    ! ERROR |  f_string |  optional |  An error message if the write failed.
    !-

    character(len=hl_gdk_pixbuf_type_len) :: ftype
    integer :: pd
    type(c_ptr), target :: err
    type(gerror), pointer :: ferr
    integer(c_int) :: iok
    character(len=120) :: ferrmsg
    type(c_ptr), dimension(:), allocatable :: copt_names, copt_vals
    character(len=hl_gdk_pixbuf_option_len), target, &
         & dimension(:), allocatable :: opt_names, opt_vals
    integer :: peq, i, nopt

    if (.not. present(type)) then
       pd = index(file, '.', back=.true.)
       if (pd == 0) then
          if (present(ok)) ok = .false.
          if (present(error)) then
             error = "No file type given and no extension to filename"
          else
             write(error_unit, *) "HL_GDK_PIXBUF_SAVE: "// &
                  & "No file type given and no extension to filename"
          end if
          return
       end if
       ftype = hl_gdk_pixbuf_format_find(extension=trim(file(pd+1:)))
       if (ftype == '') then
          if (present(ok)) ok = .false.
          if (present(error)) then
             error = "No file type found for extension: "//trim(file(pd+1:))
          else
             write(error_unit, *) "HL_GDK_PIXBUF_SAVE: "// &
                  & "No file type found for extension: "//trim(file(pd+1:))
          end if
          return
       end if
    else
       ftype = type
    end if

    err = c_null_ptr
    if (present(options)) then
       nopt = size(options)
    else
       nopt = 0
    end if
    allocate(copt_names(nopt+1), copt_vals(nopt+1))
    allocate(opt_names(nopt), opt_vals(nopt))

    copt_names(nopt+1) = c_null_ptr
    copt_vals(nopt+1) = c_null_ptr

    do i = 1, nopt
       peq = index(options(i), "=")
       opt_names(i) = options(i)(:peq-1)//c_null_char
       opt_vals(i) = trim(options(i)(peq+1:))//c_null_char
       copt_names(i) = c_loc(opt_names(i))
       copt_vals(i) = c_loc(opt_vals(i))
    end do
    iok = gdk_pixbuf_savev(pixbuf, trim(file)//c_null_char, &
         & trim(ftype)//c_null_char, copt_names, copt_vals, c_loc(err))

    if (.not. c_f_logical(iok)) then
       if (present(ok)) ok = .false.
       call c_f_pointer(err, ferr)
       call c_f_string(ferr%message, ferrmsg)
       if (present(error)) then
          error=trim(ferrmsg)
       else
          write(error_unit, *) "HL_GDK_PIXBUF_SAVE: "//trim(ferrmsg)
       end if
    else if (present(ok)) then
       ok = .true.
    end if
  end subroutine hl_gdk_pixbuf_save

  ! The routines below here deal with GdkPixbufFormat

  !+
  subroutine hl_gdk_pixbuf_get_formats(names, writable, description, &
       & scalable, license)

    character(len=*), intent(out), dimension(:), allocatable :: names
    logical, intent(out), dimension(:), allocatable, optional :: writable, &
         & scalable
    character(len=*), intent(out), dimension(:), allocatable, &
         & optional :: license
    character(len=*), intent(out), dimension(:), allocatable, &
         & optional :: description

    ! Get information about the available file formats for reading/writing
    ! pixbufs.
    !
    ! NAMES |  f_string() |  required |  The names found
    ! WRITABLE |  logical() |  optional |  Whether the formats are writable.
    ! DESCRIPTION |  f_string() |  optional |  Descriptions of the formats.
    ! SCALABLE |  logical() |  optional |  Whether the formats are scalable.
    ! LICENSE |  f_string() |  optional |  The licence of the format module.
    !-

    type(c_ptr) :: flist, fmt
    integer(c_int) :: nfmt, i

    flist = gdk_pixbuf_get_formats()

    nfmt = g_slist_length(flist)

    allocate(names(nfmt))
    if (present(writable)) allocate(writable(nfmt))
    if (present(description)) allocate(description(nfmt))
    if (present(scalable)) allocate(scalable(nfmt))
    if (present(license)) allocate(license(nfmt))

    do i = 0, nfmt-1
       fmt = g_slist_nth_data(flist, i)
       call c_f_string(gdk_pixbuf_format_get_name(fmt), names(i+1))
       if (present(description)) &
            & call c_f_string(gdk_pixbuf_format_get_description(fmt), &
            & description(i+1))
       if (present(license)) &
            & call c_f_string(gdk_pixbuf_format_get_license(fmt), &
            & license(i+1))
       if (present(writable)) &
            & writable(i+1) = c_f_logical(gdk_pixbuf_format_is_writable(fmt))
       if (present(scalable)) &
            & scalable(i+1) = c_f_logical(gdk_pixbuf_format_is_scalable(fmt))
    end do

    call g_slist_free(flist)

  end subroutine hl_gdk_pixbuf_get_formats

  !+
  function hl_gdk_pixbuf_format_info(name, mime_types, extensions, &
       & writable, scalable, description, license) result(found)

    logical :: found
    character(len=*), intent(in) :: name
    character(len=*), intent(out), optional, allocatable, &
         & dimension(:) :: mime_types, extensions
    logical, intent(out), optional :: writable, scalable
    character(len=*), intent(out), optional :: description, license

    ! Get information about a specific file type.
    !
    ! NAME |  string |  required |  The file type to query.
    ! MIME_TYPE |  fstring() |  optional |  Will contain a list of mime-types associated with this file type.
    ! EXTENSIONS |  fstring() |  optional |  Will contain a list of file extensions normally used for this type.
    ! WRITABLE |  logical |  optional |  Whether the type is writable.
    ! SCALABLE |  logical |  optional |  Whether the type is scalable.
    ! DESCRIPTION |  f_string |  optional |  A description of the file type.
    ! LICENSE |  f_string |  optional |  The license of the module for the file type.
    !
    ! Returns .true. if the type is found, .false. if it is not.
    !-

    character(len=hl_gdk_pixbuf_type_len), dimension(:), allocatable :: names
    integer(c_int) :: idx, i
    type(c_ptr) :: flist, fmt
    type(c_ptr), target :: vlist
    type(c_ptr), dimension(:), pointer :: val

    call hl_gdk_pixbuf_get_formats(names)
    if (all(names /= name)) then
       found = .false.
       return
    end if
    found = .true.

    ! Extract the matching format.
    do i = 1,size(names)
       if (names(i) == name) then
          idx = i-1
          exit
       end if
    end do
    flist = gdk_pixbuf_get_formats()
    fmt = g_slist_nth_data(flist, idx)
    call g_slist_free(flist)

    ! Extract the requested information

    if (present(writable)) &
         & writable = c_f_logical(gdk_pixbuf_format_is_writable(fmt))
    if (present(scalable)) &
         & scalable = c_f_logical(gdk_pixbuf_format_is_scalable(fmt))

    if (present(description)) &
         & call c_f_string(gdk_pixbuf_format_get_description(fmt), &
         & description)
    if (present(license)) &
         & call c_f_string(gdk_pixbuf_format_get_license(fmt), &
         & license)

    if (present(mime_types)) then
       vlist = gdk_pixbuf_format_get_mime_types(fmt)
       ! A size of 20 should be enough for all formats:
       call c_f_pointer(vlist, val, [20])
       i = 1
       idx = 0
       ! Get the size of the val() array:
       do
          if (.not. c_associated(val(i))) then
             idx = i-1
             exit
          end if
          i = i+1
       end do

       if (idx > 0) then
          allocate(mime_types(idx))
          do i = 1, idx
             call c_f_string(val(i), mime_types(i))
          end do
       end if
    end if
    if (present(extensions)) then
       vlist = gdk_pixbuf_format_get_extensions(fmt)
       ! A size of 20 should be enough for all formats:
       call c_f_pointer(vlist, val, [20])
       i = 1
       idx = 0
       ! Get the size of the val() array:
       do
          if (.not. c_associated(val(i))) then
             idx = i-1
             exit
          end if
          i = i+1
       end do

       if (idx > 0) then
          allocate(extensions(idx))
          do i = 1, idx
             call c_f_string(val(i), extensions(i))
          end do
       end if
    end if
  end function hl_gdk_pixbuf_format_info

  !+
  function hl_gdk_pixbuf_format_find(mime_type, extension) result(name)
    character(len=hl_gdk_pixbuf_type_len) :: name
    character(len=*), intent(in), optional :: mime_type, extension

    ! Find the format type appropriate to a mime_type or a
    ! filename extension.
    !
    ! MIME_TYPE |  f_string |  optional |  The mime-type to match.
    ! EXTENSION |  f_string |  optional |  The file extension to match.
    !
    ! If no match is found then an empty string is returned.
    ! Mime_type has precedence over extension.
    !-

    character(len=hl_gdk_pixbuf_type_len), dimension(:), allocatable :: names
    character(len=20), dimension(:), allocatable :: types
    integer :: i, zidx
    logical :: found

    call hl_gdk_pixbuf_get_formats(names)

    name = ''

    if (present(mime_type)) then
       zidx = index(mime_type, c_null_char)
       if (zidx == 0) zidx=len(mime_type)+1
       do i = 1, size(names)
          found = hl_gdk_pixbuf_format_info(names(i), mime_types=types)
          if (any(types == mime_type(:zidx-1))) then
             name = names(i)
             exit
          end if
       end do
    else if (present(extension)) then
       zidx = index(extension, c_null_char)
       if (zidx == 0) zidx=len_trim(extension)+1
       do i = 1, size(names)
          found = hl_gdk_pixbuf_format_info(names(i), extensions=types)
          if (any(types == extension(:zidx-1))) then
             name = names(i)
             exit
          end if
       end do
    end if
  end function hl_gdk_pixbuf_format_find

end module gdk_pixbuf_hl
