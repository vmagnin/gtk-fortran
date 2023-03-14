! This file is part of gtk-fortran, a GTK / Fortran interface library.
! Copyright (C) 2013 The gtk-fortran team
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
!
! Last modification: vmagnin 2023-03-14

program g_io_demo

  ! This is a very basic demo to get started with GIO. It uses Fortran I/O to
  ! read text from the keyboard & writes it to the file gio_demo.dat in the
  ! current directory.

  use, intrinsic :: iso_c_binding
  use gtk_sup

  !********************************
  ! Gtk modules for gio_demo.f90
  use g, only: g_file_new_for_path, g_file_replace, g_object_unref, &
       & g_output_stream_close, g_output_stream_write

  use gtk, only: FALSE, G_FILE_CREATE_NONE


  character(len=80) :: str
  integer(c_int8_t), dimension(80), target :: istr

  type(c_ptr) :: file, stream
  type(gerror), target :: errmsg
  character(len=120) :: errtxt
  integer(4) :: ios, i
  integer(c_size_t) :: ncput, nchars
  integer(c_int) :: iok

  file = g_file_new_for_path('gio_demo.dat'//c_null_char)

  ! Use g_file_replace here so that it won't crash if the file exists.
  stream = g_file_replace(file, c_null_char, FALSE, G_FILE_CREATE_NONE, &
       & c_null_ptr, c_loc(errmsg))

  if (.not. c_associated(stream)) then
     call c_f_string(errmsg%message, errtxt)
     print *, errtxt
     stop
  end if

  do
     write(*, "(a)", advance='no') "Text> "
     read(*, "(a)", iostat=ios) str
     if (ios /= 0) exit

     nchars = len_trim(str)
     do i = 1, int(nchars, kind=4)
        istr(i) = int(ichar(str(i:i)), kind=c_int8_t)
     end do
     istr(nchars+1) = ichar(c_new_line)

     ncput = g_output_stream_write(stream, c_loc(istr), &
          & nchars+1, c_null_ptr, c_loc(errmsg))
     if (ncput < 0_c_size_t) then
        call c_f_string(errmsg%message, errtxt)
        print *, errtxt
        stop
     end if
  end do

  iok = g_output_stream_close(stream, c_null_ptr, c_loc(errmsg))
  call g_object_unref(stream)
  if (.not. c_f_logical(iok)) then
     call c_f_string(errmsg%message, errtxt)
     print *, errtxt
     stop
  end if

  call g_object_unref(file)
  print *
end program g_io_demo
