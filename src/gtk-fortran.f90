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
! The gtk-?-fortran command prints information about gtk-fortran.
! Contributors: Vincent Magnin 2021-01-29
!------------------------------------------------------------------------------

module handlers_gtk_fortran
  use iso_c_binding
  use gtk, only: gtk_get_major_version, gtk_get_minor_version, &
               & gtk_get_micro_version
  use gtk_sup, only: c_f_string_copy
  use g, only: g_get_prgname, g_get_os_info

  implicit none

  contains

  subroutine activate(app, gdata) bind(c)
    type(c_ptr), value, intent(in)  :: app, gdata
    character(len=128) :: name_string, os_string

    call c_f_string_copy(g_get_prgname(), name_string)
    call c_f_string_copy(g_get_os_info("PRETTY_NAME"//c_null_char), os_string)
    
    print '(4A,I0,A1,I0,A1,I0,A1)', TRIM(name_string), " (", TRIM(os_string),&
      & ", GTK ", gtk_get_major_version(),".", &
      & gtk_get_minor_version(), ".", gtk_get_micro_version(), ")"

    print '(A)', "Licensed under GNU GPL 3 with the additional permissions&
      & described in the GCC Runtime Library Exception version 3.1"
    print '(A)', "This is free software: you are free to change and redistribute it."
    print '(A)', "There is NO WARRANTY, to the extent permitted by law."
    print *
    print '(A)', "Usage example:"
    print '(3A)', "gfortran my_app.f90 $(pkg-config --cflags --libs ", TRIM(name_string), ")"
    print *
    print '(A)', "Documentation: https://github.com/vmagnin/gtk-fortran/wiki"
    print '(A)', "Bug reports:   https://github.com/vmagnin/gtk-fortran/issues"
  end subroutine activate
end module handlers_gtk_fortran


program gtk_fortran
  use gtk, only: gtk_application_new, g_signal_connect, G_APPLICATION_FLAGS_NONE
  use g, only: g_application_run, g_object_unref
  use handlers_gtk_fortran

  implicit none
  integer(c_int)     :: status
  type(c_ptr)        :: app

  app = gtk_application_new("gtk-fortran.command"//c_null_char, &
                            & G_APPLICATION_FLAGS_NONE)
  call g_signal_connect(app, "activate"//c_null_char, c_funloc(activate), &
                      & c_null_ptr)
  status = g_application_run(app, 0_c_int, c_null_ptr)
  if (status /= 0) print '(A, I0)', ">>> GApplication error: ", status

  call g_object_unref(app)
end program gtk_fortran

