! This file is part of gtk-fortran, a GTK / Fortran interface library.
! Copyright (C) 2011 The gtk-fortran team
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
! Contributed by Vincent Magnin, Jerry DeLisle, "jtappin" and Tobias Burnus, 2011-01-23
! Last modification: 2022-09-29, 2023-03-10
!
! This module is needed for every gtk-fortran application. The gtkenums-auto.in
! and gtk-auto.in files are automatically included.
module gtk
  use, intrinsic :: iso_c_binding
  implicit none
  public :: FALSE, TRUE, gtk_init, function_g_signal_connect, &
            & g_signal_connect, function_g_signal_connect_swapped, &
            & g_signal_connect_swapped

  !**************************************************************************
  ! The enums automatically generated by cfwrapper.py are included here:
  include "gtkenums-auto.in"
  !**************************************************************************

  interface
    ! In GTK 4, the gtk_init() and gtk_init_check() functions no longer
    ! accept commandline arguments:
    !   void gtk_init (void);
    ! https://developer.gnome.org/gtk4/stable/gtk4-General.html#gtk-init
    ! https://developer.gnome.org/gtk4/3.98/ch31s02.html#id-1.6.4.4.36
    subroutine gtk_init() bind(c)
      implicit none
    end subroutine

    !**************************************************************************
    ! The interfaces automatically generated by cfwrapper.py are included here:
    include "gtk-auto.in"
    !**************************************************************************
  end interface

  ! We define the GLib FALSE and TRUE constants, which type is
  ! gboolean, which is gint, which corresponds to the standard C int.
  ! FALSE is defined as 0, all non-zero values in C evaluate to "true".
  ! https://developer.gnome.org/glib/stable/glib-Basic-Types.html#gboolean
  ! https://developer.gnome.org/glib/stable/glib-Basic-Types.html#gint
  ! https://developer.gnome.org/glib/stable/glib-Standard-Macros.html#TRUE:CAPS
  integer(c_int), parameter   :: FALSE = 0
  integer(c_int), parameter   :: TRUE = 1

contains
  ! For convenience, /usr/include/glib-2.0/gobject/gsignal.h defines a macro based
  ! on the g_signal_connect_data() function:
  ! #define g_signal_connect(instance, detailed_signal, c_handler, data) \
  !  g_signal_connect_data ((instance), (detailed_signal), (c_handler), (data), NULL, (GConnectFlags) 0)
  ! It returns the handler ID, of type #gulong (always greater than 0 for successful connections).
  ! https://developer.gnome.org/gobject/stable/gobject-Signals.html#g-signal-connect
  ! Historically, in gtk-fortran g_signal_connect() was declared as a subroutine, because the handler_id
  ! returned by the GLib function is usually never used. Here we define both a g_signal_connect()
  ! function and a subroutine. You will generally use the subroutine in your programs.

  function function_g_signal_connect (instance, detailed_signal, c_handler, data0) result(handler_id)
    use g, only: g_signal_connect_data

    type(c_ptr), intent(in)             :: instance
    character(kind=c_char), intent(in)  :: detailed_signal(*)
    type(c_funptr), intent(in)          :: c_handler
    type(c_ptr), optional, intent(in)   :: data0
    integer(c_long)                     :: handler_id

    if (present(data0)) then
      handler_id =  g_signal_connect_data (instance, detailed_signal, &
                     & c_handler, data0, c_null_funptr, 0_c_int)
    else
      handler_id =  g_signal_connect_data (instance, detailed_signal, &
                     & c_handler, c_null_ptr, c_null_funptr, 0_c_int)
    end if
  end function function_g_signal_connect

  subroutine g_signal_connect (instance, detailed_signal, c_handler, data0)
    type(c_ptr), intent(in)             :: instance
    character(kind=c_char), intent(in)  :: detailed_signal(*)
    type(c_funptr), intent(in)          :: c_handler
    type(c_ptr), optional, intent(in)   :: data0
    integer(c_long)                     :: handler_id

    handler_id = function_g_signal_connect (instance, detailed_signal, c_handler, data0)
  end subroutine g_signal_connect

  ! #define g_signal_connect_swapped(instance, detailed_signal, c_handler, data) \
  !  g_signal_connect_data ((instance), (detailed_signal), (c_handler), (data), NULL, G_CONNECT_SWAPPED)
  ! The instance on which the signal is emitted and data will be swapped when calling the handler.
  ! https://developer.gnome.org/gobject/stable/gobject-Signals.html#g-signal-connect-swapped
  ! We define both a g_signal_connect() function and a subroutine.
  ! You will generally use the subroutine in your programs.

  function function_g_signal_connect_swapped (instance, detailed_signal, c_handler, data0) result(handler_id)
    use g, only: g_signal_connect_data

    type(c_ptr), intent(in)             :: instance
    character(kind=c_char), intent(in)  :: detailed_signal(*)
    type(c_funptr), intent(in)          :: c_handler
    type(c_ptr), intent(in)             :: data0
    integer(c_long)                     :: handler_id

    handler_id =  g_signal_connect_data (instance, detailed_signal, &
                     & c_handler, data0, c_null_funptr, G_CONNECT_SWAPPED)
  end function function_g_signal_connect_swapped

  subroutine g_signal_connect_swapped (instance, detailed_signal, c_handler, data0)
    type(c_ptr), intent(in)             :: instance
    character(kind=c_char), intent(in)  :: detailed_signal(*)
    type(c_funptr), intent(in)          :: c_handler
    type(c_ptr), intent(in)             :: data0
    integer(c_long)                     :: handler_id

    handler_id = function_g_signal_connect_swapped (instance, detailed_signal, c_handler, data0)
  end subroutine g_signal_connect_swapped

end module gtk
