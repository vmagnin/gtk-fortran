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
! Interfaces contributed by Tobias Burnus, Jerry DeLisle, and Vincent Magnin

module gtk
  use iso_c_binding, only: c_int, c_bool, c_char, c_null_char, c_ptr, c_null_ptr
  implicit none

  interface

    function gtk_drawing_area_new() bind(c)
      use iso_c_binding, only: c_ptr
      type(c_ptr) :: gtk_drawing_area_new
    end function 

    function gtk_widget_get_window (widget) bind(c)
      use iso_c_binding, only: c_ptr
      type(c_ptr), value :: widget
      type(c_ptr) :: gtk_widget_get_window
    end function

    function gtk_text_view_new() bind(c)
      use iso_c_binding, only: c_ptr
      type(c_ptr) :: gtk_text_view_new
    end function 

    function gtk_text_view_get_buffer (text_view) bind(c)
      use iso_c_binding, only: c_ptr
      type(c_ptr), value :: text_view
      type(c_ptr) :: gtk_text_view_get_buffer
    end function

    subroutine gtk_text_buffer_set_text (buffer, text, length) bind(c)
      use iso_c_binding, only: c_ptr, c_char, c_int
      character(kind=c_char) :: text(*)
      type(c_ptr), value :: buffer
      integer(c_int), value :: length
    end subroutine

    function gtk_scrolled_window_new (hadjustment, vadjustment) bind(c)
      use iso_c_binding, only: c_ptr
      type(c_ptr), value :: hadjustment, vadjustment
      type(c_ptr) :: gtk_scrolled_window_new
    end function

    function gtk_progress_bar_new() bind(c)
      use iso_c_binding, only: c_ptr
      type(c_ptr) :: gtk_progress_bar_new
    end function gtk_progress_bar_new

    subroutine gtk_progress_bar_set_text (pbar, text) bind(c)
      use iso_c_binding, only: c_ptr, c_char
      character(kind=c_char) :: text(*)
      type(c_ptr), value :: pbar
    end subroutine gtk_progress_bar_set_text

    subroutine gtk_progress_bar_set_fraction (pbar, frac) bind(c)
      use iso_c_binding, only: c_ptr, c_double
      real(c_double), value :: frac
      type(c_ptr), value :: pbar
    end subroutine gtk_progress_bar_set_fraction

    subroutine gtk_progress_bar_pulse (pbar) bind(c)
      use iso_c_binding, only: c_ptr
      type(c_ptr), value :: pbar
    end subroutine gtk_progress_bar_pulse

    subroutine gtk_widget_show (window) bind(c)
      use iso_c_binding, only: c_ptr
      type(c_ptr), value :: window
    end subroutine

    function gtk_window_new (windowtype) bind(c)
      use iso_c_binding, only: c_ptr, c_int
      integer(c_int), value :: windowtype
      type(c_ptr) :: gtk_window_new
    end function gtk_window_new

    function gtk_hbox_new (homogenous, space) bind(c)
      use iso_c_binding, only: c_bool, c_int, c_ptr
      logical(c_bool), value :: homogenous
      integer(c_int), value :: space
      type(c_ptr) :: gtk_hbox_new
    end function gtk_hbox_new

    function gtk_vbox_new (homogenous, space) bind(c)
      use iso_c_binding, only: c_bool, c_int, c_ptr
      logical(c_bool), value :: homogenous
      integer(c_int), value :: space
      type(c_ptr) :: gtk_vbox_new
    end function gtk_vbox_new

    function gtk_button_new_with_label (label) bind(c)
      use iso_c_binding, only: c_ptr, c_char
      character(kind=c_char) :: label(*)
      type(c_ptr) :: gtk_button_new_with_label
    end function gtk_button_new_with_label

    subroutine gtk_container_add (container, object) bind(c)
      use iso_c_binding, only: c_ptr
      type(c_ptr), value :: container, object
    end subroutine gtk_container_add

    subroutine gtk_box_pack_start (box, widget, expand, fill, padding) bind(c)
      use iso_c_binding, only: c_bool, c_int, c_ptr
      type(c_ptr), value :: box, widget
      logical(c_bool), value :: expand, fill
      integer(c_int), value :: padding
    end subroutine

    subroutine gtk_container_set_border_width (container, width) bind(c)
      use iso_c_binding, only: c_int, c_ptr
      type(c_ptr), value :: container
      integer(c_int), value :: width
    end subroutine

    subroutine gtk_window_set_title(window, title) bind(c)
      use iso_c_binding, only: c_ptr, c_char
      type(c_ptr), value :: window
      character(kind=c_char), dimension(*) :: title
    end subroutine gtk_window_set_title

    subroutine gtk_window_set_default_size(window, width, height) bind(c)
      use iso_c_binding, only: c_ptr, c_int
      type(c_ptr), value :: window
      integer(c_int), value :: width, height
    end subroutine gtk_window_set_default_size

    subroutine gtk_main() bind(c)
    end subroutine gtk_main

    subroutine gtk_main_quit() bind(c)
    end subroutine gtk_main_quit

    subroutine g_signal_connect_data (instance, detailed_signal,&
				    & c_handler, gobject, dummy) bind(c)
      use iso_c_binding, only: c_ptr, c_char
      character(c_char) :: detailed_signal(*)
      type(c_ptr), value       :: instance, gobject, c_handler, dummy
    end subroutine g_signal_connect_data

    subroutine gtk_init_real(argc,argv) bind(c,name='gtk_init')
      use iso_c_binding, only: c_int, c_ptr
      integer(c_int) :: argc
      type(c_ptr)    :: argv
    end subroutine 

  end interface 

  character(c_char), parameter :: CNULL = c_null_char
  type(c_ptr), parameter       :: NULL = c_null_ptr
  logical(c_bool), parameter   :: TRUE = .true.
  logical(c_bool), parameter   :: FALSE = .false.
  integer(c_int), parameter    :: GTK_WINDOW_TOPLEVEL = 0
  integer(c_int), parameter    :: GTK_WINDOW_POPUP = 1

contains
  subroutine g_signal_connect (instance, detailed_signal, c_handler)
      use iso_c_binding, only: c_ptr, c_char
      character(c_char):: detailed_signal(*)
      type(c_ptr)      :: instance, c_handler
      call g_signal_connect_data (instance, detailed_signal, c_handler, NULL, NULL)    
  end subroutine g_signal_connect

  subroutine gtk_init()
    use iso_c_binding, only: c_ptr, c_char, c_int, c_null_char, c_loc

    character(len=256,kind=c_char) :: arg
    character(len=1,kind=c_char), dimension(:),pointer :: carg
    type(c_ptr), allocatable, target :: argv(:)
    integer(c_int) :: argc, strlen, i, j

    argc = command_argument_count()
    allocate(argv(0:argc))

    do i = 0, argc
      call get_command_argument (i,arg,strlen)
      allocate(carg(0:strlen))
      do j = 0, strlen-1
        carg(j) = arg(j+1:j+1)
      end do
      carg(strlen) = c_null_char
      argv(i) = c_loc (carg(0))
    end do

    argc = argc + 1
    call gtk_init_real (argc, c_loc(argv))

  end subroutine gtk_init

end module gtk
