! Copyright (C) 2011
! Free Software Foundation, Inc.
!
! This file is part of the gtk-fortran gtk+ Fortran Interface library.
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
! Contributed by James Tappin.
! Last modification: vmagnin 2020-06-03 (GTK 4 version)
!------------------------------------------------------------------------------

module handlers
  use iso_c_binding
!  use gth_hl
  use gtk_hl_container
  use gtk_hl_button
  use gtk, only: gtk_window_set_child, gtk_label_new, &
       & gtk_widget_show, gtk_init, TRUE, FALSE
  use g, only: g_main_loop_new, g_main_loop_run, g_main_loop_quit

  implicit none
  type(c_ptr) :: win, base, nbook, qbut, table
  type(c_ptr) :: my_gmainloop

contains
  subroutine my_destroy(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    print *, "Exit called"
    call g_main_loop_quit(my_gmainloop)
  end subroutine my_destroy

  subroutine bpress(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata
    integer(kind=c_int), pointer :: fdata

    call c_f_pointer(gdata, fdata)
    print *, "Pressed button ", fdata
  end subroutine bpress
end module handlers

program containers
  ! Containers
  ! Test/demo of tables & notebooks.
  use handlers

  implicit none
  integer(kind=c_int) :: ipos
  type(c_ptr) :: junk
  integer(kind=c_int) :: i
  integer(kind=c_int), dimension(6), target :: bval = (/ (i, i = 1,6) /)
  character(len=15) :: ltext

  ! Initialize GTK+
  call gtk_init()

  ! Create a window that will hold the widget system
  win=hl_gtk_window_new('Table/notebook'//c_null_char, destroy=c_funloc(my_destroy))

  ! Now make a column box & put it into the window
  base = hl_gtk_box_new()
  call gtk_window_set_child(win, base)

  ! Make a notebook container
  nbook = hl_gtk_notebook_new()
  call hl_gtk_box_pack(base, nbook)

  ! First page is a 3x6 table
  table=hl_gtk_table_new(homogeneous=TRUE)
  ipos = hl_gtk_notebook_add_page(nbook, table, &
       & label="Example table"//c_null_char)

  do i = 1, 6
     write(ltext, "('Table row',I2)") i
     junk = gtk_label_new(trim(ltext)//c_null_char)
     call hl_gtk_table_attach(table, junk, 0_c_int, i-1_c_int, xspan=2_c_int)
     junk = hl_gtk_button_new("Press"//c_null_char, clicked=c_funloc(bpress), &
          & data = c_loc(bval(i)))
     call hl_gtk_table_attach(table, junk, 2_c_int, i-1_c_int)
  end do

  ! Then 4 relocatable dummy pages
  do i = 1,4
     write(ltext, "('Dummy page',I2)") i+1
     junk = gtk_label_new(trim(ltext)//c_null_char)

     ipos = hl_gtk_notebook_add_page(nbook, junk, label=trim(ltext)//c_null_char, &
          & reorderable=TRUE)
  end do

  ! And a quit button
  junk = hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(my_destroy))
  call hl_gtk_box_pack(base, junk, expand=FALSE)

  ! realize the window
  call gtk_widget_show(win)

  ! Event loop
  my_gmainloop = g_main_loop_new(c_null_ptr, FALSE)
  call g_main_loop_run(my_gmainloop)

end program containers
