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
! gfortran -g gtk.f90 gtk-sup.f90 gtk-hl.f90 hl_list_n.f90 `pkg-config --cflags --libs gtk+-2.0`
! Contributed by James Tappin.

module ln_handlers
  use gtk_hl
  use gtk, only: gtk_button_new, gtk_check_button_new, gtk_container_add, gtk_ent&
       &ry_get_text, gtk_entry_get_text_length, gtk_entry_new, gtk_entry_set_text, gtk&
       &_main, gtk_main_quit, gtk_widget_destroy, gtk_toggle_button_get_active, gtk_to&
       &ggle_button_set_active, gtk_widget_show, gtk_widget_show_all, gtk_window_new, &
       & gtk_init
  use g, only: alloca, g_object_set_property


  implicit none

  ! The widgets. (Strictly only those that need to be accessed
  ! by the handlers need to go here).

  type(c_ptr) :: ihwin,ihscrollcontain,ihlist, base, &
       &  qbut

contains
  subroutine my_destroy(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata
    print *, "Exit called"
    call gtk_widget_destroy(ihwin)
    call gtk_main_quit ()
  end subroutine my_destroy

  subroutine list_select(list, gdata) bind(c)
    type(c_ptr), value :: list, gdata

    integer, pointer :: fdata
    integer(kind=c_int) :: nsel
    integer(kind=c_int), dimension(:), allocatable :: selections
    integer(kind=c_int) :: n, n3
    integer(kind=c_int64_t) :: n4
    real(kind=c_float) :: nlog
    character(len=30) :: name
    character(len=10) :: nodd

    nsel = hl_gtk_listn_get_selections(NULL, selections, list)
    if (nsel == 0) then
       print *, "No selection"
       return
    end if

    ! Find and print the selected row(s)
    print *, nsel,"Rows selected"
    print *, selections
    if (nsel == 1) then
       call hl_gtk_listn_get_cell(ihlist, selections(1), 0, svalue=name)
       call hl_gtk_listn_get_cell(ihlist, selections(1), 1, ivalue=n)
       call hl_gtk_listn_get_cell(ihlist, selections(1), 2, ivalue=n3)
       call hl_gtk_listn_get_cell(ihlist, selections(1), 4, l64value=n4)
       call hl_gtk_listn_get_cell(ihlist, selections(1), 3, fvalue=nlog)
       call hl_gtk_listn_get_cell(ihlist, selections(1), 5, svalue=nodd)
       print "('Row:',I3,' Name: ',a,' N:',I3,' 3N:',I4,' N**4:',I7,&
            &' log(n):',F7.5,' Odd?: ',a)", selections(1), trim(name), &
            & n, n3, n4, nlog, nodd
    end if

    deallocate(selections)
  end subroutine list_select

  subroutine display_int(col, cell, model, iter, data) bind(c)
    type(c_ptr), value :: col, cell, model, iter, data

    character(len=10) :: rstring
    integer(kind=c_int) :: ival
    type(gvalue), target :: ivalue, svalue
    type(c_ptr) :: val_ptr
    integer(kind=c_int), pointer :: colno

    call c_f_pointer(data, colno)

    call gtk_tree_model_get_value(model, iter, colno, c_loc(ivalue))
    ival = g_value_get_int(c_loc(ivalue))

    write(rstring, "(I7.7)") ival

    val_ptr = c_loc(svalue)
    val_ptr = g_value_init(val_ptr, G_TYPE_STRING)

    call g_value_set_string(val_ptr, trim(rstring)//cnull)
    call g_object_set_property(cell, "text"//cnull, val_ptr)
  end subroutine display_int
end module ln_handlers

program list_n
  ! LIST_N
  ! Demo of multi column list

  use ln_handlers

  implicit none

  character(len=35) :: line
  integer :: i, ltr
  integer, target :: iappend=0, idel=0
  integer(kind=type_kind), dimension(6) :: ctypes
  character(len=20), dimension(6) :: titles
  integer(kind=c_int), dimension(6) :: sortable
  integer(kind=c_int), target :: fmt_col = 2
  ! Initialize GTK+
  call gtk_init()

  ! Create a window that will hold the widget system
  ihwin=hl_gtk_window_new('multi-column list demo'//cnull, destroy=c_funloc(my_destroy))

  ! Now make a column box & put it into the window
  base = hl_gtk_box_new()
  call gtk_container_add(ihwin, base)

  ! Now make a multi column list with multiple selections enabled
  ctypes = (/ G_TYPE_STRING, G_TYPE_INT, G_TYPE_INT, G_TYPE_FLOAT, &
       & G_TYPE_UINT64, G_TYPE_BOOLEAN /)
  sortable = (/ FALSE, TRUE, FALSE, FALSE, FALSE, TRUE /)
  titles(1) = "Name"
  titles(2) = "N"
  titles(3) = "3N"
  titles(4) = "Log(n)"
  titles(5) = "N**4"
  titles(6) = "Odd?"

  ihlist = hl_gtk_listn_new(ihscrollcontain, types=ctypes, &
       & changed=c_funloc(list_select),&
       & multiple=TRUE, height=250, swidth=400, titles=titles, &
       & sortable=sortable)

  call hl_gtk_listn_set_cell_data_func(ihlist, fmt_col, func=c_funloc(display_int), &
       & data=c_loc(fmt_col))

  ! Now put 10 rows into it
  do i=1,10
     call hl_gtk_listn_ins(ihlist)
     write(line,"('List entry number ',I0)") i
     ltr=len_trim(line)+1
     line(ltr:ltr)=cnull
     call hl_gtk_listn_set_cell(ihlist, i-1, 0, svalue=line)
     call hl_gtk_listn_set_cell(ihlist, i-1, 1, ivalue=i)
     call hl_gtk_listn_set_cell(ihlist, i-1, 2, ivalue=3*i)
     call hl_gtk_listn_set_cell(ihlist, i-1, 3, fvalue=log10(real(i)))
     call hl_gtk_listn_set_cell(ihlist, i-1, 4, l64value=int(i,c_int64_t)**4)
     call hl_gtk_listn_set_cell(ihlist, i-1, 5, ivalue=mod(i,2))
  end do

  ! It is the scrollcontainer that is placed into the box.
  call hl_gtk_box_pack(base, ihscrollcontain)

  ! Also a quit button
  qbut = hl_gtk_button_new("Quit"//cnull, clicked=c_funloc(my_destroy))
  call hl_gtk_box_pack(base,qbut)

  ! realize the window

  call gtk_widget_show_all(ihwin)

  ! Event loop

  call gtk_main()

end program list_n
