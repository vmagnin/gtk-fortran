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
! Last modification: vmagnin 02-20-2019

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
       &  qbut, lbl

contains
  subroutine my_destroy(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    print *, "Exit called"
    call gtk_main_quit ()
  end subroutine my_destroy

  subroutine list_select(list, gdata) bind(c)
    type(c_ptr), value :: list, gdata

    integer(kind=c_int) :: nsel
    integer(kind=c_int), dimension(:), allocatable :: selections
    integer(kind=c_int) :: n, n3
    integer(kind=c_int64_t) :: n4
    real(kind=c_float) :: nlog
    character(len=30) :: name
    character(len=10) :: nodd
    character :: code, ucode
    integer(kind=c_int) :: icode, iucode

    nsel = hl_gtk_listn_get_selections(C_NULL_PTR, selections, list)
    if (nsel == 0) then
       print *, "No selection"
       return
    end if

    ! Find and print the selected row(s)
    print *, nsel,"Rows selected"
    print *, selections
    if (nsel == 1) then
       call hl_gtk_listn_get_cell(ihlist, selections(1), 0_c_int, svalue=name)
       call hl_gtk_listn_get_cell(ihlist, selections(1), 1_c_int, ivalue=n)
       call hl_gtk_listn_get_cell(ihlist, selections(1), 2_c_int, ivalue=n3)
       call hl_gtk_listn_get_cell(ihlist, selections(1), 4_c_int, l64value=n4)
       call hl_gtk_listn_get_cell(ihlist, selections(1), 3_c_int, fvalue=nlog)
       call hl_gtk_listn_get_cell(ihlist, selections(1), 5_c_int, svalue=nodd)
       call hl_gtk_listn_get_cell(ihlist, selections(1), 6_c_int, svalue=code)
       call hl_gtk_listn_get_cell(ihlist, selections(1), 7_c_int, svalue=ucode)
       call hl_gtk_listn_get_cell(ihlist, selections(1), 6_c_int, ivalue=icode)
       call hl_gtk_listn_get_cell(ihlist, selections(1), 7_c_int, ivalue=iucode)
       print "('Row:',I3,' Name: ',a,' N:',I3,' 3N:',I4,' N**4:',I7,&
            &' log(n):',F7.5,' Odd?: ',a, ' Code:',a,a)", &
            & selections(1), trim(name), n, n3, n4, nlog, nodd, code, ucode
       print *, ichar(code), ichar(ucode)
       print *, icode, iucode
    end if

    deallocate(selections)
  end subroutine list_select

  subroutine display_int(col, cell, model, iter, data) bind(c)
    type(c_ptr), value :: col, cell, model, iter, data

    ! Formatting routine attached via hl_gtk_listn_set_cell_data_func
    ! Note that the column index is passed via the DATA argument, so
    ! far as I can see the only other way is to use constants.

    character(len=10) :: rstring
    integer(kind=c_int) :: ival
    type(gvalue), target :: ivalue, svalue
    type(c_ptr) :: val_ptr
    integer(kind=c_int), pointer :: colno

    call c_f_pointer(data, colno)

    call gtk_tree_model_get_value(model, iter, colno, c_loc(ivalue))
    ival = g_value_get_int(c_loc(ivalue))

    write(rstring, "(I7.6)") ival

    val_ptr = c_loc(svalue)
    val_ptr = g_value_init(val_ptr, G_TYPE_STRING)

    call g_value_set_string(val_ptr, trim(rstring)//c_null_char)
    call g_object_set_property(cell, "text"//c_null_char, val_ptr)
  end subroutine display_int

  subroutine cell_edited(renderer, path, text, gdata) bind(c)
    type(c_ptr), value :: renderer, path, text, gdata

    ! Callback for edited cells. 

    character(len=200) :: fpath, ftext
    integer(kind=c_int) :: irow
    integer(kind=c_int), pointer :: icol
    integer :: ios
    type(c_ptr) :: pcol, list
    integer(kind=c_int) :: n

    call convert_c_string(path, fpath)
    read(fpath, *) irow
    pcol = g_object_get_data(renderer, "column-number"//c_null_char)
    call c_f_pointer(pcol, icol)
    call convert_c_string(text, ftext)
    list = g_object_get_data(renderer, "view"//c_null_char)

    if (icol == 0) then
       call hl_gtk_listn_set_cell(list, irow, icol, &
            & svalue=trim(ftext))
    else
       read(ftext, *, iostat=ios) n
       if (ios /= 0) return
       call hl_gtk_listn_set_cell(ihlist, irow, 2_c_int, &
            & ivalue=3_c_int*n)
       call hl_gtk_listn_set_cell(ihlist, irow, 3_c_int, fvalue=log10(real(n)))
       call hl_gtk_listn_set_cell(ihlist, irow, 4_c_int, &
            & l64value=int(n,c_int64_t)**4)
       call hl_gtk_listn_set_cell(ihlist, irow, 5_c_int, ivalue=mod(n,2_c_int))
       ! Note we set the N value last as this is a sortable column, if we
       ! are sorted on ODD/EVEN it will probably still go wrong.
       call hl_gtk_listn_set_cell(ihlist, irow, 1_c_int, ivalue=n)
    end if
  end subroutine cell_edited

end module ln_handlers

program list_n
  ! LIST_N
  ! Demo of multi column list

  use ln_handlers

  implicit none

  character(len=35) :: line
  integer(kind=c_int) :: i, ltr
  integer(kind=type_kind), dimension(8) :: ctypes
  character(len=20), dimension(8) :: titles
  integer(kind=c_int), dimension(8) :: sortable, editable
  integer(kind=c_int), target :: fmt_col = 2
  character(kind=c_char), dimension(10) :: codes
  ! Initialize GTK+
  call gtk_init()

  ! Create a window that will hold the widget system
  ihwin=hl_gtk_window_new('multi-column list demo'//c_null_char, &
       & destroy=c_funloc(my_destroy))

  ! Now make a column box & put it into the window
  base = hl_gtk_box_new()
  call gtk_container_add(ihwin, base)

  ! Now make a multi column list with multiple selections enabled
  ctypes = (/ G_TYPE_STRING, G_TYPE_INT, G_TYPE_INT, G_TYPE_FLOAT, &
       & G_TYPE_UINT64, G_TYPE_BOOLEAN, G_TYPE_CHAR, G_TYPE_UCHAR /)
  sortable = (/ FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE /)
  editable = (/ TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE /)
  codes = [ 'A', 'X', 'B', '?', 'A', 'C', char(185), 'u', '*', char(201)]

  titles(1) = "Name"
  titles(2) = "N"
  titles(3) = "3N"
  titles(4) = "Log(n)"
  titles(5) = "N**4"
  titles(6) = "Odd?"
  titles(7) = "Code"
  titles(8) = "Ucode"

  ihlist = hl_gtk_listn_new(ihscrollcontain, types=ctypes, &
       & changed=c_funloc(list_select),&
       & multiple=TRUE, height=250_c_int, swidth=600_c_int, titles=titles, &
       & sortable=sortable, editable=editable, &
       & edited=c_funloc(cell_edited))

  call hl_gtk_listn_set_cell_data_func(ihlist, fmt_col, &
       & func=c_funloc(display_int), &
       & data=c_loc(fmt_col))

  ! Now put 10 rows into it
  do i=1,10
     call hl_gtk_listn_ins(ihlist)
     write(line,"('List entry number ',I0)") i
     ltr=len_trim(line)+1
     line(ltr:ltr)=c_null_char
     call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 0_c_int, svalue=line)
     call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 1_c_int, ivalue=i)
     call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 2_c_int, ivalue=3_c_int*i)
     call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 3_c_int, &
          & fvalue=log10(real(i)))
     call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 4_c_int, &
          & l64value=int(i,c_int64_t)**4)
     call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 5_c_int, &
          & ivalue=mod(i,2_c_int))
     call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 6_c_int, svalue=codes(i))
     call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 7_c_int, svalue=codes(i))
  end do

  ! A silly idea to test a hunch
  print *, codes
  codes = char(ichar(codes)-2)
  print *, codes

  ! It is the scrollcontainer that is placed into the box.
  call hl_gtk_box_pack(base, ihscrollcontain)

  ! Add a note about editable columns
  lbl = gtk_label_new("The ""Name"" and ""N"" columns are editable"//c_null_char)
  call hl_gtk_box_pack(base, lbl)

  ! Also a quit button
  qbut = hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(my_destroy))
  call hl_gtk_box_pack(base,qbut)

  ! realize the window

  print *, "Created"
  call gtk_widget_show_all(ihwin)
  print *, "Realized"

  ! Event loop

  call gtk_main()

end program list_n
