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
! Contributed by James Tappin.
! Last modification: vmagnin 02-27-2019

module ln_handlers
  use gtk_hl
  use gtk, only: gtk_button_new, gtk_check_button_new, gtk_container_add, gtk_ent&
       &ry_get_text, gtk_entry_get_text_length, gtk_entry_new, gtk_entry_set_text, gtk&
       &_main, gtk_main_quit, gtk_widget_destroy, gtk_toggle_button_get_active, gtk_to&
       &ggle_button_set_active, gtk_widget_show, gtk_widget_show, gtk_window_new, &
       & gtk_init, GTK_POLICY_NEVER 
  use g, only: alloca, g_object_set_property
  use gdk_pixbuf_hl

  implicit none

  ! The widgets. (Strictly only those that need to be accessed
  ! by the handlers need to go here).
  type(c_ptr) :: ihwin,ihscrollcontain,ihlist, base, qbut, lbl

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
    real(kind=c_double) :: x, x3
    integer(kind=c_int64_t) :: n4
    real(kind=c_float) :: nlog
    character(len=30) :: name
    character(len=10) :: nodd
    character :: code
    type(c_ptr) :: pixbuf
    integer(kind=c_short), dimension(:,:,:), allocatable :: pixels

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
       call hl_gtk_listn_get_cell(ihlist, selections(1), 1_c_int, dvalue=x)
       call hl_gtk_listn_get_cell(ihlist, selections(1), 2_c_int, dvalue=x3)
       call hl_gtk_listn_get_cell(ihlist, selections(1), 4_c_int, l64value=n4)
       call hl_gtk_listn_get_cell(ihlist, selections(1), 3_c_int, fvalue=nlog)
       call hl_gtk_listn_get_cell(ihlist, selections(1), 5_c_int, svalue=nodd)
       call hl_gtk_listn_get_cell(ihlist, selections(1), 6_c_int, svalue=code)
       call hl_gtk_listn_get_cell(ihlist, selections(1), 8_c_int, &
            & pbvalue=pixbuf)
       call hl_gdk_pixbuf_get_pixels(pixbuf, pixels)
       print "('Row:',I3,' Name: ',a,' X:',F7.2,' 3X:',F7.2,' X**4:',I7,&
            &' log(n):',F7.5,' Odd?: ',a, ' Code:',a)", &
            & selections(1), trim(name), x, x3, n4, nlog, nodd, code
       print *, "Pixels: ", pixels(:,3,3)
    end if

    deallocate(selections)
  end subroutine list_select

  subroutine cell_edited(renderer, path, text, gdata) bind(c)
    type(c_ptr), value :: renderer, path, text, gdata

    ! Callback for edited cells. 
    character(len=200) :: fpath, ftext
    integer(kind=c_int) :: irow
    integer(kind=c_int), pointer :: icol
    integer :: ios
    type(c_ptr) :: pcol, list
    real(kind=c_double) :: x

    call convert_c_string(path, fpath)
    read(fpath, *) irow
    pcol = g_object_get_data(renderer, "column-number"//c_null_char)
    call c_f_pointer(pcol, icol)
    call convert_c_string(text, ftext)
    list = g_object_get_data(renderer, "view"//c_null_char)

    print *, "Edit in column", icol

    if (icol == 0) then
       call hl_gtk_listn_set_cell(list, irow, icol, &
            & svalue=trim(ftext))
    else
       read(ftext, *, iostat=ios) x
       if (ios /= 0) return
       call hl_gtk_listn_set_cell(ihlist, irow, 2_c_int, dvalue=3*x)
       call hl_gtk_listn_set_cell(ihlist, irow, 3_c_int, dvalue=log10(x))
       call hl_gtk_listn_set_cell(ihlist, irow, 4_c_int, &
            & l64value=int(x**4,c_int64_t))
       call hl_gtk_listn_set_cell(ihlist, irow, 5_c_int, &
            & ivalue=mod(int(x, c_int),2_c_int))
       call hl_gtk_listn_set_cell(ihlist, irow, 7_c_int, &
            & ivalue=mod(int(3*x, c_int),100_c_int))
       call hl_gtk_listn_set_cell(ihlist, irow, 1_c_int, dvalue=x)
    end if
  end subroutine cell_edited

  subroutine ccell_edit(renderer, path, text, gdata) bind(c)
    type(c_ptr), value :: renderer, path, text, gdata

    ! Basic callback to report what's called
    character(len=200) :: fpath, ftext
    integer(kind=c_int) :: irow

    call c_f_string(path, fpath)
    call c_f_string(text, ftext)
    read(fpath, *) irow

    print *, "Combo sent edited signal from ", trim(fpath)
    print *, "Text was ", trim(ftext)
    call hl_gtk_listn_set_cell(ihlist, irow, 9_c_int, svalue=trim(ftext))

  end subroutine ccell_edit

  subroutine ccell_changed(renderer, path, iter, gdata) bind(c)
    type(c_ptr), value :: renderer, path, iter, gdata

    ! Basic callback to report what's called
    character(len=200) :: fpath

    call c_f_string(path, fpath)
    print *, "Combo sent changed signal from ", trim(fpath)

  end subroutine ccell_changed
  
  subroutine cell_clicked(renderer, path, gdata) bind(c)
    type(c_ptr), value :: renderer, path, gdata

    character(len=200) :: fpath
    integer(kind=c_int) :: irow
    integer(kind=c_int), pointer :: icol
    type(c_ptr) :: pcol, list
    logical :: state

    call convert_c_string(path, fpath)
    read(fpath, *) irow

    pcol = g_object_get_data(renderer, "column-number"//c_null_char)
    call c_f_pointer(pcol, icol)

    list = g_object_get_data(renderer, "view"//c_null_char)

    state = c_f_logical(gtk_cell_renderer_toggle_get_active(renderer))

    print *, "Changed state in row", irow, " to ", .not. state

    call hl_gtk_listn_set_cell(list, irow, icol, &
         & logvalue= .not. state)

  end subroutine cell_clicked

  subroutine rcell_clicked(renderer, path, gdata) bind(c)
    type(c_ptr), value :: renderer, path, gdata

    ! Default callback for a toggle button in a list
    !
    ! RENDERER: c_ptr: required: The renderer which sent the signal
    ! PATH: c_ptr: required: The path at which to insert
    ! GDATA: c_ptr: required: User data, Not used.
    !
    ! The column number is passed via the "column-number" gobject data value.
    ! The treeview containing the cell is passed via the "view" gobject
    ! data value.
    ! The row number is passed as a string in the PATH argument.
    ! This routine is not normally called by the application developer.
    !-
    character(len=200) :: fpath
    integer(kind=c_int) :: irow
    integer(kind=c_int), pointer :: icol
    integer(kind=c_int) :: i
    type(c_ptr) :: pcol, list
    logical :: state
    integer(kind=c_int) :: nrows

    call convert_c_string(path, fpath)
    read(fpath, *) irow

    pcol = g_object_get_data(renderer, "column-number"//c_null_char)
    call c_f_pointer(pcol, icol)

    list = g_object_get_data(renderer, "view"//c_null_char)

    state = c_f_logical(gtk_cell_renderer_toggle_get_active(renderer))
    print *, irow, state

    if (state) return ! Don't act on an unset

    ! Find the first iterator
    nrows = gtk_tree_model_iter_n_children (gtk_tree_view_get_model(list), &
         & c_null_ptr)
    do i = 0,nrows-1
       call hl_gtk_listn_set_cell(list, i, icol, &
            & logvalue= i == irow)
    end do
  end subroutine rcell_clicked

  subroutine display_dbl(col, cell, model, iter, data) bind(c)
    type(c_ptr), value :: col, cell, model, iter, data

    ! Formatting routine attached via hl_gtk_listn_set_cell_data_func
    ! Note that the column index is passed via the DATA argument, so
    ! far as I can see the only other way is to use constants.
    character(len=20) :: rstring
    real(kind=c_double) :: dval
    type(gvalue), target :: dvalue, svalue
    type(c_ptr) :: val_ptr
    integer(kind=c_int), pointer :: colno

    call c_f_pointer(data, colno)

    call gtk_tree_model_get_value(model, iter, colno, c_loc(dvalue))
    dval = g_value_get_double(c_loc(dvalue))

    write(rstring, "(f0.1)") dval

    val_ptr = c_loc(svalue)
    val_ptr = g_value_init(val_ptr, G_TYPE_STRING)

    call g_value_set_string(val_ptr, trim(rstring)//c_null_char)
    call g_object_set_property(cell, "text"//c_null_char, val_ptr)
  end subroutine display_dbl
end module ln_handlers

program list_rend
  ! LIST_REND
  ! Demo of multi column list, with renderers

  use ln_handlers

  implicit none

  integer, parameter :: ncols = 11, nrows=10
  character(len=35) :: line
  integer(kind=c_int) :: i, ltr
  integer(kind=type_kind), dimension(ncols) :: ctypes
  character(len=20), dimension(ncols) :: titles, renderers
  integer(kind=c_int), dimension(ncols) :: editable
  integer(kind=c_int), dimension(ncols) :: widths
  integer(kind=c_int), dimension(2), target :: fmt_col = [1, 2]
  integer(kind=c_short), dimension(3, 100, 24) :: image
  integer(kind=c_short), dimension(nrows) :: red, green, blue
  type(c_ptr) :: pixbuf

  red =   [0_c_short, 255_c_short, 255_c_short,   0_c_short,   0_c_short,&
          &0_c_short, 255_c_short, 255_c_short, 85_c_short, 170_c_short]
  green = [0_c_short, 255_c_short,   0_c_short, 255_c_short,   0_c_short,&
          &255_c_short,   0_c_short, 255_c_short, 85_c_short, 170_c_short]
  blue =  [0_c_short, 255_c_short,   0_c_short,   0_c_short, 255_c_short,&
          &255_c_short, 255_c_short,   0_c_short, 85_c_short, 170_c_short]

  ! Initialize GTK+
  call gtk_init()

  ! Create a window that will hold the widget system
  ihwin=hl_gtk_window_new('Renderers list demo'//c_null_char, &
       & destroy=c_funloc(my_destroy))

  ! Now make a column box & put it into the window
  base = hl_gtk_box_new()
  call gtk_container_add(ihwin, base)

  ! Now make a multi column list with multiple selections enabled
  ctypes = (/ G_TYPE_STRING, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, &
       & G_TYPE_UINT64, G_TYPE_BOOLEAN, G_TYPE_BOOLEAN, G_TYPE_INT,&
       & gdk_pixbuf_get_type(), G_TYPE_STRING , G_TYPE_BOOLEAN /)
  editable = (/ TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, &
       & FALSE, TRUE, TRUE /)
  widths = [-1, -1, -1, -1, -1, -1, -1, 150, -1, -1, -1]

  titles = (/ character(len=20) :: "Name", "N", "3N", "Log(n)", &
       & "N**4", "Odd?", "Select?", "Fraction", "Colour", "Choose", "Pick" /)
  renderers = (/ hl_gtk_cell_text, hl_gtk_cell_spin, hl_gtk_cell_text, &
       & hl_gtk_cell_text, hl_gtk_cell_text, hl_gtk_cell_text,&
       & hl_gtk_cell_toggle, hl_gtk_cell_progress, hl_gtk_cell_pixbuf, &
       & hl_gtk_cell_combo, hl_gtk_cell_radio /)

  ihlist = hl_gtk_listn_new(types=ctypes, &
       & changed=c_funloc(list_select),&
       & multiple=TRUE, titles=titles, width=widths, &
       & renderers=renderers, editable=editable, &
       & edited=c_funloc(cell_edited), toggled=c_funloc(cell_clicked), &
       & toggled_radio=c_funloc(rcell_clicked), &
       & edited_combo=c_funloc(ccell_edit), &
       & changed_combo=c_funloc(ccell_changed))

  call hl_gtk_listn_config_spin(ihlist, 1_c_int, vmax = huge(1._c_double), &
       & step = 0.1_c_double, digits=1_c_int)
  call hl_gtk_listn_config_combo(ihlist, 9_c_int, &
       & vals=['one  ', 'two  ', 'three'], &
       & has_entry=FALSE)

  do i = 1, size(fmt_col)
     call hl_gtk_listn_set_cell_data_func(ihlist, fmt_col(i), &
          & func=c_funloc(display_dbl), &
          & data=c_loc(fmt_col(i)))
  end do
  ! Now put <nrows> rows into it
  call hl_gtk_listn_ins(ihlist, count=nrows)
  do i=1,nrows 
     write(line,"('List entry number ',I0)") i
     ltr=len_trim(line)+1
     line(ltr:ltr)=c_null_char
     call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 0_c_int, svalue=line)
     call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 1_c_int, &
          & dvalue=real(i, c_double))
     call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 2_c_int, &
          & dvalue=real(3*i, c_double))
     call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 3_c_int, &
          & fvalue=log10(real(i)))
     call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 4_c_int, &
          & l64value=int(i, c_int64_t)**4)
     call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 5_c_int, &
          & ivalue=mod(i,2_c_int))
     call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 6_c_int, &
          & logvalue=mod(i,3_c_int) == 0)
     call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 7_c_int, &
          & ivalue=mod(3_c_int*i, 100_c_int))
     image(1,:,:) = red(i)
     image(2,:,:) = green(i)
     image(3,:,:) = blue(i)
     pixbuf = hl_gdk_pixbuf_new(image)
     call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 8_c_int, pbvalue=pixbuf)
     call hl_gtk_listn_combo_set_select(ihlist, i-1_c_int, 9_c_int, &
          & selection=mod(i,3_c_int))
     call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 10_c_int, logvalue= i==4)
  end do

  ! It is the scrollcontainer that is placed into the box.
  call hl_gtk_box_pack(base, ihlist)

  ! Add a note about editable columns
  lbl = gtk_label_new('The "Name", "N" and "Select?" columns are editable'&
       &//c_null_char)
  call hl_gtk_box_pack(base, lbl)

  ! Also a quit button
  qbut = hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(my_destroy))
  call hl_gtk_box_pack(base,qbut)

  ! realize the window
  call gtk_widget_show(ihwin)

  ! Event loop
  call gtk_main()

end program list_rend
