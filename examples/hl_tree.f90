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
! Last modification: vmagnin 02-20-2019, 2020-07-14
!------------------------------------------------------------------------------

module tr_handlers
  use gtk_hl_container
  use gtk_hl_button
  use gtk_hl_tree
  use gtk, only: gtk_button_new, gtk_widget_show, gtk_window_destroy, &
               & gtk_window_set_child
  use g, only: alloca

  implicit none
  ! The widgets. (Strictly only those that need to be accessed
  ! by the handlers need to go here).
  type(c_ptr) :: ihwin,ihscrollcontain,ihlist, base, &
       &  qbut, dbut, lbl

contains

  subroutine my_destroy(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata
    print *, "Exit called"
    call gtk_window_destroy(ihwin)
  end subroutine my_destroy

  subroutine del_row(but, gdata) bind(c)
    type(c_ptr), value, intent(in) :: but, gdata
    integer(kind=c_int), dimension(:,:), allocatable :: selections
    integer(kind=c_int), dimension(:), allocatable :: dep
    integer(kind=c_int) :: nsel

    nsel = hl_gtk_tree_get_selections(ihlist, selections, &
         & depths=dep)

    if (nsel /= 1) then
       print *, "Not a single selection"
       return
    end if

    call hl_gtk_tree_rem(ihlist, selections(:dep(1),1))

    call gtk_widget_set_sensitive(but, FALSE)
  end subroutine del_row

  subroutine list_select(list, gdata) bind(c)
    type(c_ptr), value, intent(in) :: list, gdata
    integer(kind=c_int) :: nsel
    integer(kind=c_int), dimension(:,:), allocatable :: selections
    integer(kind=c_int), dimension(:), allocatable :: dep
    integer(kind=c_int) :: n, n3
    integer(kind=c_int64_t) :: n4
    real(kind=c_float) :: nlog
    character(len=30) :: name
    character(len=10) :: nodd
    integer :: i
    nsel = hl_gtk_tree_get_selections(C_NULL_PTR, selections, selection=list, &
         & depths=dep)
    if (nsel == 0) then
       print *, "No selection"
       return
    end if

    ! Find and print the selected row(s)
    print *, nsel,"Rows selected"
    print *, "Depths", dep
    print *, "Rows"
    do i = 1, nsel
       print *, selections(:dep(i),i)
    end do

    if (nsel == 1) then
       call hl_gtk_tree_get_cell(ihlist, selections(:dep(1),1), 0_c_int, &
            & svalue=name)
       call hl_gtk_tree_get_cell(ihlist, selections(:dep(1),1), 1_c_int, &
            & ivalue=n)
       call hl_gtk_tree_get_cell(ihlist, selections(:dep(1),1), 2_c_int, &
            & ivalue=n3)
       call hl_gtk_tree_get_cell(ihlist, selections(:dep(1),1), 4_c_int, &
            & l64value=n4)
       call hl_gtk_tree_get_cell(ihlist, selections(:dep(1),1), 3_c_int, &
            & fvalue=nlog)
       call hl_gtk_tree_get_cell(ihlist, selections(:dep(1),1), 5_c_int,&
            & svalue=nodd)
       print "('Name: ',a,' N:',I3,' 3N:',I4,' N**4:',I7,&
            &' log(n):',F7.5,' Odd?: ',a)", trim(name), &
            & n, n3, n4, nlog, nodd
       call gtk_widget_set_sensitive(dbut, TRUE)
    else
       call gtk_widget_set_sensitive(dbut, FALSE)
    end if

    deallocate(selections)
  end subroutine list_select

  subroutine activate(app, gdata) bind(c)
    use iso_c_binding, only: c_ptr, c_funloc, c_null_char
    use gtk, only: gtk_application_window_new, gtk_window_set_title
    implicit none
    type(c_ptr), value, intent(in)  :: app, gdata
    character(len=35) :: line
    integer(kind=c_int) :: i, ltr, j
    integer(kind=type_kind), dimension(6) :: ctypes
    character(len=20), dimension(6) :: titles
    integer(kind=c_int), dimension(6) :: sortable, editable

    ! Create a window that will hold the widget system
    ihwin = gtk_application_window_new(app)
    call gtk_window_set_title(ihwin, "Tree view demo"//c_null_char)

    ! Now make a column box & put it into the window
    base = hl_gtk_box_new()
    call gtk_window_set_child(ihwin, base)

    ! Now make a multi column list with multiple selections enabled
    ctypes = (/ G_TYPE_STRING, G_TYPE_INT, G_TYPE_INT, G_TYPE_FLOAT, &
         & G_TYPE_UINT64, G_TYPE_BOOLEAN /)
    sortable = (/ FALSE, TRUE, FALSE, FALSE, FALSE, TRUE /)
    editable = (/ TRUE, FALSE, TRUE, FALSE, FALSE, FALSE /)

    titles(1) = "Name"
    titles(2) = "N"
    titles(3) = "3N"
    titles(4) = "Log(n)"
    titles(5) = "N**4"
    titles(6) = "Odd?"

    ihlist = hl_gtk_tree_new(ihscrollcontain, types=ctypes, &
         & changed=c_funloc(list_select),&
         &  multiple=TRUE, height=250_c_int, swidth=400_c_int, titles=titles, &
         & sortable=sortable, editable=editable)

    ! Now put 10 top level rows into it
    do i=1,10
       call hl_gtk_tree_ins(ihlist, row = (/ -1_c_int /))
       write(line,"('List entry number ',I0)") i
       ltr=len_trim(line)+1
       line(ltr:ltr)=c_null_char
       call hl_gtk_tree_set_cell(ihlist, absrow=i-1_c_int, col=0_c_int, &
            & svalue=line)
       call hl_gtk_tree_set_cell(ihlist, absrow=i-1_c_int, col=1_c_int, &
            & ivalue=i)
       call hl_gtk_tree_set_cell(ihlist, absrow=i-1_c_int, col=2_c_int, &
            & ivalue=3_c_int*i)
       call hl_gtk_tree_set_cell(ihlist, absrow=i-1_c_int, col=3_c_int, &
            & fvalue=log10(real(i)))
       call hl_gtk_tree_set_cell(ihlist, absrow=i-1_c_int, col=4_c_int, &
            & l64value=int(i,c_int64_t)**4)
       call hl_gtk_tree_set_cell(ihlist, absrow=i-1_c_int, col=5_c_int, &
            & ivalue=mod(i,2_c_int))
    end do

    ! Add some child rows
    do j = 2, 6, 2
       call hl_gtk_tree_ins(ihlist, row = (/ j, -1_c_int /), count=5)
       do i = 1, 5
          write(line,"('List entry number',I0,':',I0)") j+1,i
          ltr=len_trim(line)+1
          line(ltr:ltr)=c_null_char
          call hl_gtk_tree_set_cell(ihlist, row=(/ j, i-1_c_int/), col=0_c_int, &
               & svalue=line)
          call hl_gtk_tree_set_cell(ihlist, row=(/ j, i-1_c_int/), col=1_c_int, &
               & ivalue=i)
          call hl_gtk_tree_set_cell(ihlist, row=(/ j, i-1_c_int/), col=2_c_int, &
               & ivalue=3_c_int*i)
          call hl_gtk_tree_set_cell(ihlist, row=(/ j, i-1_c_int/), col=3_c_int, &
               & fvalue=log10(real(i)))
          call hl_gtk_tree_set_cell(ihlist, row=(/ j, i-1_c_int/), col=4_c_int, &
               & l64value=int(i,c_int64_t)**4)
          call hl_gtk_tree_set_cell(ihlist, row=(/ j, i-1_c_int/), col=5_c_int, &
               & ivalue=mod(i,2_c_int))
       end do
    end do

    ! It is the scrollcontainer that is placed into the box.
    call hl_gtk_box_pack(base, ihscrollcontain)

    ! Add a note about editable columns
    lbl = gtk_label_new("The ""Name"" and ""3N"" columns are editable"&
         & //c_null_char)
    call hl_gtk_box_pack(base, lbl)

    ! Delete selected row
    dbut = hl_gtk_button_new("Delete selected row"//c_null_char, &
         & clicked=c_funloc(del_row), &
         & tooltip="Delete the selected row"//c_null_char, sensitive=FALSE)

    call hl_gtk_box_pack(base, dbut)

    ! Also a quit button
    qbut = hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(my_destroy))
    call hl_gtk_box_pack(base,qbut)

    ! realize the window
    call gtk_widget_show(ihwin)
  end subroutine activate
end module tr_handlers

program tree
  ! TREE
  ! Demo of a tree
  use iso_c_binding, only: c_ptr, c_funloc, c_null_char
  use tr_handlers

  implicit none
  type(c_ptr)        :: app

  app = hl_gtk_application_new("gtk-fortran.examples.hl_tree"//c_null_char, &
                             & c_funloc(activate))
end program tree
