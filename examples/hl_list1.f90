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
! Last modifications: jerryd 2020-06-05 (GTK 4), vmagnin 2020-12-20
!------------------------------------------------------------------------------

module l1_handlers
!  use gtk_hl
  use gtk_hl_container
  use gtk_hl_button
  use gtk_hl_tree
  use gtk_hl_entry
  use gtk, only: gtk_button_new, gtk_check_button_new, &
       & gtk_entry_get_text_length, gtk_entry_new, &
       & gtk_entry_get_buffer, gtk_entry_buffer_get_text, &
       & gtk_entry_buffer_set_text, &
       & gtk_check_button_get_active,&
       & gtk_check_button_set_active, gtk_widget_show, &
       & gtk_window_destroy, gtk_window_set_child
  use g, only: alloca

  implicit none
  ! The widgets. (Strictly only those that need to be accessed
  ! by the handlers need to go here).
  type(c_ptr) :: ihwin,ihscrollcontain,ihlist, base, &
       & newline, qbut, dbut, dabut, jbox, jbox2, abut, swbut

contains
  subroutine my_destroy(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata

    print *, "Exit called"
    call gtk_window_destroy(ihwin)
  end subroutine my_destroy

  recursive subroutine list_select(list, gdata) bind(c)
    type(c_ptr), value, intent(in) :: list, gdata

    integer, pointer :: fdata
    integer(kind=c_int) :: nsel
    integer(kind=c_int), dimension(:), allocatable :: selections

    if (c_associated(gdata)) then
       call c_f_pointer(gdata, fdata)
       nsel = hl_gtk_list1_get_selections(C_NULL_PTR, selections, list)
       if (nsel == 0) return

       if (fdata == 0) then
          ! Find and print the selected row(s)
          print *, nsel,"Rows selected"
          print *, selections
          deallocate(selections)
       else    ! Delete the selected row
          call gtk_check_button_set_active(dbut, FALSE)
          fdata = 0
          print *, "Delete row:", selections(1)
          call hl_gtk_list1_rem(ihlist, selections(1))
       end if
    end if

  end subroutine list_select

  subroutine text_cr(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata

    integer, pointer :: fdata

    if (c_associated(gdata)) then
       call c_f_pointer(gdata, fdata)
       fdata = 1
    end if
  end subroutine text_cr

  subroutine b_click(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata

    integer, pointer :: fdata
    type(c_ptr) :: buffer
    integer(c_int16_t) :: ntext
    character(kind=c_char, len=100) :: ftext

    if (c_associated(gdata)) then
       call c_f_pointer(gdata, fdata)
       if (fdata == 1) then
          ntext = gtk_entry_get_text_length(newline)
          buffer = gtk_entry_get_buffer(newline)
          call c_f_string_copy(gtk_entry_buffer_get_text(buffer), ftext)

          print *, len_trim(ftext), "*",trim(ftext),"*"
          call hl_gtk_list1_ins(ihlist, trim(ftext)//c_null_char)
          fdata = 0

          buffer = gtk_entry_get_buffer(newline)
          ! number of caracters = -1 for automatic length:
          call gtk_entry_buffer_set_text (buffer, ""//c_null_char, -1)
       end if
    end if
  end subroutine b_click

  subroutine del_toggle(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata

    integer, pointer :: fdata

    if (c_associated(gdata)) then
       call c_f_pointer(gdata, fdata)
       fdata = gtk_check_button_get_active(widget)
    end if
  end subroutine del_toggle

  subroutine delete_all(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata

    call hl_gtk_list1_rem(ihlist)
  end subroutine delete_all

  subroutine swap_rows(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata

    integer(kind=c_int) :: nsel, nrows
    integer(kind=c_int), dimension(:), allocatable :: selections
    integer :: i

    nsel = hl_gtk_list1_get_selections(ihlist, selections)
    nrows = hl_gtk_list1_get_n_rows(ihlist)

    select case(nsel)
    case(0) ! No selected rows (roll the list)
       allocate(selections(nrows))
       selections = [ (i-1, i=1, nrows) ]
       selections = cshift(selections, 1)
       call hl_gtk_list1_reorder(ihlist, selections)
    case(1) ! One row selected (move it up one unless it's the first)
       if (selections(1) == 0) then
          call hl_gtk_list1_move_row(ihlist, selections(1), 1_c_int, after=TRUE)
       else
          call hl_gtk_list1_move_row(ihlist, selections(1), &
               & selections(1)-1_c_int)
       end if
    case default ! Multiple selections (swap first 2 selected rows)
       call hl_gtk_list1_swap_rows(ihlist, selections(1), selections(2))
    end select

    deallocate(selections)
  end subroutine swap_rows

  subroutine activate(app, gdata) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char
    use gtk, only: gtk_application_window_new, gtk_window_set_title
    implicit none
    type(c_ptr), value, intent(in)  :: app, gdata
    character(len=35) :: line
    integer :: i, ltr
    integer, target :: iappend=0, idel=0

    ! Create the window:
    ihwin = gtk_application_window_new(app)
    call gtk_window_set_title(ihwin, "List demo"//c_null_char)

    ! Now make a column box & put it into the window
    base = hl_gtk_box_new()
    call gtk_window_set_child(ihwin, base)

    ! Now make a single column list with multiple selections enabled
    ihlist = hl_gtk_list1_new(ihscrollcontain, changed=c_funloc(list_select),&
         & data=c_loc(idel), multiple=TRUE, height=400_c_int, &
         & title="My list"//c_null_char)

    ! Now put 10 rows into it
    do i=1,10
       write(line,"('List entry number ',I0)") i
       ltr=len_trim(line)+1
       line(ltr:ltr)=c_null_char
       print *, line
       call hl_gtk_list1_ins(ihlist, line)
    end do

    ! It is the scrollcontainer that is placed into the box.
    call hl_gtk_box_pack(base, ihscrollcontain)

    ! Make row box put it in the column box and put an editable
    ! 1-line text widget and a button in it
    jbox = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jbox)

    newline = hl_gtk_entry_new(len=35_c_int, editable=TRUE, &
         & activate=c_funloc(text_cr), data=c_loc(iappend), &
         & tooltip="Enter some text followed by <CR>"//c_new_line//&
         &"then click 'Append' to add it to the list"//c_null_char)
    call hl_gtk_box_pack(jbox, newline)
    abut = hl_gtk_button_new("Append"//c_null_char, clicked=c_funloc(b_click),&
         & data=c_loc(iappend))
    call hl_gtk_box_pack(jbox, abut)

    ! Make a row box and put it in the main box
    jbox2 = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jbox2)
    ! Make a checkbox button and put it in the row box
    dbut = hl_gtk_check_button_new("Delete line"//c_null_char,&
         & toggled=c_funloc(del_toggle), initial_state=FALSE, &
         & data=c_loc(idel), &
         & tooltip="Set this then click on a line to delete it"//c_null_char)
    call hl_gtk_box_pack(jbox2, dbut)

    ! And a delete all button.
    dabut = hl_gtk_button_new("Clear"//c_null_char, clicked=c_funloc(delete_all))
    call hl_gtk_box_pack(jbox2, dabut)

    ! And a swap rows button
    swbut = hl_gtk_button_new("Swap rows"//c_null_char, clicked=c_funloc(swap_rows))
    call hl_gtk_box_pack(jbox2, swbut)

    ! Also a quit button
    qbut = hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(my_destroy))
    call hl_gtk_box_pack(base,qbut)

    ! realize the window
    call gtk_widget_show(ihwin)
  end subroutine activate
end module l1_handlers


program list1
  ! LIST1
  ! Demo of single column list
  use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char
  use l1_handlers

  implicit none
  type(c_ptr)        :: app

  app = hl_gtk_application_new("gtk-fortran.examples.hl_list1"//c_null_char, &
                             & c_funloc(activate))
end program list1

