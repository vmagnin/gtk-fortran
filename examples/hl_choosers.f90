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
!------------------------------------------------------------------------------
! Contributed by James Tappin.
! Last modifications: vmagnin 2020-06-18 (GTK 4 version), 2020-12-17
! Demo of file choosers.
!------------------------------------------------------------------------------

module handlers
!  use gtk_hl
  use gtk_hl_container
  use gtk_hl_button
  use gtk_hl_dialog
  use gtk_hl_chooser
  use gtk_hl_misc
  use gtk_hl_entry
  use gtk, only: gtk_button_new, gtk_window_set_child, &
       & gtk_text_view_new, gtk_widget_set_sensitive, gtk_widget_show, &
       & gtk_window_destroy, &
       & TRUE, FALSE, GTK_BUTTONS_YES_NO, GTK_RESPONSE_NO
  use g, only: alloca, g_file_get_path, g_object_unref

  implicit none
  ! Those widgets that need to be addressed explicitly in the handlers
  type(c_ptr) :: window, sbut, sabut, tedit
  ! Other variables that need to be shared between handlers
  logical, private :: file_is_changed = .FALSE.
  character(len=120), private :: filename=''

contains
  subroutine my_destroy(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata
    integer(c_int) :: ok
    character(len=60), dimension(4) :: msg

    msg(1) = "File is changed"
    msg(2) = ""
    msg(3) = "Quitting now will destroy your changes"
    msg(4) = "Do you really want to quit"

    if (file_is_changed) then
       ok = hl_gtk_message_dialog_show(msg, GTK_BUTTONS_YES_NO, &
            & "Really Quit"//c_null_char, parent=window)
       if (ok == GTK_RESPONSE_NO) return
    end if

    print *, "Exit called"
    call gtk_window_destroy(window)
  end subroutine my_destroy

  subroutine open_file(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata

    integer(c_int) :: isel
    character(len=120), dimension(:), allocatable :: chfile
    character(len=30), dimension(2) :: filters
    character(len=30), dimension(2) :: filtnames
    character(len=200) :: inln
    integer :: ios
    integer :: idxs

    filters(1) = "*.txt,*.lis"
    filters(2) = "*.f90"
    filtnames(1) = "Text files"
    filtnames(2) = "Fortran code"

    isel = hl_gtk_file_chooser_show(chfile, create=FALSE,&
         & title="Select input file"//c_null_char, filter=filters, &
         & filter_name=filtnames, wsize=[ 600_c_int, 400_c_int ], &
         & edit_filters=TRUE, &
         & parent=window, all=TRUE)
    print *, "isel = hl_gtk_file_chooser_show=", isel
    if (isel == FALSE) return   ! No selection made

    filename = chfile(1)
    deallocate(chfile)

    open(37, file=filename, action='read')
    call hl_gtk_text_view_delete(tedit)
    do
       read(37,"(A)",iostat=ios) inln
       if (ios /= 0) exit
       call hl_gtk_text_view_insert(tedit, [ trim(inln)//c_new_line ])
    end do
    close(37)
    idxs = index(filename, '/', .true.)+1
    call gtk_window_set_title(window, trim(filename(idxs:))//c_null_char)

    ! We manually reset the changed flag as the text box signal handler sets it.

    file_is_changed = .FALSE.
    call gtk_widget_set_sensitive(sabut, TRUE)
    call gtk_widget_set_sensitive(sbut, FALSE)
  end subroutine open_file


  subroutine save_file(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata

    character(len=200), dimension(:), allocatable :: text
    integer :: i

    call hl_gtk_text_view_get_text(tedit, text)

    open(37, file=filename, action='write')
    do i = 1, size(text)
       write(37, '(A)') trim(text(i))
    end do
    close(37)
    deallocate(text)

    file_is_changed = .FALSE.
    call gtk_widget_set_sensitive(widget, false)
  end subroutine save_file

  subroutine save_file_as(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata

    integer(c_int) :: isel
    character(len=120), dimension(:), allocatable :: chfile
    character(len=20), dimension(2) :: filters
    character(len=30), dimension(2) :: filtnames
    integer :: i
    character(len=200), dimension(:), allocatable :: text
    integer :: idxs

    filters(1) = "*.txt,*.lis"
    filters(2) = "*.f90"
    filtnames(1) = "Text files"
    filtnames(2) = "Fortran code"

    isel = hl_gtk_file_chooser_show(chfile, create=TRUE,&
         & title="Select input file"//c_null_char, filter=filters, &
         & filter_name=filtnames, initial_file=trim(filename)//c_null_char, &
         & confirm_overwrite=TRUE, all=TRUE, parent=window)

    if (isel == FALSE) return   ! No selection made

    filename = chfile(1)
    deallocate(chfile)
    idxs = index(filename, '/', .true.)+1
    call gtk_window_set_title(window, trim(filename(idxs:))//c_null_char)
    call hl_gtk_text_view_get_text(tedit, text)

    open(37, file=filename, action='write')
    do i = 1, size(text)
       write(37, '(A)') trim(text(i))
    end do
    close(37)
    deallocate(text)

    file_is_changed = .FALSE.

    call gtk_widget_set_sensitive(sbut, false)
  end subroutine save_file_as

  subroutine file_edited(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata

    file_is_changed = .true.
    if (filename == '') then
       call gtk_widget_set_sensitive(sabut, TRUE)
    else
       call gtk_widget_set_sensitive(sbut, TRUE)
    end if
  end subroutine file_edited


  subroutine activate(app, gdata) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char
    use gtk, only: gtk_application_window_new, gtk_window_set_title
    implicit none
    type(c_ptr), value, intent(in)  :: app, gdata
    ! Widgets that don't need to be global
    type(c_ptr) :: base, jb, junk

    ! Create a window and a column box
    window = gtk_application_window_new(app)
    call gtk_window_set_title(window, "Choosers demo"//c_null_char)

    base = hl_gtk_box_new()
    call gtk_window_set_child(window, base)

    ! A row of buttons
    jb = hl_gtk_box_new(horizontal=TRUE, homogeneous=TRUE)
    call hl_gtk_box_pack(base, jb)
    junk = hl_gtk_button_new("Open"//c_null_char, clicked=c_funloc(open_file))
    call hl_gtk_box_pack(jb, junk)
    sbut = hl_gtk_button_new("Save"//c_null_char, clicked=c_funloc(save_file),&
         & sensitive=FALSE)
    call hl_gtk_box_pack(jb, sbut)
    sabut = hl_gtk_button_new("Save as"//c_null_char, clicked=c_funloc(save_file_as), &
         & sensitive=FALSE)
    call hl_gtk_box_pack(jb, sabut)

    ! A multiline text editor in which to display the file.
    tedit = hl_gtk_text_view_new(jb, editable=TRUE, &
         & changed=c_funloc(file_edited), ssize = [ 750_c_int, 400_c_int ] )
    call hl_gtk_box_pack(base, jb)

    ! A quit button
    junk = hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(my_destroy))
    call hl_gtk_box_pack(base, junk)

    ! Realise & enter event loop
    call gtk_widget_show(window)
  end subroutine activate
end module handlers


program choosers_demo
  use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char
  use handlers

  implicit none
  type(c_ptr)        :: app

  app = hl_gtk_application_new("gtk-fortran.examples.hl_choosers"//c_null_char, &
                             & c_funloc(activate))
end program choosers_demo

